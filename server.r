function(input, output, session) {
  
  # 使用 reactive 表達式來處理資料，避免重複計算
  processed_data <- reactive({
    # 讀取資料
    pop_data <- tryCatch({
      data <- read_population_data(input$city, input$year)
      if (is.null(data) || nrow(data) == 0) return(NULL)
      
      # 確保日期格式正確
      data$date <- tryCatch({
        as.Date(paste0(data$date, "-01"), format = "%Y-%m-%d")
      }, error = function(e) {
        # 如果轉換失敗，嘗試其他格式
        tryCatch({
          as.Date(paste0(data$date, "/01"), format = "%Y/%m/%d")
        }, error = function(e) {
          NULL
        })
      })
      
      # 確保人口數為數值
      data$population <- as.numeric(gsub(",", "", as.character(data$population)))
      data
    }, error = function(e) {
      NULL
    })

    price_data <- tryCatch({
      data <- read_house_price_data(input$city, input$year)
      if (is.null(data) || nrow(data) == 0) return(NULL)
      
      # 確保日期格式正確
      data$date <- tryCatch({
        as.Date(paste0(data$date, "-01"), format = "%Y-%m-%d")
      }, error = function(e) {
        tryCatch({
          as.Date(paste0(data$date, "/01"), format = "%Y/%m/%d")
        }, error = function(e) {
          NULL
        })
      })
      
      # 確保房價為數值
      data$price_per_sqm <- as.numeric(gsub(",", "", as.character(data$price_per_sqm)))
      data
    }, error = function(e) {
      NULL
    })

    # 如果任一資料集為空，返回NULL
    if (is.null(pop_data) || is.null(price_data)) return(NULL)
    
    # 合併資料
    merged_data <- pop_data %>%
      left_join(price_data, by = "date") %>%
      na.omit()
    
    list(
      pop_data = pop_data,
      price_data = price_data,
      merged_data = merged_data
    )
  })
  
  # 繪製人口趨勢圖
  output$population_plot <- renderPlot({
    data <- processed_data()
    if (is.null(data) || is.null(data$pop_data)) {
      return(NULL)
    }
    
    ggplot(data$pop_data, aes(x = date, y = population)) +
      geom_line(color = "#0072B2", linewidth = 1) +
      geom_point(color = "#0072B2", size = 2) +
      labs(title = paste0(input$city, " ", input$year, "年人口趨勢"),
           x = "月份",
           y = "人口數") +
      scale_x_date(date_labels = "%b", date_breaks = "1 month") +
      theme_minimal(base_size = 16)
  })
  
  # 繪製房價趨勢圖
  output$price_plot <- renderPlot({
    data <- processed_data()
    if (is.null(data) || is.null(data$price_data)) {
      return(NULL)
    }
    
    ggplot(data$price_data, aes(x = date, y = price_per_sqm)) +
      geom_line(color = "#D55E00", linewidth = 1) +
      geom_point(color = "#D55E00", size = 2) +
      labs(title = paste0(input$city, " ", input$year, "年房價趨勢"),
           x = "月份",
           y = "每平方公尺單價") +
      scale_x_date(date_labels = "%b", date_breaks = "1 month") +
      theme_minimal(base_size = 16)
  })
  
  # 繪製相關性分析圖
  output$correlation_plot <- renderPlot({
    data <- processed_data()
    if (is.null(data) || is.null(data$merged_data) || nrow(data$merged_data) == 0) {
      return(NULL)
    }
    
    merged_data <- data$merged_data
    corr <- cor(merged_data$population, merged_data$price_per_sqm, use = "complete.obs")
    
    ggplot(merged_data, aes(x = population, y = price_per_sqm)) +
      geom_point(color = "#009E73", size = 2, alpha = 0.7) +
      geom_smooth(method = "lm", se = TRUE, color = "#E69F00", linetype = "dashed") +
      labs(title = paste0("人口數與房價相關性分析 (r = ", round(corr, 3), ")"),
           x = "人口數",
           y = "每平方公尺單價") +
      theme_minimal(base_size = 16)
  })
  
  # 顯示相關係數
  output$correlation_text <- renderText({
    data <- processed_data()
    if (is.null(data) || is.null(data$merged_data) || nrow(data$merged_data) == 0) {
      return("無法計算相關係數：資料不足")
    }
    
    correlation <- cor(data$merged_data$population, 
                      data$merged_data$price_per_sqm, 
                      use = "complete.obs")
    paste0("相關係數：", round(correlation, 4))
  })
}
