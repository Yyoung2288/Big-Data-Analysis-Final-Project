function(input, output, session) {
  
  # 當城市或年份改變時，更新區域選單
  observe({
    districts <- get_districts(input$city, input$year)
    updateSelectInput(session, "district",
                     choices = districts,
                     selected = "全部")
  })
  
  # 使用 reactive 表達式來處理資料，避免重複計算
  processed_data <- reactive({
    # 讀取資料
    pop_data <- tryCatch({
      data <- read_population_data(input$city, input$year, input$district)
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
      
      # 確保人口數為數值並處理NA
      data$population <- suppressWarnings(as.numeric(gsub(",", "", as.character(data$population))))
      # 使用線性插值填補NA值
      if(any(is.na(data$population))) {
        if ("district" %in% colnames(data)) {
          # 多區域資料，按區域分組處理
          data <- data %>%
            group_by(district) %>%
            arrange(date) %>%
            mutate(population = zoo::na.approx(population, na.rm = FALSE)) %>%
            ungroup()
        } else {
          # 單一資料
          data <- data[order(data$date),]
          data$population <- zoo::na.approx(data$population, na.rm = FALSE)
        }
      }
      data
    }, error = function(e) {
      NULL
    })

    price_data <- tryCatch({
      data <- read_house_price_data(input$city, input$year, input$district)
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
      
      # 確保房價為數值並處理NA
      data$price_per_sqm <- suppressWarnings(as.numeric(gsub(",", "", as.character(data$price_per_sqm))))
      # 使用線性插值填補NA值
      if(any(is.na(data$price_per_sqm))) {
        data <- data %>%
          group_by(鄉鎮市區_The.villages.and.towns.urban.district) %>%
          arrange(date) %>%
          mutate(price_per_sqm = zoo::na.approx(price_per_sqm, na.rm = FALSE)) %>%
          ungroup()
      }
      data
    }, error = function(e) {
      NULL
    })

    # 如果任一資料集為空，返回NULL
    if (is.null(pop_data) || is.null(price_data)) return(NULL)
    
    # 合併資料
    if ("district" %in% colnames(pop_data)) {
      # 多區域人口資料，需要與房價資料按區域匹配
      # 重新命名房價資料的區域欄位以便合併
      price_data$district <- price_data$鄉鎮市區_The.villages.and.towns.urban.district
      
      merged_data <- pop_data %>%
        inner_join(price_data, by = c("date", "district")) %>%
        arrange(date, district)
    } else {
      # 單一人口資料，按日期合併
      # 如果房價資料有多個區域，取平均值
      if ("鄉鎮市區_The.villages.and.towns.urban.district" %in% colnames(price_data)) {
        price_data_avg <- price_data %>%
          group_by(date) %>%
          summarise(price_per_sqm = mean(price_per_sqm, na.rm = TRUE), .groups = "drop")
      } else {
        price_data_avg <- price_data
      }
      
      merged_data <- pop_data %>%
        inner_join(price_data_avg, by = "date") %>%
        arrange(date)
    }
    
    # 移除仍然有NA的列
    merged_data <- merged_data[complete.cases(merged_data),]
    
    list(
      pop_data = pop_data,
      price_data = price_data,
      merged_data = merged_data
    )
  })
  
  # 繪製人口趨勢圖
  output$population_plot <- renderPlot({
    data <- processed_data()
    if (is.null(data) || is.null(data$pop_data) || nrow(data$pop_data) < 2) {
      return(NULL)
    }
    
    title <- paste0(input$city, " ", input$year, "年人口趨勢")
    
    # 檢查是否有區域資訊（多區域資料）
    if ("district" %in% colnames(data$pop_data)) {
      # 多區域資料，繪製多條線
      n_districts <- length(unique(data$pop_data$district))
      color_palette <- scales::hue_pal()(n_districts)
      
      # 如果只有一個區域，使用單一顏色
      if (n_districts == 1) {
        p <- ggplot(data$pop_data, aes(x = date, y = population)) +
          geom_line(color = "#0072B2", linewidth = 1) +
          geom_point(color = "#0072B2", size = 2)
      } else {
        p <- ggplot(data$pop_data, aes(x = date, y = population, color = district)) +
          geom_line(linewidth = 1) +
          geom_point(size = 2) +
          scale_color_manual(values = color_palette)
      }
      
      p + labs(title = title,
               x = "月份",
               y = "人口數",
               color = "區域") +
        scale_x_date(date_labels = "%b", date_breaks = "1 month") +
        theme_minimal(base_size = 16) +
        theme(legend.position = "right",
              legend.title = element_text(size = 12),
              legend.text = element_text(size = 10))
    } else {
      # 單一資料（向後相容）
      ggplot(data$pop_data, aes(x = date, y = population)) +
        geom_line(color = "#0072B2", linewidth = 1) +
        geom_point(color = "#0072B2", size = 2) +
        labs(title = title,
             x = "月份",
             y = "人口數") +
        scale_x_date(date_labels = "%b", date_breaks = "1 month") +
        theme_minimal(base_size = 16)
    }
  })
  
  # 繪製房價趨勢圖
  output$price_plot <- renderPlot({
    data <- processed_data()
    if (is.null(data) || is.null(data$price_data) || nrow(data$price_data) < 2) {
      return(NULL)
    }
    
    title <- paste0(input$city, " ", input$year, "年房價趨勢")
    
    # 設定顏色調色盤
    n_districts <- length(unique(data$price_data$鄉鎮市區_The.villages.and.towns.urban.district))
    color_palette <- scales::hue_pal()(n_districts)
    
    # 如果只有一個區域，使用單一顏色
    if (n_districts == 1) {
      p <- ggplot(data$price_data, aes(x = date, y = price_per_sqm)) +
        geom_line(color = "#D55E00", linewidth = 1) +
        geom_point(color = "#D55E00", size = 2)
    } else {
      p <- ggplot(data$price_data, aes(x = date, y = price_per_sqm, 
                                      color = 鄉鎮市區_The.villages.and.towns.urban.district)) +
        geom_line(linewidth = 1) +
        geom_point(size = 2) +
        scale_color_manual(values = color_palette)
    }
    
    p + labs(title = title,
             x = "月份",
             y = "每平方公尺單價",
             color = "區域") +
      scale_x_date(date_labels = "%b", date_breaks = "1 month") +
      theme_minimal(base_size = 16) +
      theme(legend.position = "right",
            legend.title = element_text(size = 12),
            legend.text = element_text(size = 10))
  })
  
  # 繪製相關性分析圖
  output$correlation_plot <- renderPlot({
    data <- processed_data()
    if (is.null(data) || is.null(data$merged_data) || nrow(data$merged_data) < 3) {
      return(NULL)
    }
    
    merged_data <- data$merged_data
    
    # 計算整體相關係數
    overall_corr <- cor(merged_data$population, merged_data$price_per_sqm, use = "complete.obs")
    
    title <- paste0(input$city, " 人口數與房價相關性分析 (r = ", round(overall_corr, 3), ")")
    
    # 檢查是否有區域資訊
    if ("district" %in% colnames(merged_data)) {
      # 多區域資料
      n_districts <- length(unique(merged_data$district))
      color_palette <- scales::hue_pal()(n_districts)
      
      # 如果只有一個區域，使用單一顏色
      if (n_districts == 1) {
        p <- ggplot(merged_data, aes(x = population, y = price_per_sqm)) +
          geom_point(color = "#009E73", size = 2, alpha = 0.7) +
          geom_smooth(method = "lm", se = TRUE, color = "#E69F00", linetype = "dashed")
      } else {
        p <- ggplot(merged_data, aes(x = population, y = price_per_sqm, color = district)) +
          geom_point(size = 2, alpha = 0.7) +
          geom_smooth(method = "lm", se = TRUE, linetype = "dashed") +
          scale_color_manual(values = color_palette)
      }
      
      p + labs(title = title,
               x = "人口數",
               y = "每平方公尺單價",
               color = "區域") +
        theme_minimal(base_size = 16) +
        theme(legend.position = "right",
              legend.title = element_text(size = 12),
              legend.text = element_text(size = 10))
    } else {
      # 單一資料（向後相容）
      ggplot(merged_data, aes(x = population, y = price_per_sqm)) +
        geom_point(color = "#009E73", size = 2, alpha = 0.7) +
        geom_smooth(method = "lm", se = TRUE, color = "#E69F00", linetype = "dashed") +
        labs(title = title,
             x = "人口數",
             y = "每平方公尺單價") +
        theme_minimal(base_size = 16)
    }
  })
  
  # 顯示相關係數
  output$correlation_text <- renderText({
    data <- processed_data()
    if (is.null(data) || is.null(data$merged_data) || nrow(data$merged_data) < 3) {
      return("無法計算相關係數：資料不足")
    }
    
    merged_data <- data$merged_data
    
    # 計算整體相關係數
    overall_correlation <- cor(merged_data$population, 
                              merged_data$price_per_sqm, 
                              use = "complete.obs")
    
    result_text <- paste0("整體相關係數：", round(overall_correlation, 4))
    
    # 如果是多區域資料，顯示各區域的相關係數
    if ("district" %in% colnames(merged_data)) {
      districts <- unique(merged_data$district)
      if (length(districts) > 1) {
        district_correlations <- sapply(districts, function(d) {
          district_data <- merged_data[merged_data$district == d, ]
          if (nrow(district_data) >= 3) {
            cor(district_data$population, district_data$price_per_sqm, use = "complete.obs")
          } else {
            NA
          }
        })
        
        district_text <- paste(
          paste0(names(district_correlations), ": ", round(district_correlations, 4)),
          collapse = "\n"
        )
        
        result_text <- paste0(result_text, "\n\n各區域相關係數：\n", district_text)
      }
    }
    
    return(result_text)
  })
}
