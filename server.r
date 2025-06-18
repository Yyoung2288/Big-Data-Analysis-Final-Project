library(shiny)
library(ggplot2)
library(dplyr)
library(zoo)
library(scales)
library(tidyr)
library(plotly)

# 載入全局函數
source("global.R")

# 定義伺服器邏輯
server <- function(input, output, session) {
  # 儲存各頁面的區域列表
  districts_cache <- reactiveValues(
    population = NULL,
    price = NULL,
    correlation = NULL,
    dualaxis = NULL
  )

  # 更新人口趨勢頁面的區域選項
  observe({
    # 只在城市改變時更新區域列表
    if (is.null(districts_cache$population) || districts_cache$population$city != input$city) {
      districts <- get_districts(input$city, 113)  # 使用任意年份都可以
      districts <- districts[districts != "全部" & districts != ""]
      districts <- districts[districts != input$city]  # 過濾掉和縣市同名的區域
      districts <- c("全部", "總計", districts)  # 加回特殊選項
      
      # 更新快取
      districts_cache$population <- list(
        city = input$city,
        districts = districts
      )
      
      # 更新選單
      updateSelectInput(session, "district", choices = districts, selected = districts[1])
    }
  }) %>% bindEvent(input$city)  # 只在城市改變時觸發
  
  # 更新房價趨勢頁面的區域選項
  observe({
    # 只在城市改變時更新區域列表
    if (is.null(districts_cache$price) || districts_cache$price$city != input$price_city) {
      districts <- get_districts(input$price_city, 113)  # 使用任意年份都可以
      districts <- districts[districts != "全部" & districts != ""]
      districts <- districts[districts != input$price_city]  # 過濾掉和縣市同名的區域
      districts <- c("全部", districts)  # 加回"全部"選項
      
      # 更新快取
      districts_cache$price <- list(
        city = input$price_city,
        districts = districts
      )
      
      # 更新選單
      updateSelectInput(session, "price_district", choices = districts, selected = districts[1])
    }
  }) %>% bindEvent(input$price_city)  # 只在城市改變時觸發
  
  # 更新相關性分析頁面的區域選項
  observe({
    # 只在城市改變時更新區域列表
    if (is.null(districts_cache$correlation) || districts_cache$correlation$city != input$corr_city) {
      districts <- get_districts(input$corr_city, 113)  # 使用任意年份都可以
      districts <- districts[districts != "全部" & districts != ""]
      districts <- districts[districts != input$corr_city]  # 過濾掉和縣市同名的區域
      
      # 更新快取
      districts_cache$correlation <- list(
        city = input$corr_city,
        districts = districts
      )
      
      # 更新選單
      updateSelectInput(session, "corr_district", choices = districts, selected = districts[1])
    }
  }) %>% bindEvent(input$corr_city)  # 只在城市改變時觸發
  
  # 更新雙軸圖頁面的區域選項（不含「全部」）
  observe({
    # 只在城市改變時更新區域列表
    if (is.null(districts_cache$dualaxis) || districts_cache$dualaxis$city != input$dual_city) {
      districts <- get_districts(input$dual_city, 113)  # 使用任意年份都可以
      districts <- districts[districts != "全部" & districts != "總計" & districts != ""]
      districts <- districts[districts != input$dual_city]  # 過濾掉和縣市同名的區域
      
      # 更新快取
      districts_cache$dualaxis <- list(
        city = input$dual_city,
        districts = districts
      )
      
      # 更新選單
      updateSelectInput(session, "dual_district", choices = districts, selected = districts[1])
    }
  }) %>% bindEvent(input$dual_city)  # 只在城市改變時觸發
  
  # 儲存當前圖表數據和標題
  population_data <- reactiveValues(
    data = NULL,
    city = NULL,
    year = NULL
  )
  
  price_data <- reactiveValues(
    data = NULL,
    city = NULL,
    year = NULL
  )
  
  correlation_data <- reactiveValues(
    data = NULL,
    city = NULL
  )
  
  # 雙軸圖資料儲存
  dualaxis_data <- reactiveValues(
    data = NULL,
    city = NULL,
    year = NULL
  )
  
  # 人口趨勢分析
  observeEvent(input$analyze, {
    if(input$year == "全部") {
      # 讀取所有年份的資料，顯示每個月的數據
      all_data <- lapply(104:113, function(year) {
        if(input$district == "總計") {
          # 讀取所有區域的資料並計算總和
          all_districts_data <- read_population_data(input$city, year, "全部")
          if(!is.null(all_districts_data)) {
            all_districts_data <- all_districts_data[all_districts_data$district != "", ]
            total_data <- all_districts_data %>%
              group_by(date) %>%
              summarise(population = sum(population, na.rm = TRUE)) %>%
              mutate(district = "總計")
            total_data$year <- year
            total_data$month <- as.numeric(substr(total_data$date, 4, 5))
            total_data$time_sequence <- (year - 104) * 12 + total_data$month
            total_data
          }
        } else {
          data <- read_population_data(input$city, year, input$district)
          if(!is.null(data)) {
            data <- data[data$district != "", ]
            data$year <- year
            data$month <- as.numeric(substr(data$date, 4, 5))
            data$time_sequence <- (year - 104) * 12 + data$month
            data
          }
        }
      })
      
      # 移除NULL元素並合併
      all_data <- all_data[!sapply(all_data, is.null)]
      pop_data <- if(length(all_data) > 0) do.call(rbind, all_data) else NULL
      
    } else {
      if(input$district == "總計") {
        # 讀取所有區域的資料並計算總和
        all_districts_data <- read_population_data(input$city, input$year, "全部")
        if(!is.null(all_districts_data)) {
          pop_data <- all_districts_data %>%
            group_by(date) %>%
            summarise(population = sum(population, na.rm = TRUE)) %>%
            mutate(district = "總計",
                   month = as.numeric(substr(date, 4, 5)))
        } else {
          pop_data <- NULL
        }
      } else {
        pop_data <- read_population_data(input$city, input$year, input$district)
        if(!is.null(pop_data)) {
          pop_data$month <- as.numeric(substr(pop_data$date, 4, 5))
        }
      }
    }
    
    population_data$data <- pop_data
    population_data$city <- input$city
    population_data$year <- input$year
  })
    
  # 人口趨勢圖表輸出
  output$population_plot <- renderPlotly({
    if (!is.null(population_data$data) && nrow(population_data$data) > 0) {
      if(population_data$year == "全部") {
        year_breaks <- seq(1, 120, by = 12)
        year_labels <- 104:113
        p <- ggplot(population_data$data, aes(x = time_sequence, y = population, 
                                           color = district,
                                           group = district,
                                           text = paste(
                                             "年份：", year,
                                             "<br>行政區：", district,
                                             "<br>人口數：", scales::comma(population), "人"
                                           ))) +
          geom_line(linewidth = 1) +
          geom_point(size = 1.5, alpha = 0.7) +
          scale_x_continuous(
            breaks = year_breaks,
            labels = year_labels,
            minor_breaks = seq(1, 120, by = 3)
          ) +
          labs(title = paste(population_data$city, "歷年人口趨勢（月度數據）"),
               x = "年份",
               y = "人口數",
               color = "鄉鎮市區") +
          scale_y_continuous(labels = scales::comma) +
          theme_minimal() +
          theme(
            legend.position = "right",
            plot.title = element_text(size = 16, face = "bold"),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 10),
            legend.title = element_text(size = 12),
            legend.text = element_text(size = 10),
            panel.grid.minor.x = element_line(linewidth = 0.3, color = "grey90")
          )
        ggplotly(p, tooltip = "text")
      } else {
        p <- ggplot(population_data$data, aes(x = month, y = population, 
                                           color = district,
                                           group = district,
                                           text = paste(
                                             "月份：", month,
                                             "<br>行政區：", district,
                                             "<br>人口數：", scales::comma(population), "人"
                                           ))) +
          geom_line(linewidth = 1) +
          geom_point(size = 1.5, alpha = 0.7) +
          scale_x_continuous(breaks = 1:12) +
          labs(title = paste(population_data$city, population_data$year, "年人口趨勢"),
               x = "月份",
               y = "人口數",
               color = "鄉鎮市區") +
          scale_y_continuous(labels = scales::comma) +
          theme_minimal() +
          theme(
            legend.position = "right",
            plot.title = element_text(size = 16, face = "bold"),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 10),
            legend.title = element_text(size = 12),
            legend.text = element_text(size = 10),
            panel.grid.minor.x = element_line(linewidth = 0.3, color = "grey90")
          )
        ggplotly(p, tooltip = "text")
      }
    }
    })
    
  # 房價趨勢分析
  observeEvent(input$analyze_price, {
    if(input$price_year == "全部") {
      # 讀取所有年份的資料
      all_data <- lapply(104:113, function(year) {
        data <- read_house_price_data(input$price_city, year, input$price_district)
        if(!is.null(data)) {
          data <- data[data$鄉鎮市區_The.villages.and.towns.urban.district != "", ]
          data$year <- year
          data
        }
      })
      
      # 合併所有年份的資料
      raw_data <- do.call(rbind, all_data)
      
      # 計算每年的平均房價
      price_data$data <- raw_data %>%
        group_by(year, 鄉鎮市區_The.villages.and.towns.urban.district) %>%
        summarise(price_per_sqm = mean(price_per_sqm, na.rm = TRUE), .groups = "drop") %>%
        arrange(year, 鄉鎮市區_The.villages.and.towns.urban.district)
      
      price_data$data$date <- price_data$data$year
    } else {
      price_data$data <- read_house_price_data(input$price_city, input$price_year, input$price_district)
      if(!is.null(price_data$data)) {
        price_data$data <- price_data$data[price_data$data$鄉鎮市區_The.villages.and.towns.urban.district != "", ]
        price_data$data$month <- as.numeric(substr(price_data$data$date, 4, 5))
      }
    }
    
    price_data$city <- input$price_city
    price_data$year <- input$price_year
  })
  
  # 房價趨勢圖表輸出
  output$price_plot <- renderPlotly({
    if (!is.null(price_data$data) && nrow(price_data$data) > 0) {
      if(price_data$year == "全部") {
        p <- ggplot(price_data$data, aes(x = date, y = price_per_sqm, 
                                    color = 鄉鎮市區_The.villages.and.towns.urban.district,
                                    group = 鄉鎮市區_The.villages.and.towns.urban.district,
                                    text = paste(
                                      "年份：", date,
                                      "<br>行政區：", 鄉鎮市區_The.villages.and.towns.urban.district,
                                      "<br>每平方公尺單價：", scales::comma(price_per_sqm), "元"
                                    ))) +
          geom_line(linewidth = 1) +
          geom_point(size = 1.5, alpha = 0.7) +
          scale_x_continuous(breaks = 104:113) +
          labs(title = paste(price_data$city, "歷年房價趨勢"),
               x = "年份",
               y = "每平方公尺單價（年均價）",
               color = "鄉鎮市區") +
          scale_y_continuous(labels = scales::comma) +
          theme_minimal() +
          theme(
            legend.position = "right",
            plot.title = element_text(size = 16, face = "bold"),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 10),
            legend.title = element_text(size = 12),
            legend.text = element_text(size = 10),
            panel.grid.minor.x = element_line(linewidth = 0.3, color = "grey90")
          )
        ggplotly(p, tooltip = "text")
      } else {
        p <- ggplot(price_data$data, aes(x = month, y = price_per_sqm, 
                                    color = 鄉鎮市區_The.villages.and.towns.urban.district,
                                    group = 鄉鎮市區_The.villages.and.towns.urban.district,
                                    text = paste(
                                      "月份：", month,
                                      "<br>行政區：", 鄉鎮市區_The.villages.and.towns.urban.district,
                                      "<br>每平方公尺單價：", scales::comma(price_per_sqm), "元"
                                    ))) +
          geom_line(linewidth = 1) +
          geom_point(size = 1.5, alpha = 0.7) +
          scale_x_continuous(breaks = 1:12) +
          labs(title = paste(price_data$city, price_data$year, "年房價趨勢"),
               x = "月份",
               y = "每平方公尺單價",
               color = "鄉鎮市區") +
          scale_y_continuous(labels = scales::comma) +
          theme_minimal() +
          theme(
            legend.position = "right",
            plot.title = element_text(size = 16, face = "bold"),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 10),
            legend.title = element_text(size = 12),
            legend.text = element_text(size = 10),
            panel.grid.minor.x = element_line(linewidth = 0.3, color = "grey90")
          )
        ggplotly(p, tooltip = "text")
      }
    }
    })
    
  # 相關性分析
  observeEvent(input$analyze_corr, {
    if(input$corr_year == "全部") {
      # 讀取所有年份的資料
      all_pop_data <- lapply(104:113, function(year) {
        data <- read_population_data(input$corr_city, year, input$corr_district)
        if(!is.null(data)) {
          data <- data[data$district != "", ]
          data$year <- year
          data$month <- as.numeric(substr(data$date, 4, 5))
          data
        }
      })
      pop_data <- do.call(rbind, all_pop_data)
      
      all_price_data <- lapply(104:113, function(year) {
        data <- read_house_price_data(input$corr_city, year, input$corr_district)
        if(!is.null(data)) {
          data <- data[data$鄉鎮市區_The.villages.and.towns.urban.district != "", ]
          data$year <- year
          data
        }
      })
      price_data_corr <- do.call(rbind, all_price_data)
    } else {
      pop_data <- read_population_data(input$corr_city, input$corr_year, input$corr_district)
      if(!is.null(pop_data)) {
        pop_data <- pop_data[pop_data$district != "", ]
      }
      
      price_data_corr <- read_house_price_data(input$corr_city, input$corr_year, input$corr_district)
      if(!is.null(price_data_corr)) {
        price_data_corr <- price_data_corr[price_data_corr$鄉鎮市區_The.villages.and.towns.urban.district != "", ]
      }
    }
    
    if (!is.null(pop_data) && !is.null(price_data_corr) && 
        nrow(pop_data) > 0 && nrow(price_data_corr) > 0) {
      
      # 合併資料
      if(input$corr_year == "全部") {
        merged_data <- merge(pop_data, price_data_corr, 
                             by.x = c("year", "district"),
                             by.y = c("year", "鄉鎮市區_The.villages.and.towns.urban.district"),
                             all = FALSE)
      } else {
        merged_data <- merge(pop_data, price_data_corr, 
                             by.x = c("date", "district"),
                             by.y = c("date", "鄉鎮市區_The.villages.and.towns.urban.district"),
                             all = FALSE)
      }
      
      correlation_data$data <- merged_data
      correlation_data$city <- input$corr_city
      correlation_data$year <- input$corr_year
      
      if (nrow(merged_data) > 0) {
        # 計算相關係數
        correlation_data$correlation <- cor(merged_data$population, merged_data$price_per_sqm, use = "complete.obs")
      }
    }
  })
  
  # 相關性分析圖表輸出
  output$correlation_plot <- renderPlotly({
    if (!is.null(correlation_data$data) && nrow(correlation_data$data) > 0) {
      # 過濾掉 NA 和無限值
      valid_data <- correlation_data$data[
        !is.na(correlation_data$data$population) & 
        !is.na(correlation_data$data$price_per_sqm) &
        !is.infinite(correlation_data$data$population) &
        !is.infinite(correlation_data$data$price_per_sqm),
      ]
      
      p <- ggplot(valid_data, aes(x = population, y = price_per_sqm)) +
        geom_point(aes(color = district), 
                  size = 1.5, alpha = 0.7) +
        stat_smooth(method = "lm", se = TRUE, 
                   aes(group = 1),
                   geom = "ribbon",
                   fill = alpha("grey70", 0.2),
                   color = NA) +
        stat_smooth(method = "lm",
                   aes(group = 1),
                   geom = "line",
                   color = alpha("black", 0.5),
                   linewidth = 1) +
        labs(title = paste(correlation_data$city, 
                         if(correlation_data$year == "全部") "歷年" else paste(correlation_data$year, "年"),
                         "人口與房價相關性分析"),
             x = "人口數",
             y = "每平方公尺單價",
             color = "鄉鎮市區") +
        scale_y_continuous(labels = scales::comma) +
        scale_x_continuous(labels = scales::comma) +
        theme_minimal() +
        theme(
          legend.position = "right",
          plot.title = element_text(size = 16, face = "bold"),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10),
          panel.grid.minor.x = element_line(linewidth = 0.3, color = "grey90")
        )
      
      # 創建懸浮提示文字
      tooltip_text <- paste(
        "行政區：", valid_data$district,
        "<br>人口數：", scales::comma(valid_data$population), "人",
        "<br>每平方公尺單價：", scales::comma(valid_data$price_per_sqm), "元"
      )
      
      ggplotly(p, tooltip = "all") %>%
        style(text = tooltip_text)
    }
    })
    
  # 顯示相關性文字說明
    output$correlation_text <- renderText({
    if (!is.null(correlation_data$correlation)) {
      corr <- correlation_data$correlation
      paste(" 相關係數：", round(corr, 4), "\n",
            "相關係數解釋：",
            if(corr > 0.7) "強正相關" 
            else if(corr > 0.3) "中等正相關" 
            else if(corr > 0) "弱正相關"
            else if(corr < -0.7) "強負相關" 
            else if(corr < -0.3) "中等負相關" 
            else if(corr < 0) "弱負相關"
            else "無相關")
    }
  })
  
  # 雙軸圖分析
  observeEvent(input$analyze_dual, {
    # 讀取人口資料
    pop_data <- NULL
    price_data <- NULL
    if(input$dual_year == "全部") {
      # 讀取人口資料
      all_pop <- lapply(104:113, function(year) {
        d <- read_population_data(input$dual_city, year, input$dual_district)
        if(!is.null(d)) {
          d$year <- year
          d$month <- as.numeric(substr(d$date, 4, 5))
          d
        }
      })
      pop_data <- do.call(rbind, all_pop)

      # 讀取房價資料並計算年平均
      all_price <- lapply(104:113, function(year) {
        d <- read_house_price_data(input$dual_city, year, input$dual_district)
        if(!is.null(d)) {
          # 計算年平均房價
          d %>%
            group_by(鄉鎮市區_The.villages.and.towns.urban.district) %>%
            summarise(
              price_per_sqm = mean(price_per_sqm, na.rm = TRUE),
              year = year,
              district = 鄉鎮市區_The.villages.and.towns.urban.district
            )
        }
      })
      price_data <- do.call(rbind, all_price)
    } else {
      pop_data <- read_population_data(input$dual_city, input$dual_year, input$dual_district)
      if(!is.null(pop_data)) {
        pop_data$month <- as.numeric(substr(pop_data$date, 4, 5))
      }
      price_data <- read_house_price_data(input$dual_city, input$dual_year, input$dual_district)
      if(!is.null(price_data)) {
        price_data$month <- as.numeric(substr(price_data$date, 4, 5))
      }
    }

    # 合併資料
    if(!is.null(pop_data) && !is.null(price_data) && nrow(pop_data) > 0 && nrow(price_data) > 0) {
      if(input$dual_year == "全部") {
        # 統一欄位名稱
        pop_data$district_name <- pop_data$district
        price_data$district_name <- price_data$district
        # 先計算人口資料的年平均，再與房價合併
        pop_yearly <- pop_data %>%
          group_by(year, district_name) %>%
          summarise(
            population = mean(population, na.rm = TRUE)
          )
        merged <- merge(pop_yearly, price_data, 
                       by.x = c("year", "district_name"),
                       by.y = c("year", "district_name"),
                       all = FALSE)
        merged$x_axis <- merged$year
      } else {
        # 統一欄位名稱
        pop_data$district_name <- pop_data$district
        price_data$district_name <- price_data$鄉鎮市區_The.villages.and.towns.urban.district
        merged <- merge(pop_data, price_data, 
                       by.x = c("date", "district_name", "month"),
                       by.y = c("date", "district_name", "month"),
                       all = FALSE)
        merged$x_axis <- merged$month
      }
      dualaxis_data$data <- merged
      dualaxis_data$city <- input$dual_city
      dualaxis_data$year <- input$dual_year
      dualaxis_data$district <- merged$district_name[1]
    } else {
      dualaxis_data$data <- NULL
    }
  })

  # 雙軸圖輸出
  output$dualaxis_plot <- renderPlotly({
    d <- dualaxis_data$data
    if(!is.null(d) && nrow(d) > 0) {
      if(dualaxis_data$year == "全部") {
        plot_ly() %>%
          add_trace(
            data = d,
            x = ~year,
            y = ~population,
            name = "人口數",
            type = "scatter",
            mode = "lines+markers",
            line = list(color = "#1f77b4", width = 2),
            marker = list(color = "#1f77b4", size = 6, opacity = 0.7),
            text = ~paste(
              "年份：", year,
              "<br>行政區：", district_name,
              "<br>人口數：", scales::comma(population), "人"
            ),
            hoverinfo = "text"
          ) %>%
          add_trace(
            data = d,
            x = ~year,
            y = ~price_per_sqm,
            name = "每平方公尺單價",
            yaxis = "y2",
            type = "scatter",
            mode = "lines+markers",
            line = list(color = "#ff7f0e", width = 2),
            marker = list(color = "#ff7f0e", size = 6, opacity = 0.7),
            text = ~paste(
              "年份：", year,
              "<br>行政區：", district_name,
              "<br>每平方公尺單價：", scales::comma(price_per_sqm), "元"
            ),
            hoverinfo = "text"
          ) %>%
          layout(
            title = list(
              text = paste(dualaxis_data$city, dualaxis_data$district, "歷年人口與房價趨勢（年平均）"),
              font = list(size = 16, family = "Arial", weight = "bold")
            ),
            xaxis = list(
              title = list(text = "年份", font = list(size = 12)),
              ticktext = 104:113,
              tickvals = 104:113,
              tickmode = "array",
              showgrid = TRUE,
              gridcolor = "rgb(235, 235, 235)",
              tickfont = list(size = 10),
              zeroline = FALSE
            ),
            yaxis = list(
              title = list(text = "人口數", font = list(size = 12)),
              showgrid = TRUE,
              gridcolor = "rgb(235, 235, 235)",
              tickformat = ",",
              tickfont = list(size = 10),
              zeroline = FALSE
            ),
            yaxis2 = list(
              title = list(text = "每平方公尺單價", font = list(size = 12)),
              overlaying = "y",
              side = "right",
              showgrid = FALSE,
              tickformat = ",",
              tickfont = list(size = 10),
              zeroline = FALSE
            ),
            legend = list(
              x = 1.1,
              y = 1,
              font = list(size = 10)
            ),
            margin = list(
              l = 50,
              r = 80,
              t = 50,
              b = 50
            ),
            plot_bgcolor = "rgb(255, 255, 255)",
            paper_bgcolor = "rgb(255, 255, 255)",
            showlegend = TRUE,
            hovermode = "x unified"
          )
      } else {
        plot_ly() %>%
          add_trace(
            data = d,
            x = ~month,
            y = ~population,
            name = "人口數",
            type = "scatter",
            mode = "lines+markers",
            line = list(color = "#1f77b4", width = 2),
            marker = list(color = "#1f77b4", size = 6, opacity = 0.7),
            text = ~paste(
              "月份：", month,
              "<br>行政區：", district_name,
              "<br>人口數：", scales::comma(population), "人"
            ),
            hoverinfo = "text"
          ) %>%
          add_trace(
            data = d,
            x = ~month,
            y = ~price_per_sqm,
            name = "每平方公尺單價",
            yaxis = "y2",
            type = "scatter",
            mode = "lines+markers",
            line = list(color = "#ff7f0e", width = 2),
            marker = list(color = "#ff7f0e", size = 6, opacity = 0.7),
            text = ~paste(
              "月份：", month,
              "<br>行政區：", district_name,
              "<br>每平方公尺單價：", scales::comma(price_per_sqm), "元"
            ),
            hoverinfo = "text"
          ) %>%
          layout(
            title = list(
              text = paste(dualaxis_data$city, dualaxis_data$district, dualaxis_data$year, "年人口與房價趨勢"),
              font = list(size = 16, family = "Arial", weight = "bold")
            ),
            xaxis = list(
              title = list(text = "月份", font = list(size = 12)),
              ticktext = 1:12,
              tickvals = 1:12,
              tickmode = "array",
              showgrid = TRUE,
              gridcolor = "rgb(235, 235, 235)",
              tickfont = list(size = 10),
              zeroline = FALSE
            ),
            yaxis = list(
              title = list(text = "人口數", font = list(size = 12)),
              showgrid = TRUE,
              gridcolor = "rgb(235, 235, 235)",
              tickformat = ",",
              tickfont = list(size = 10),
              zeroline = FALSE
            ),
            yaxis2 = list(
              title = list(text = "每平方公尺單價", font = list(size = 12)),
              overlaying = "y",
              side = "right",
              showgrid = FALSE,
              tickformat = ",",
              tickfont = list(size = 10),
              zeroline = FALSE
            ),
            legend = list(
              x = 1.1,
              y = 1,
              font = list(size = 10)
            ),
            margin = list(
              l = 50,
              r = 80,
              t = 50,
              b = 50
            ),
            plot_bgcolor = "rgb(255, 255, 255)",
            paper_bgcolor = "rgb(255, 255, 255)",
            showlegend = TRUE,
            hovermode = "x unified"
          )
      }
    }
  })
}
