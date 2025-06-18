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
  # 更新人口趨勢頁面的區域選項
  observe({
    districts <- get_districts(input$city, if(input$year == "全部") 113 else input$year)
    districts <- districts[districts != "全部" & districts != ""]
    districts <- districts[districts != input$city]  # 過濾掉和縣市同名的區域
    districts <- c("全部", "總計", districts)  # 加回特殊選項
    current_district <- input$district
    selected <- if (!is.null(current_district) && current_district %in% districts) current_district else districts[1]
    updateSelectInput(session, "district", choices = districts, selected = selected)
  })
  
  # 更新房價趨勢頁面的區域選項
  observe({
    districts <- get_districts(input$price_city, if(input$price_year == "全部") 113 else input$price_year)
    districts <- districts[districts != "全部" & districts != ""]
    districts <- districts[districts != input$price_city]  # 過濾掉和縣市同名的區域
    districts <- c("全部", districts)  # 加回"全部"選項
    current_district <- input$price_district
    selected <- if (!is.null(current_district) && current_district %in% districts) current_district else districts[1]
    updateSelectInput(session, "price_district", choices = districts, selected = selected)
  })
  
  # 更新相關性分析頁面的區域選項
  observe({
    districts <- get_districts(input$corr_city, if(input$corr_year == "全部") 113 else input$corr_year)
    districts <- districts[districts != "全部" & districts != ""]
    districts <- districts[districts != input$corr_city]  # 過濾掉和縣市同名的區域
    current_district <- input$corr_district
    selected <- if (!is.null(current_district) && current_district %in% districts) current_district else districts[1]
    updateSelectInput(session, "corr_district", choices = districts, selected = selected)
  })
  
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
  
  # 人口趨勢分析
  observeEvent(input$analyze, {
    if(input$year == "全部") {
      # 讀取所有年份的資料，顯示每個月的數據
      all_data <- lapply(104:113, function(year) {
        if(input$district == "總計") {
          # 讀取所有區域的資料並計算總和
          all_districts_data <- read_population_data(input$city, year, "全部")
          if(!is.null(all_districts_data)) {
            # 過濾掉空白區域
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
            # 過濾掉空白區域
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
      if(length(all_data) > 0) {
        pop_data <- do.call(rbind, all_data)
      } else {
        pop_data <- NULL
      }
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
        p <- ggplot(population_data$data, aes(x = time_sequence, y = population, color = district)) +
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
            panel.grid.minor.x = element_line(color = "grey90", size = 0.3)
          )
        ggplotly(p)
      } else {
        p <- ggplot(population_data$data, aes(x = month, y = population, color = district)) +
          geom_line(linewidth = 1) +
          geom_point(size = 2) +
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
            legend.text = element_text(size = 10)
          )
        ggplotly(p)
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
          # 過濾掉空白區域
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
        # 過濾掉空白區域
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
                                    group = 鄉鎮市區_The.villages.and.towns.urban.district)) +
          geom_line(linewidth = 1) +
          geom_point(size = 3) +
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
            legend.text = element_text(size = 10)
          )
        ggplotly(p)
      } else {
        p <- ggplot(price_data$data, aes(x = month, y = price_per_sqm, 
                                    color = 鄉鎮市區_The.villages.and.towns.urban.district)) +
          geom_line() +
          geom_point() +
          scale_x_continuous(breaks = 1:12) +
          labs(title = paste(price_data$city, price_data$year, "年房價趨勢"),
               x = "月份",
               y = "每平方公尺單價",
               color = "鄉鎮市區") +
          scale_y_continuous(labels = scales::comma) +
          theme_minimal() +
          theme(legend.position = "right")
        ggplotly(p)
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
          # 過濾掉空白區域
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
          # 過濾掉空白區域
          data <- data[data$鄉鎮市區_The.villages.and.towns.urban.district != "", ]
          data$year <- year
          data
        }
      })
      price_data_corr <- do.call(rbind, all_price_data)
    } else {
      pop_data <- read_population_data(input$corr_city, input$corr_year, input$corr_district)
      if(!is.null(pop_data)) {
        # 過濾掉空白區域
        pop_data <- pop_data[pop_data$district != "", ]
      }
      
      price_data_corr <- read_house_price_data(input$corr_city, input$corr_year, input$corr_district)
      if(!is.null(price_data_corr)) {
        # 過濾掉空白區域
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
      p <- ggplot(correlation_data$data, aes(x = population, y = price_per_sqm, color = district)) +
        geom_point(size = 3) +
        geom_smooth(method = "lm", se = TRUE) +
        labs(title = paste(correlation_data$city, 
                           if(correlation_data$year == "全部") "歷年" else paste(correlation_data$year, "年"),
                           "人口與房價相關性分析"),
             x = "人口數",
             y = "每平方公尺單價",
             color = "鄉鎮市區") +
        theme_minimal() +
        theme(legend.position = "right")
      ggplotly(p)
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
}
