function(input, output, session) {
  
  # 當點擊分析按鈕時觸發
  observeEvent(input$analyze, {
    # 讀取資料
    pop_data <- read_population_data(input$city, input$year)
    price_data <- read_house_price_data(input$city, input$year)
    
    # 繪製人口趨勢圖
    output$population_plot <- renderPlot({
  ggplot(pop_data, aes(x = date, y = population, group = 1)) +
    geom_line(color = "blue") +
    labs(title = paste0(input$city, " ", input$year, "年人口趨勢"),
         x = "日期",
         y = "人口數") +
    theme_minimal()
})
    
    # 繪製房價趨勢圖
    output$price_plot <- renderPlot({
      ggplot(price_data, aes(x = date, y = price_per_sqm)) +
        geom_line(color = "red") +
        labs(title = paste0(input$city, " ", input$year, "年房價趨勢"),
             x = "日期",
             y = "每平方公尺單價") +
        theme_minimal()
    })
    
    # 繪製相關性分析圖
    output$correlation_plot <- renderPlot({
      merged_data <- pop_data %>%
        left_join(price_data, by = "date") %>%
        na.omit()
      
      ggplot(merged_data, aes(x = population, y = price_per_sqm)) +
        geom_point() +
        geom_smooth(method = "lm", se = TRUE) +
        labs(title = "人口數與房價相關性分析",
             x = "人口數",
             y = "每平方公尺單價") +
        theme_minimal()
    })
    
    # 顯示相關係數
    output$correlation_text <- renderText({
      correlation <- calculate_correlation(pop_data, price_data)
      paste0("相關係數：", round(correlation, 4))
    })
  })
}
