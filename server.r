function(input, output, session) {
  
  # 根據選擇的縣市和日期範圍過濾資料
  filtered_data <- reactive({
    subset(merged_data,
           city == input$city &
           date >= input$date_range[1] &
           date <= input$date_range[2])
  })
  
  # 繪製主要圖表
  output$main_plot <- renderPlotly({
    data <- filtered_data()
    
    if (input$plot_type == "population") {
      p <- ggplot(data, aes(x = date)) +
        geom_line(aes(y = population, color = "總人口")) +
        geom_line(aes(y = net_migration, color = "淨遷入人口")) +
        labs(title = paste(input$city, "人口變化趨勢"),
             x = "日期",
             y = "人口數",
             color = "指標") +
        theme_minimal()
      
    } else if (input$plot_type == "price") {
      p <- ggplot(data, aes(x = date, y = price)) +
        geom_line(color = "red") +
        labs(title = paste(input$city, "房價變化趨勢"),
             x = "日期",
             y = "房價") +
        theme_minimal()
      
    } else {
      p <- ggplot(data, aes(x = population, y = price)) +
        geom_point() +
        geom_smooth(method = "lm") +
        labs(title = paste(input$city, "人口與房價關係"),
             x = "人口數",
             y = "房價") +
        theme_minimal()
    }
    
    ggplotly(p)
  })
  
  # 顯示資料表格
  output$data_table <- renderTable({
    data <- filtered_data()
    # 只顯示重要的欄位
    data[, c("date", "population", "net_migration", "price")]
  }, digits = 2)
}
