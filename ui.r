fluidPage(
  titlePanel("台灣各縣市房價與人口變化分析"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("city", "選擇縣市：",
                 choices = city_list,
                 selected = "台北市"),
      
      dateRangeInput("date_range", "選擇日期範圍：",
                    start = date_range[1],
                    end = date_range[2]),
      
      selectInput("plot_type", "選擇圖表類型：",
                 choices = c("人口變化趨勢" = "population",
                           "房價變化趨勢" = "price",
                           "人口與房價關係" = "correlation"),
                 selected = "population")
    ),
    
    mainPanel(
      plotlyOutput("main_plot"),
      br(),
      tableOutput("data_table")
    )
  )
)
