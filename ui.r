fluidPage(
  titlePanel("台灣人口遷移與房價關係分析"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("city", "選擇城市：",
                 choices = names(city_codes),
                 selected = names(city_codes)[1]),
      selectInput("year", "選擇年份：",
                 choices = 104:112,
                 selected = 104),
      actionButton("analyze", "分析")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("人口趨勢",
                 plotOutput("population_plot")),
        tabPanel("房價趨勢",
                 plotOutput("price_plot")),
        tabPanel("相關性分析",
                 plotOutput("correlation_plot"),
                 verbatimTextOutput("correlation_text"))
      )
    )
  )
)
