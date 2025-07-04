library(shiny)
library(shinydashboard)

source("global.R")

ui <- dashboardPage(
  dashboardHeader(
    title = "台灣人口遷移與房價關係分析",
    titleWidth = 300
  ),
  
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("人口趨勢", tabName = "population", icon = icon("users")),
      menuItem("房價趨勢", tabName = "price", icon = icon("home")),
      menuItem("人口-房價雙軸圖", tabName = "dualaxis", icon = icon("chart-bar")),
      menuItem("相關性分析", tabName = "correlation", icon = icon("chart-line"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "population",
        fluidRow(
          box(
            title = "控制面板",
            width = 3,
            selectInput(
              "city",
              "選擇城市：",
              choices = names(CITY_CODES),
              selected = names(CITY_CODES)[1]
            ),
            selectInput(
              "district",
              "選擇區域：",
              choices = c("全部", "總計"),
              selected = "全部"
            ),
            selectInput(
              "year",
              "選擇年份：",
              choices = c("全部", 104:113),
              selected = "全部"
            ),
            actionButton("analyze", "分析")
          ),
          box(
            title = "人口趨勢圖",
            width = 9,
            plotlyOutput("population_plot")
          )
        )
      ),
      
      tabItem(
        tabName = "price",
        fluidRow(
          box(
            title = "控制面板",
            width = 3,
            selectInput(
              "price_city",
              "選擇城市：",
              choices = names(CITY_CODES),
              selected = names(CITY_CODES)[1]
            ),
            selectInput(
              "price_district",
              "選擇區域：",
              choices = c("全部"),
              selected = "全部"
            ),
            selectInput(
              "price_year",
              "選擇年份：",
              choices = c("全部", 104:113),
              selected = "全部"
            ),
            actionButton("analyze_price", "分析")
          ),
          box(
            title = "房價趨勢圖",
            width = 9,
            plotlyOutput("price_plot")
          )
        )
      ),

      tabItem(
        tabName = "dualaxis",
        fluidRow(
          box(
            title = "控制面板",
            width = 3,
            selectInput(
              "dual_city",
              "選擇城市：",
              choices = names(CITY_CODES),
              selected = names(CITY_CODES)[1]
            ),
            selectInput(
              "dual_district",
              "選擇區域：",
              choices = NULL,
              selected = NULL
            ),
            selectInput(
              "dual_year",
              "選擇年份：",
              choices = c("全部", 104:113),
              selected = "全部"
            ),
            actionButton("analyze_dual", "分析")
          ),
          box(
            title = "人口-房價雙軸圖",
            width = 9,
            plotlyOutput("dualaxis_plot")
          )
        )
      ),
      
      tabItem(
        tabName = "correlation",
        fluidRow(
          box(
            title = "控制面板",
            width = 3,
            selectInput(
              "corr_city",
              "選擇城市：",
              choices = names(CITY_CODES),
              selected = names(CITY_CODES)[1]
            ),
            selectInput(
              "corr_district",
              "選擇區域：",
              choices = NULL,
              selected = NULL
            ),
            selectInput(
              "corr_year",
              "選擇年份：",
              choices = c("全部", 104:113),
              selected = "全部"
            ),
            actionButton("analyze_corr", "分析")
          ),
          box(
            title = "相關性分析",
            width = 9,
            plotlyOutput("correlation_plot"),
            verbatimTextOutput("correlation_text")
          )
        )
      )
    )
  )
)
