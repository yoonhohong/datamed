# 필요한 라이브러리 로드
library(shiny)
library(gapminder)

# UI 정의
ui <- fluidPage(
  titlePanel("Gapminder 데이터셋을 이용한 Shiny 앱"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "국가 선택:", choices = unique(gapminder$country)),
      br(),
      helpText("선택한 국가의 인구, GDP 및 기대 수명을 확인하세요.")
    ),
    
    mainPanel(
      tableOutput("country_data")
    )
  )
)

# Server 정의
server <- function(input, output) {
  
  selected_country_data <- reactive({
    filter(gapminder, country == input$country)
  })
  
  output$country_data <- renderTable({
    selected_country_data()
  })
}

# Shiny 앱 실행
shinyApp(ui = ui, server = server)
