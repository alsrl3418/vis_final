library(shiny)
library(tidyverse)
library(lubridate)
load("mapsaved.RData")

ui <- fluidPage(
  titlePanel("지역별 고용지표 분석"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("고용 지표를 선택하고 원하는 시점을 선택하세요"),
      
      selectInput("laborindex",
                  label = "고용 지표 선택",
                  choices = c("15세이상인구","경제활동인구",
                              "비경제활동인구","경제활동참가율",
                              "실업률","고용률","15~64세 고용률"),
                  selected = "고용률"),
      
      dateInput("date",
                label = "해당 월 선택(매월 1일로 선택해 주세요)",
                value = "2020-01-01",
                format = "yyyy-mm-01"),
      
      radioButtons("sex",
                   label = "성별 선택",
                   choices = c("전체", "남자" , "여자"),
                   selected = "전체"),
      
      img(src = "image101.jpg", height = 100, width = 150)
      
      
    ),
    mainPanel(plotOutput("plot1"))
  ),
  
  fluidRow(
    
    selectInput("area",
                label = "지역 선택",
                choices = c(unique(emprates$시도별)),
                selected = "서울특별시"),
    
    
    plotOutput(outputId = "plot2", height = "300px")
  )
  
  
)

server <- function(input, output) {
  selectedData = reactive({
    emprates 
  })
  
  output$plot1 = renderPlot({
    map +
      geom_point(data = selectedData() %>%
                   filter(성별 == input$sex, 항목 == input$laborindex , dates == input$date) %>%
                   rename(region = "시도별", rates = "값") %>%
                   left_join(region_latlon, by = "region") %>%
                   mutate(rates = as.numeric(rates))
                 , aes(lon, lat, size = rates, col = rates)) +
      scale_color_distiller(palette = "YlOrRd")
    
  })

  output$plot2 = renderPlot({
    ggplot(data = selectedData()%>%
             filter(성별 == input$sex, 항목 == input$laborindex, 시도별 == input$area) %>%
             rename(region = "시도별", rates = "값")
           ,aes(x = dates, y = rates, col = region)) +
      geom_line()
    
    
    
  })
  
  
  
  
}


shinyApp(ui, server)







