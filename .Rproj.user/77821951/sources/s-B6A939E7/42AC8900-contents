library(shiny)
library(ggplot2)
library(readr)
library(lubridate)
library(dplyr)

df <- read_csv("./air_quality.csv", col_names = c("timestamp", "pmt_2_5", "pmt_10"), lazy = F)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      radioButtons("var", "Select variable to plot",
                   c("PMT 2.5 μg/m3" = "pmt_2_5",
                     "PMT 10 μg/m3" = "pmt_10")),
      
      uiOutput("date_selection"),
      
      checkboxInput("toggle_date_mode", "Enter custom date range", value = FALSE)
      
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

server <- function(input, output, session) {
  
  output$date_selection <- renderUI({
    if(!input$toggle_date_mode){
      selectInput("date_interval", label = "Date interval", 
                  choices = c("Previous 24 hours", "Previous 7 days", "Previous month", "All data"))
    } else {
      dateRangeInput("date_interval", label = "Date range")
    }
  })
  
  observe({
    #selected_parameters$vars <- input$var_selection
    
    #if(!is.null(input$date_interval)){
    #  input$date_interval <- input$date_interval
    #}
    
  })
  
  plotting_data <- reactive({
    req(input$date_interval)
    
    if(is.Date(input$date_interval[1])){
      df %>%
        filter(timestamp >= input$date_interval[1],
               timestamp <= input$date_interval[2])
      
    } else if(input$date_interval == "Previous 7 days"){
      df %>%
        filter(timestamp >= max(timestamp) - weeks(1))
      
    } else if(input$date_interval == "All data"){
      df
      
    } else if(input$date_interval == "Previous 24 hours"){
      df %>%
        filter(timestamp >= max(timestamp) - hours(24))
      
    } else if(input$date_interval == "Previous month"){
      df %>%
        filter(timestamp >= max(timestamp) %m-% months(1))
    } 
  })
  
  output$plot <- renderPlot({
    req(input$var)
    ggplot(plotting_data(), aes_string("timestamp", input$var)) + geom_point()
  })
}

shinyApp(ui, server)