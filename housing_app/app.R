# Author: Sihyuan Han

library(readr)
library(tidyverse)
library(ggplot2)

estate <- readr::read_csv("./data/estate.csv",
                          col_types = cols(
                            AC = col_factor(),
                            Pool = col_factor(),
                            Highway = col_factor()
                          ))
estate %>% 
  mutate(`Price($K)` = Price/1000) %>% 
  select(`Price($K)`, everything(), -Price) ->
  estate

readr::write_rds(estate,file = "./data/estate.rds" , compress = "gz")

estate <- read_rds("./data/estate.rds")

library(shiny)

ui <- fluidPage(
  mainPanel(
    tabsetPanel(type = "pills",
                tabPanel("Univariate",
                  varSelectInput("var1", "Variable?", data = estate),
                  checkboxInput("log1", "Log_Transform?"), 
                  sliderInput("bins", "Number of Bins?", value = 40, min = 1, max = 100),
                  numericInput("num2", "Null Value", value = 0),
                  verbatimTextOutput("code")
                ),
                tabPanel("Bivariate",
                  varSelectInput("var2", "X Variable?", data = estate),
                  checkboxInput("Log2", "Log_Transform?"), 
                  varSelectInput("var3", "Y Variable?", data = estate),
                  checkboxInput("Log3", "Log_Transform?"),
                  checkboxInput("OLS1", "Fit OLS?")
                ),
                tabPanel("Spreadsheet",
                  dataTableOutput("dynamic")
                )
    )
  ),
  sidebarPanel(
    plotOutput("plot")
  )
)

server <- function(input, output, session) {
  
  attach(estate)
  
  output$code <- renderPrint({
    t.test(estate[[input$var1]], null.value = input$num2)
  })
  
  output$dynamic <- renderDataTable({
    estate
  })
  
  output$plot <- renderPlot({
    ggplot(estate, aes(x = !!input$var1)) +
      geom_histogram(bins = input$bins)
  })
}

shinyApp(ui, server)