# Author: Sihyuan Han

library(readr)
library(tidyverse)

estate <- readr::read_csv("./data/estate.csv",
                          col_types = cols(
                            AC = col_factor(),
                            Pool = col_factor(),
                            Highway = col_factor()
                          ))
estate %>% 
  mutate(Price = Price/1000) ->
  estate

readr::write_rds(estate,file = "./data/estate.rds" , compress = "gz")

estate <- read_rds("./data/estate.rds")

t.test(estate$Price)

library(shiny)

ui <- fluidPage(
  mainPanel(
    tabsetPanel(type = "pills",
                tabPanel("Univariate",
                  varSelectInput("var1", "Variable?", data = estate),
                  checkboxInput("Log1", "Log_Transform?"), 
                  sliderInput("num1", "Number of Bins?", value = 40, min = 1, max = 100),
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
  )
)

server <- function(input, output, session) {
  output$code <- renderPrint({
    t.test(estate$Price)
  })
  
  output$dynamic <- renderDataTable({
    estate
  })
}

shinyApp(ui, server)