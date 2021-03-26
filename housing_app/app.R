# Author: Sihyuan Han

estate <- readr::read_csv("./data/estate.csv")
readr::write_rds(estate,file = "./data/estate.rds" , compress = "gz")

library(shiny)
library(readr)
estate <- read_rds("./data/estate.rds")

ui <- fluidPage(
  varSelectInput("var1", "Variable?", data = estate),
  checkboxInput("Log1", "Log_Transform?"), 
  sliderInput("num1", "Number of Bins?", value = "40", min = 1, max = 100),
  numericInput("num2", "Null Value", value = 0)
)

server <- function(input, output, session) {
  output$estate <- renderTable({
    head(estate)
  })
}

shinyApp(ui, server)