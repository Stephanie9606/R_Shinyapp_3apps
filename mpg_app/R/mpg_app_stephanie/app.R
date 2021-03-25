# Author: Sihyuan Han

library(shiny)
library(ggplot2)

ui <- fluidPage(
    varSelectInput("carcol", "X variable", data = mpg, selected = "cty"),
    varSelectInput("carcol", "Y variable", data = mpg, selected = "hwy"),
    varSelectInput("carcol", "Color cariable (categorical)", data = mpg, selected = "class")
)

server <- function(input, output) {
 
}

shinyApp(ui = ui, server = server)