# Author: Sihyuan Han

library(shiny)
library(ggplot2)
plotchoices <- c("Density Plot", "Histogram", "Frequency Polygon")


ui <- fluidPage(
    varSelectInput("var1", "X variable", data = mtcars),
    radioButtons("plottype", "Choose a plot type", choices = plotchoices),
    # plotOutput("plot")
)

server <- function(input, output) {
    # output$plot <- renderPlot({
    #     ggplot(mtcars, aes(x = !!input$var1, y = !!input$var2)) +
    #         geom_point()
    # })
}

shinyApp(ui = ui, server = server)