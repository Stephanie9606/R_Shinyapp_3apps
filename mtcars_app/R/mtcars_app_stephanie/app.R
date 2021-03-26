# Author: Sihyuan Han

library(shiny)
library(ggplot2)
plotchoices <- c("Density Plot", "Histogram", "Frequency Polygon")

ui <- fluidPage(
    varSelectInput("var1", "X variable", data = mtcars),
    radioButtons("plottype", "Choose a plot type", choices = plotchoices),
    plotOutput("plot")
)

server <- function(input, output) {
    
    # plotType
    plotType <- function(x, type) {
        switch(input$plottype,
               "Density Plot" = geom_density(x, outline.type = "full"),
               "Histogram" = geom_histogram(x),
               "Frequency Polygon" = geom_freqpoly(x))
    }
    
    # reactive ggplot
    p <- reactive({
        ggplot(mtcars, aes(x = !!input$var1))
    })
    
    # plot
    output$plot <- renderPlot({
        p() +
            plotType(input$plotType) +
            theme_bw()
    })
}

shinyApp(ui = ui, server = server)