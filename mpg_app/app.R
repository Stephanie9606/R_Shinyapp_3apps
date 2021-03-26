# Author: Sihyuan Han

library(shiny)
library(ggplot2)

ui <- fluidPage(
    varSelectInput("var1", "X variable", data = mpg, selected = "cty"),
    varSelectInput("var2", "Y variable", data = mpg, selected = "hwy"),
    varSelectInput("colc", "Color cariable (categorical)", data = mpg, selected = "class"),
    plotOutput("plot")
)

server <- function(input, output) {
    output$plot <- renderPlot({
        ggplot(mpg, aes(x = !!input$var1, y = !!input$var2, col = !!input$colc)) +
            geom_point() +
            theme_bw()
    })
}

shinyApp(ui = ui, server = server)