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
                  checkboxInput("log1", "Log_Transform?", value = FALSE), 
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
  
  output$code <- renderPrint({
    
   # new df
   est_newdf <- estate  
      
   if(isTRUE(input$log1)){
      est_newdf$ttvalue <- log(est_newdf[[as.character(input$var1)]])
    }
    
   if(!isTRUE(input$log1)){
      est_newdf$ttvalue <- est_newdf[[as.character(input$var1)]]
    }
    
   # validate(
   #   need(is.factor(est_newdf$ttvalue) == TRUE, "Variable is not numeric")
   #  )
   
    tout <- t.test(est_newdf$ttvalue, null.value = input$num2)
    print(tout)
  })
  
  # spreadsheet
  output$dynamic <- renderDataTable({
    estate
  })
  
  # plot
  output$plot <- renderPlot({
    
    # new df
    est_newdf <- estate  
    
    if(isTRUE(input$log1)){
      est_newdf$ttvalue <- log(est_newdf[[as.character(input$var1)]])
    }
    
    if(!isTRUE(input$log1)){
      est_newdf$ttvalue <- est_newdf[[as.character(input$var1)]]
    }
    
    # if else numeric var
    if(is.numeric(est_newdf$ttvalue) == T){
      ggplot(est_newdf, aes(x = ttvalue)) +
        geom_histogram(bins = input$bins)
    } else{
      ggplot(est_newdf, aes(x = ttvalue)) +
        geom_bar(bins = input$bins)
    }
  
  })
}

shinyApp(ui, server)