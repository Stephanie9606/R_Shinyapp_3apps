# Author: Sihyuan Han

library(readr)
library(tidyverse)
library(ggplot2)
library(ggstance)

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
  titlePanel("EDA of Estate Data"),
  tabsetPanel(type = "tabs",
                tabPanel("Univariate",
                         sidebarLayout(
                           sidebarPanel(
                             varSelectInput("var1", "Variable?", data = estate),
                             checkboxInput("log1", "Log_Transform?", value = FALSE), 
                             sliderInput("bins", "Number of Bins?", value = 40, min = 1, max = 100),
                             numericInput("num2", "Null Value", value = 0),
                             verbatimTextOutput("code")
                           ),
                           mainPanel(
                             plotOutput("plot1")
                           )
                         )
                ),
                tabPanel("Bivariate",
                         sidebarLayout(
                           sidebarPanel(
                             varSelectInput("var2", "X Variable?", data = estate),
                             checkboxInput("Log2", "Log_Transform?"), 
                             varSelectInput("var3", "Y Variable?", data = estate),
                             checkboxInput("Log3", "Log_Transform?"),
                             checkboxInput("ols", "Fit OLS?")
                           ),
                           mainPanel(
                             plotOutput("plot2")
                           )
                         )
                ),
                tabPanel("Spreadsheet",
                  dataTableOutput("dynamic")
                )
    )
  )

server <- function(input, output, session) {
# First Tab
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
  
  # plot
  output$plot1 <- renderPlot({
    
    p1 <- ggplot(estate, aes(x = !!input$var1))
    
    # if log
    if(isTRUE(input$log1)){
      p1 <- p1 +
        scale_x_log10()
    }
    
    # if else numeric var
    if(is.numeric(estate[[input$var1]])){
      p1 <- p1 +
        geom_histogram(bins = input$bins)
    } else {
      p1 <- p1 +
        geom_bar(bins = input$bins)
    }
    p1
  })
  
# Second Tab
  output$plot2 <- renderPlot({
    # modularity
    pp <- ggplot(data = estate, aes(x = !!input$var2, y = !!input$var3))
    
    # if else numeric var
     if(is.numeric(estate[[input$var2]]) && is.numeric(estate[[input$var3]])){
       pp <- pp +
         geom_point()
     } else if (isTRUE(!!input$log2)){
       pp <- pp +
       scale_x_log10() }
    # } else if (isTRUE(estate[[input$ols]])){
    #   pp <- pp +
    #     geom_smooth(method = "lm")
    # }
    # 
    # if(is.factor(estate[[input$var2]]) && is.factor(estate[[input$var3]])){
    #   pp <- pp +
    #     geom_boxploth()
    # }
    # 
    # if(is.factor(estate[[input$var2]]) && is.factor(estate[[input$var3]])){
    #   pp <- pp +
    #     geom_jitter()
    # }
    
    # if else num/factor
    if(is.numeric(estate[[input$var2]]) && is.numeric(estate[[input$var3]])){
      pp <- pp +
        geom_point()
    } else if (is.factor(estate[[input$var2]]) && is.factor(estate[[input$var3]])){
      pp <- pp +
        geom_jitter()
    } else {
      pp <- pp +
        geom_boxploth()
    }

    pp
  })
  
# Third Tab
  output$dynamic <- renderDataTable({
    estate
  })  
}

shinyApp(ui, server)