# Author: Sihyuan Han

library(readr)
library(tidyverse)
library(ggplot2)

estate <- readr::read_csv("./data/estate.csv",
                          col_types = cols(
                            AC = col_factor(),
                            Pool = col_factor(),
                            Style = col_factor(),
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
  # main space
  fluidRow(
  tabsetPanel(type = "tabs",
                tabPanel("Univariate",
                         sidebarLayout(
                           sidebarPanel(
                             varSelectInput("var1", "Variable?", data = estate),
                             checkboxInput("log1", "Log_Transform?", value = FALSE), 
                             sliderInput("bins", "Number of Bins?", value = 40, min = 1, max = 100),
                             numericInput("num2", "Null Value", value = 0),
                             tableOutput("ttestdf")
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
                             checkboxInput("log2", "Log_Transform?"), 
                             varSelectInput("var3", "Y Variable?", data = estate),
                             checkboxInput("log3", "Log_Transform?"),
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
  ),
  # log ols space
  fluidRow(column(4, verbatimTextOutput("slm")),
           column(4, plotOutput("plot3")),
           column(4, plotOutput("plot4"))
           )
)


server <- function(input, output, session) {
# First Tab
  # t.test table
  output$ttestdf <- renderTable({
    if(is.numeric(estate[[input$var1]])){
      if(input$log1){
        estate %>% 
          select(input$var1) %>% 
          log() %>% 
          t.test(mu = input$num2) ->
          tout
        tout %>% 
          broom::tidy() %>% 
          select("P-value" = p.value, "Estimate" = estimate, "95% Lower" = conf.low, "95% Upper" = conf.high) ->
          toutvalue
        toutvalue
      } else{
        estate %>% 
          select(input$var1) %>% 
          t.test(mu = input$num2) ->
          tout
        tout %>% 
          broom::tidy() %>% 
          select("P-value" = p.value, "Estimate" = estimate, "95% Lower" = conf.low, "95% Upper" = conf.high) ->
          toutvalue
        toutvalue
      }
    } else{
      validate(
        need(is.numeric(estate[[input$var1]]), "Variable is not numeric")
      )
    }
  })
  
  # plot
  output$plot1 <- renderPlot({
    # modularity
    p1 <- ggplot(estate, aes(x = !!input$var1))
    
    # if log
    if(isTRUE(input$log1)){
      p1 <- p1 +
        scale_x_log10() +
        labs(x = paste("Log(",input$var1,")"))
    }
    
    # if-else numeric var
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
    p2 <- ggplot(data = estate, aes(x = !!input$var2, y = !!input$var3))
    
    # if log x,y
    if(isTRUE(input$log2)){
      p2 <- p2 +
        scale_x_log10() +
        labs(x = paste("Log(",input$var2,")"))
      
    # validate
      validate(
        need(is.numeric(estate[[input$var2]]), "Factor Variable Cannot Do Log_Transform!")
      )
    }
    
    if(isTRUE(input$log3)){
      p2 <- p2 +
        scale_y_log10() +
        labs(x = paste("Log(",input$var3,")"))
      
      # validate
      validate(
        need(is.numeric(estate[[input$var3]]), "Factor Variable Cannot Do Log_Transform!")
      )
    }
    
    # if ols
    if(isTRUE(input$ols)){
      p2 <- p2 +
        geom_smooth(method = "lm", se = FALSE)
    }
    
    # if-else numeric/factor
    if(is.numeric(estate[[input$var2]]) && is.numeric(estate[[input$var3]])){
      p2 <- p2 +
        geom_point()
    } else if (is.factor(estate[[input$var2]]) && is.factor(estate[[input$var3]])){
      p2 <- p2 +
        geom_jitter()
    } else{
      p2 <- p2 +
        ggstance::geom_boxploth()
    }
    p2
  })
  
# Third Tab
  output$dynamic <- renderDataTable({
    est_onlynum <- estate[ , purrr::map_lgl(estate, is.numeric)]
    est_onlynum
  }, options = list(iDisplayLength = 10))
  
# ols part
# ols part: lm summary
  output$slm <- renderPrint({
    if(isTRUE(input$ols)){
      lmout <- lm(estate[[input$var3]] ~ estate[[input$var2]])
      summary(lmout)
    }
  })
  
# ols part: residual plot
  output$plot3 <- renderPlot({
    if(isTRUE(input$ols)){
      lmres <- lm(log(estate[[input$var3]]) ~ log(estate[[input$var2]]))
      qplot(x = lmres$fitted, y = lmres$residuals)
    }
  })
  
  
# ols part: QQ plot 
  output$plot4 <- renderPlot({
    if(isTRUE(input$ols)){
      lmres <- lm(log(estate[[input$var3]]) ~ log(estate[[input$var2]]))
      qplot(sample = lmres$residuals, geom = "qq") +
        geom_qq_line()
    }
  })
    
}

shinyApp(ui, server)