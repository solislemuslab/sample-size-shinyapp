# R Shinyapp Sample size

This shinyapp will calculate the sample size needed for a given power.

library(ggplot2)
library(bslib)
library(thematic)
library(DT)




library(shiny)

ui <- fluidPage(
    theme = bslib::bs_theme(bootswatch = "darkly"),

    #sample size for equality of means
    
    titlePanel("Sample Size Shiny App"),
    fluidRow(
        column(3,
           "Distribution 1",
            numericInput("mu1", "Mean 1", value = 132.85, min=0),
            numericInput("s1", "Standard Deviation 1", value = 15.34, min=0)
           ),
        column(3,
            "Distribution 2",
            numericInput("mu2", "Mean 2", value = 127.44, min=0),
            numericInput("s2", "Standard Deviation 2", value = 18.23, min=0)
            ),
        column(4,
               "Significance Level and Power",
               sliderInput("alpha", label = "alpha", value = 0.05, min=0, max=0.05, step = 0.01),
               sliderInput("power", label = "Power", value = 0.08, min = 0, max = 1, step = 0.01)
            ),
    ),
    #output sample size n
    fluidRow(
        column(8, verbatimTextOutput("n"))
    )
)

server <- function(input, output, session) {

    output$n = renderText({
        zalpha = qnorm(1-input$alpha/2,0,1)
        zbeta = qnorm(input$power,0,1)
        n = ((input$s1*input$s1 + input$s2*input$s2)*(zalpha+zbeta)^2)/(input$mu2-input$mu1)^2
        n
    })
    }

shinyApp(ui = ui, server = server)
