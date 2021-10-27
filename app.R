library(ggplot2)
library(bslib)
library(thematic)
library(DT)
library(shiny)
library(tidyverse)
library(RColorBrewer)


ui <- fluidPage(
   theme = bslib::bs_theme(bootswatch = "flatly"),
    
    tabsetPanel(
        #sample size for equality of means
        tabPanel("Equality of Means",
                 titlePanel("Sample Size for Equality of Means"),
                 fluidRow(
                     column(3,
                            "Distribution 1",
                            numericInput("mu1", "Mean 1", value = 132.85, min = 0),
                            numericInput("s1", "Standard Deviation 1", value = 15.34, min = 0)
                     ),
                     column(3,
                            "Distribution 2",
                            numericInput("mu2", "Mean 2", value = 127.44, min = 0),
                            numericInput("s2", "Standard Deviation 2", value = 18.23, min = 0)
                     ),
                     column(4,
                            "Significance Level and Power",
                            sliderInput("alpha", label = "alpha", value = 0.05, min = 0, max = 0.05, step = 0.001),
                            sliderInput("power", label = "Power", value = 0.08, min = 0, max = 1, step = 0.001)
                     ),
                 ),
                 
                 #output sample size n
                 fluidRow(
                     column(10, textOutput("n_norm"))
                 ),
                 #output normal plot
                 fluidRow(
                     column(10, plotOutput("plotnorm"))
                 )),
        #sample size for binomial proportions
        tabPanel("Two Binomial Distributions",
                 titlePanel("Sample Size for Comparing Two Binomial Proportions"),
                 fluidRow(
                     column(3,
                            "Distribution 1",
                            numericInput("p1", label = "Proportion 1", value = 0.0015, min = 0, max = 1),
                            "Distribution 2",
                            numericInput("p2", label = "Proportion 2", value = 0.0012, min = 0, max = 1),
                            numericInput("k", label = "k", value = 1, min = 1)),
                     column(3,
                            "Significance Level and Power",
                            sliderInput("alpha_bin", label = "alpha", value = 0.05, min=0, max=0.05, step = 0.001),
                            sliderInput("power_bin", label = "Power", value = 0.08, min = 0, max = 1, step = 0.001),
                     )),
                 #output sample size n
                 fluidRow(
                     column(8, verbatimTextOutput("n_bin"))
                 ),
                 #output bin plot
                 fluidRow(
                   column(10, plotOutput("plotbin"))
        )),
        
        #sample size for survival curves
        tabPanel("Survival Curves",
                 titlePanel("Sample Size for Survival Curves"), 
                 fluidRow(
                     column(3, 
                            "Group 1",
                            numericInput("hr", "Hazard Ratio", value = 0.7, min = 0, max = 1),
                            numericInput("k_survival", "k", value = 1, min = 0)),
                     column(3,
                            "Proportions",
                            numericInput("pE", "Proportion Participants in Experimental", value = 0.3707, min = 0, max = 1),
                            numericInput("pC", "Proportion Participants in Control", value = 0.4890, min = 0, max = 1)),
                     column(4,
                            "Significance Level and Power",
                            sliderInput("alpha_survival", label = "alpha", value = 0.05, min = 0, max = 0.05, step = 0.001),
                            sliderInput("power_survival", label = "Power", value = 0.08, min = 0, max = 1, step = 0.001)
                            
                     )
                 ),
                 #output sample size n
                 fluidRow(
                     column(8, verbatimTextOutput("n_survival"))
                 )
        )
    )
)
server <- function(input, output, session) {
  
  #normal  n
  output$n_norm = renderText({
    n_norm = round(n_norm_reactive(), digits = 3)
    paste("Sample Size: ", n_norm)
  })
  
  #reactive normal n
  n_norm_reactive = reactive({
    zalpha = qnorm(1-input$alpha/2,0,1)
    zbeta = qnorm(input$power,0,1)
    n_norm_reactive = ((input$s1*input$s1 + input$s2*input$s2)*(zalpha+zbeta)^2)/(input$mu2-input$mu1)^2
    n_norm_reactive
  })
  
  #plot reactive normal n
    output$plotnorm = renderPlot({
        rand = data.frame(1:(n_norm_reactive()), rnorm(n_norm_reactive(), input$mu1, input$s1), rnorm(n_norm_reactive(), input$mu2, input$s2))
        colnames(rand) = c("subjects", "values", "values2")
        ggplot(rand) +
          geom_histogram(aes(x=values), fill = "#1B9E77", colour = "#1B9E77", alpha = 0.5)+
          geom_histogram(aes(x=values2), fill = "#D95F02", colour = "#D95F02", alpha = 0.5)+
          xlab("Values") + 
          ylab("Frequency") +
          theme(panel.background = element_rect(fill = "transparent"),
                plot.background = element_rect(fill = "transparent", color = NA))
        
  })
    
    #binomial n
    output$n_bin = renderText({
        q1 = 1-input$p1
        q2 = 1-input$p2
        zalpha = qnorm(1-input$alpha_bin/2,0,1)
        zbeta = qnorm(input$power_bin,0,1)
        p = (input$p1+input$k*input$p2)/(1+input$k)
        q = 1-p
        num = sqrt(p*q*(1+1/input$k))*zalpha + sqrt(input$p1*q1+input$p2*q2/input$k)*zbeta
        n_bin = round(num^2 / (input$p1-input$p2)^2, digits = 3)
        paste("Sample Size: ", n_bin)
    })
    
    #reactive binomial n
    n_bin_reactive = reactive({
      q1 = 1-input$p1
      q2 = 1-input$p2
      zalpha = qnorm(1-input$alpha_bin/2,0,1)
      zbeta = qnorm(input$power_bin,0,1)
      p = (input$p1+input$k*input$p2)/(1+input$k)
      q = 1-p
      num = sqrt(p*q*(1+1/input$k))*zalpha + sqrt(input$p1*q1+input$p2*q2/input$k)*zbeta
      n_bin_reactive = num^2 / (input$p1-input$p2)^2
      n_bin_reactive
    })
    
    #plot reactive normal n
    output$plotbin = renderPlot({
      rand = data.frame(1:(n_bin_reactive()), rbinom(1:(n_bin_reactive()), n_bin_reactive(), input$p1))
      colnames(rand) = c("subjects", "values3")
      ggplot(rand) +
        geom_histogram(aes(x=values3), fill = "#1B9E77", colour = "#1B9E77", alpha = 0.5)+
        #geom_histogram(aes(x=values2), fill = "#D95F02", colour = "#D95F02", alpha = 0.5)+
        xlab("Values") + 
        ylab("Frequency") +
        theme(panel.background = element_rect(fill = "transparent"),
              plot.background = element_rect(fill = "transparent", color = NA))
      
    })
    
    output$n_survival = renderText({
        zalpha = qnorm(1-input$alpha/2,0,1)
        zbeta = qnorm(input$power,0,1)
        m = (1/input$k)*((input$k*input$hr+1)/(input$hr-1))^2*(zalpha+zbeta)^2
        n_survival = m/(input$k*input$pE + input$pC)
    })
    
}
shinyApp(ui = ui, server = server)