library(ggplot2)
library(bslib)
library(thematic)
library(DT)
library(shiny)
library(tidyverse)
library(RColorBrewer)
library(ggfortify)
library(survival)
library(greekLetters)


ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "flatly"),
  
  tabsetPanel(
    #sample size for equality of means
    tabPanel("Equality of Means",
             titlePanel("Sample Size for Equality of Means"),
             fluidRow(
               column(10,
                      textOutput("mean_description")
               )),
             br(),
             fluidRow(
               column(4,
                      fluidRow(
                        column(6,
                               numericInput("mu1", "Mean 1", value = 132.85, min = 0,step = 0.001),
                               numericInput("s1", "Standard Deviation 1", value = 15.34, min = 0,step = 0.001)),
                        column(6,
                               numericInput("mu2", "Mean 2", value = 127.44, min = 0,step = 0.001),
                               numericInput("s2", "Standard Deviation 2", value = 18.23, min = 0, step = 0.001))),
                      numericInput("alpha", label = paste(greeks("alpha"), " (Significance Level)"), value = 0.05, min = 0.001, max = 0.05, step = 0.001),
                      numericInput("power", label = paste("1-", greeks("beta"), " (Power)"), value = 0.8, min = 0.001, max = .999, step = 0.001)),
               
               column(8, 
                      #output normal plot
                      plotOutput("plotnorm"),
                      #output sample size n
                      textOutput("n_norm"))
             )
    ),
    #sample size for binomial proportions
    tabPanel("Two Binomial Distributions",
             titlePanel("Sample Size for Comparing Two Binomial Proportions"),
             fluidRow(
               column(10,
                      textOutput("bin_description"),
               )),
             br(),
             fluidRow(
               column(4,
                      numericInput("p1", label = "Proportion 1", value = 0.015, min = 0, max = 1, step = 0.001),
                      numericInput("p2", label = "Proportion 2", value = 0.012, min = 0, max = 1, step = 0.001),
                      numericInput("k", label = "k", value = 1, min = 1, step = 0.25),
                      numericInput("alpha_bin", label = paste(greeks("alpha"), " (Significance Level)"), value = 0.05, min = 0.001, max = 0.05, step = 0.001),
                      numericInput("power_bin", label = paste("1-", greeks("beta"), " (Power)"), value = 0.8, min = 0.001, max = 0.999, step = 0.001)),
               
               column(8, 
                      #output bin plot
                      plotOutput("plotbin"),
                      #output sample size n -- Group 1
                      textOutput("n_bin"),
                      #output sample size kn -- Group 2
                      textOutput("kn_bin")
               )
             )),
    
    #sample size for survival curves
    tabPanel("Survival Curves",
             titlePanel("Sample Size for Survival Curves"), 
             fluidRow(
               column(10,
                      textOutput("surv_description")
               )),
             br(),
             fluidRow(
               column(3, 
                      numericInput("hr", "Hazard Ratio", value = 0.7, min = 0, max = 1),
                      numericInput("surv_k", "k", value = 1, min = 0)),
               column(3,
                      numericInput("pT", "Proportion Participants in Treatment", value = 0.3707, min = 0, max = 1),
                      numericInput("pC", "Proportion Participants in Control", value = 0.4890, min = 0, max = 1)),
               column(4,
                      numericInput("surv_alpha", label = paste(greeks("alpha"), " (Significance Level)"), value = 0.05, min = 0.001, max = 0.05, step = 0.001),
                      numericInput("surv_power", label = paste("1-", greeks("beta"), " (Power)"), value = 0.8, min = 0.001, max = 0.999, step = 0.001)
                      
               )
             ),
             #output sample size n
             fluidRow(
               column(8, textOutput("n_survival"))
             )
    )
  )
)
server <- function(input, output, session) {
  
  #normal description
  output$mean_description = renderText({
    "This tab calculates the sample size for the equality of the means for two normally distributed 
    samples of equal size. Input values under mean, standard deviation, significance level,
    and power to calculate the necessary sample size."
  })
  
  #normal n output
  output$n_norm = renderText({
    n_norm = round(n_norm_reactive(), digits = 3)
    paste("Sample Size per Group: ", n_norm)
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
    norm_data = data.frame(1:(n_norm_reactive()), rnorm(n_norm_reactive(), input$mu1, input$s1), rnorm(n_norm_reactive(), input$mu2, input$s2))
    big_norm_data = data.frame(big = 1:1000000, bignormvalues1 = rnorm(1000000, input$mu1, input$s1), bignormvalues2 = rnorm(1000000, input$mu2, input$s2))
    colnames(norm_data) = c("subjects", "normvalues1", "normvalues2")
    ggplot(, geom = 'blank') +   
      geom_line(aes(x = big_norm_data$bignormvalues1, y = ..density.., color = 'Population 1', col = "#1B9E77"), stat = 'density') +  
      geom_line(aes(x = big_norm_data$bignormvalues2, y = ..density.., color = 'Population 2', col = "#D95F02"), stat = 'density') +
      geom_histogram(aes(x = norm_data$normvalues1, y = ..density..), alpha = 0.4,fill = "#1B9E77", col = "#1B9E77") +    
      geom_histogram(aes(x = norm_data$normvalues2, y = ..density..), alpha = 0.4,fill = "#D95F02", col = "#D95F02") +
      xlab("Values") + 
      ylab("Frequency") +
      theme(panel.background = element_rect(fill = "transparent"),
            plot.background = element_rect(fill = "transparent", color = NA))+
      labs(colour = "Population", title = "Two Histograms of Two Graphs Based on Sample Size with Normal Densities Overlayed")
    
  })
  
  #binomial description
  output$bin_description = renderText({
    "This tab calculates the sample sizes needed to compare two binomial proportions using a two-sided
      test where the Group 2 sample is k times as large as the Group 1 sample and the samples are independent. Input values under the proportions, k,
      significance level, and power to calculate the necessary sample sizes."
  })
  
  #binomial n output
  output$n_bin = renderText({
    n_bin = round(n_bin_reactive(), digits = 3)
    paste("Sample Size for Group 1: ", n_bin)
  })
  
  #binomial kn output
  output$kn_bin = renderText({
    kn_bin = round(input$k * n_bin_reactive(), digits = 3)
    paste("Sample Size for Group 2: ", kn_bin)
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
  
  #plot reactive binomial n
  output$plotbin = renderPlot({
    success = 0:(n_bin_reactive())
    prob1 = dbinom((0:n_bin_reactive()), n_bin_reactive(), input$p1)
    data_bin = data.frame(success, prob1)
    big_norm_data2 = data.frame(big = 1:1000000, bignormvalues1 = rnorm(1000000, (n_bin_reactive())*(input$p1), (n_bin_reactive())*(input$p1)*(1-input$p1)), bignormvalues2 =  rnorm(1000000, ((input$k)*(n_bin_reactive())*(input$p2)), (input$k)*(n_bin_reactive())*(input$p2)*(1-input$p2)))
    ggplot(,) +   
      #geom_col(aes(x=factor(data_bin$success), y=prob1))  +                 
      geom_line(aes(x = big_norm_data2$bignormvalues1, y = ..density.., color = 'Group 1', col = "#1B9E77"), stat = 'density') +    
      geom_line(aes(x = big_norm_data2$bignormvalues2, y = ..density.., color = 'Group 2', col = "#1B9E77"), stat = 'density') +
      xlab("Values") + 
      ylab("Frequency") +
      theme(panel.background = element_rect(fill = "transparent"),
            plot.background = element_rect(fill = "transparent", color = NA)) +
      labs(colour = "Population", title = "Two Histograms of Two Proportions Based on Sample Size with Normal Densities Overlayed")
    
  })
  
  #survival description
  output$surv_description = renderText({
    "This tab calculates the sample size estimation for the comparison of survival curves
      between two groups under the Cox Proportional-Hazards Model. The ratio of participants
      in the treatment group to control is k. Input values under hazard ratio, k, significance level,
     and power to calculate the necessary sample size."
  })
  
  #survival n output
  output$n_survival = renderText({
    zalpha = qnorm(1-input$surv_alpha/2,0,1)
    zbeta = qnorm(input$surv_power,0,1)
    m = (1/input$surv_k)*((input$k*input$hr+1)/(input$hr-1))^2*(zalpha+zbeta)^2
    n_survival = m/(input$surv_k*input$pT + input$pC)
    paste("Sample Size: ", n_survival)
  })
  
}
shinyApp(ui = ui, server = server)

