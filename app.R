library(ggplot2)
library(bslib)
library(thematic)
library(DT)
library(shiny)
library(tidyverse)
library(greekLetters)
library(shinyhelper)


ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "flatly"),
  
  tabsetPanel(
    #welcome!
    tabPanel("Home",
             titlePanel("Welcome"),
             fluidRow(
               column(10,
               textOutput("welcome_description")
               )
             ),
             br(),
             fluidRow(
               column(10,
                      htmlOutput("welcome_info"))
             )
             ),
    
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
                               numericInput("power", label = paste("1-", greeks("beta"), " (Power)"), value = 0.8, min = 0.001, max = .999, step = 0.001),
                      helper(shiny:: actionButton('calculate',"Calculate"), title = '', icon = "question-circle", type = "inline",
                             content = "Values entered must be greater than 0 and within the specified range."
                             ),
                      ),
                     
               
               column(8, 
                      #output normal plot
                      plotOutput("plotnorm"),
                      #output sample size n
                      textOutput("n_norm")),
              
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
                      numericInput("p1", label = "Proportion 1", value = 0.15, min = 0, max = 1, step = 0.001),
                      numericInput("p2", label = "Proportion 2", value = 0.12, min = 0, max = 1, step = 0.001),
                      numericInput("k", label = "k", value = 1, min = 1, step = 0.25),
                      numericInput("alpha_bin", label = paste(greeks("alpha"), " (Significance Level)"), value = 0.05, min = 0.001, max = 0.05, step = 0.001),
                      numericInput("power_bin", label = paste("1-", greeks("beta"), " (Power)"), value = 0.8, min = 0.001, max = 0.999, step = 0.001),
                      helper(shiny:: actionButton('calculate_bin',"Calculate"), title = '', icon = "question-circle", type = "inline",
                             content = "Values entered must be greater than 0 and within the specified range."
                      ),
             ),
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
                      numericInput("surv_k", "k", value = 1, min = 0),
                      helper(shiny:: actionButton('calculate_surv',"Calculate"), title = '', icon = "question-circle", type = "inline",
                             content = "Values entered must be greater than 0 and within the specified range."
                      )),
               column(4,
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
    ),
    #welcome!
    tabPanel("Help",
             titlePanel("Help"),
             fluidRow(
               column(10,
                      textOutput("help_description")
               )
             ),
             br(),
             fluidRow(
               column(10,
                      htmlOutput('FAQ'))
             )
    ),
  )
)
server <- function(input, output, session) {
  
  v <- reactiveValues(data = iris,
                      plotnorm = NULL,
                      text = NULL)
  
  vbin <- reactiveValues(data = iris,
                      plotbin = NULL,
                      text = NULL)
  
  #welcome description
  output$welcome_description = renderText({
    "Welcome to the Sample Size Shiny App! Using respective tabs located at the top of the page, you can calculate
    the necessary sample size for equality of means, binomial proportions, and survival curves."
  })

    output$welcome_info = renderUI({HTML(paste(
      "To learn more about sample size calculations, visit these links:",
      "Hypothesis testing: https://www.youtube.com/watch?v=0oc49DyA3hU",
      "Statistical power: https://www.youtube.com/watch?v=Rsc5znwR5FA",
      "Sample size: https://www.youtube.com/watch?v=67zCIqdeXpo",
      "P-hacking and power calculations: https://www.youtube.com/watch?v=UFhJefdVCjE", sep="<br/>")
  )})
  
  #normal description
  output$mean_description = renderText({
    "This tab calculates the sample size for the test of equality of means for two normally distributed 
    populations. Both samples are assumed to have the same size. The user needs to specify the following input values: mean and standard deviation for each population, significance level,
    and power to calculate the necessary sample size."
  })
  
  #normal n output
  output$n_norm = renderText({
    n_norm = round(n_norm_reactive(), digits = 3)
    paste("Sample Size per Group: ", n_norm)
  })
  
  #reactive normal n
  n_norm_reactive = eventReactive(input$calculate, {
    zalpha = qnorm(1-input$alpha/2,0,1)
    zbeta = qnorm(input$power,0,1)
    n_norm_reactive = (((input$s1*input$s1 + input$s2*input$s2)*(zalpha+zbeta)^2)/(input$mu2-input$mu1)^2)
    n_norm_reactive
  })
  
  #plot reactive normal n
  observeEvent(input$calculate, {
    norm_data = data.frame(1:(n_norm_reactive()), rnorm(n_norm_reactive(), input$mu1, input$s1), rnorm(n_norm_reactive(), input$mu2, input$s2))
    big_norm_data = data.frame(big = 1:1000000, bignormvalues1 = rnorm(1000000, input$mu1, input$s1), bignormvalues2 = rnorm(1000000, input$mu2, input$s2))
    colnames(norm_data) = c("subjects", "normvalues1", "normvalues2")
    v$plotnorm = ggplot(, geom = 'blank') +   
      geom_line(aes(x = big_norm_data$bignormvalues1, y = ..density.., color = 'Population 1', col = "#1B9E77"), stat = 'density') +  
      geom_line(aes(x = big_norm_data$bignormvalues2, y = ..density.., color = 'Population 2', col = "#D95F02"), stat = 'density') +
      geom_histogram(aes(x = norm_data$normvalues1, y = ..density..), alpha = 0.4,fill = "#1B9E77", col = "#1B9E77") +    
      geom_histogram(aes(x = norm_data$normvalues2, y = ..density..), alpha = 0.4,fill = "#D95F02", col = "#D95F02") +
      xlab("Values") + 
      ylab("Frequency") +
      theme(panel.background = element_rect(fill = "transparent"),
            plot.background = element_rect(fill = "transparent", color = NA))+
      labs(colour = "Population", title = "Histograms for the Two Samples Based on Sample Size with Normal Densities Overlayed")
      v$text = "Bar"

  })
  
  output$plotnorm = renderPlot({
    if(is.null(v$plotnorm)) return()
    v$plotnorm
  })
  
  #binomial description
  output$bin_description = renderText({
    "This tab calculates the sample size for the two-sided test of equality of proportions for two Binomial 
    populations where the the sample size of Group 2 k times as large as the sample size of Group 1. The user needs to specify the following input values: expected proportions for each population, k,
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
  n_bin_reactive = eventReactive(input$calculate_bin,{
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
  
  output$plotbin = renderPlot({
    if(is.null(vbin$plotbin)) return()
    vbin$plotbin
  })
  
  #plot reactive binomial n
  observeEvent(input$calculate_bin, {
    success = 0:(floor(n_bin_reactive()))
    prob1 = dbinom((0:floor(n_bin_reactive())), floor(n_bin_reactive()), input$p1)
    data_bin = data.frame(success, prob1)
    success2 = 0:(floor((input$k) * (n_bin_reactive())))
    prob2 = dbinom((0:floor((input$k) * n_bin_reactive())), floor((input$k) * n_bin_reactive()), input$p2)
    data_bin2 = data.frame(success2, prob2)
    big_norm_data2 = data.frame(bignormvalues1 = rnorm(1000000, floor(n_bin_reactive())*(input$p1), sqrt(floor(n_bin_reactive()*(input$p1)*(1-input$p1)))), 
                                bignormvalues2 =  rnorm(1000000, (floor(n_bin_reactive())*(input$p2)), sqrt(floor(n_bin_reactive())*(input$p2)*(1-input$p2))))
    
    vbin$plotbin = ggplot(,  geom = 'blank') +   
      geom_line(aes(x = (big_norm_data2$bignormvalues1), y = ..density..), col = "#1B9E77", stat = 'density') +    
      geom_line(aes(x = (big_norm_data2$bignormvalues2), y = ..density..), col = "#D95F02", stat = 'density') +
      geom_col(aes(x = data_bin$success, y=data_bin$prob1),  alpha = 0.4, fill = "#1B9E77", col = "#1B9E77")  + 
      geom_col(aes(x = data_bin2$success2, y=data_bin2$prob2,  alpha = 0.4, fill = "#D95F02", col = "#D95F02"))  +
      xlab("Values") + 
      ylab("Frequency") +
      theme(panel.background = element_rect(fill = "transparent"),
            plot.background = element_rect(fill = "transparent", color = NA),
            legend.position = "none") +
      labs(colour = "Population", title = "Histograms of the Two Samples based on Sample Size with Normal Densities (used to approximate) Overlayed") 
      vbin$text = "Bar"
  })
  
  #survival description
  output$surv_description = renderText({
    "This tab calculates the sample size estimation for the comparison of survival curves
      between two groups under the Cox Proportional-Hazards Model. The ratio of participants
      in the treatment group to control is k. Input values under hazard ratio, k, significance level,
     and power to calculate the necessary sample size."
  })
  
  #survival n output
  output$n_survival =  eventReactive(input$calculate_surv,{
    zalpha = qnorm(1-input$surv_alpha/2,0,1)
    zbeta = qnorm(input$surv_power,0,1)
    m = (1/input$surv_k)*((input$k*input$hr+1)/(input$hr-1))^2*(zalpha+zbeta)^2
    n_survival = round(m/(input$surv_k*input$pT + input$pC),3)
    paste("Sample Size: ", n_survival)
  })
  
  #help description
  output$help_description = renderText({
    "Some Helpful Tips."
  })
  
  #help FAQ
  output$FAQ = renderUI({HTML(paste(
    "Frequently Asked Questions",
    "Q: How to get help?",
    "A: You should contact the lab: https://solislemuslab.github.io//pages/people.html",
    "Q: I found a bug or error in the code, how can I report it?",
    "A: You should contact the lab: https://solislemuslab.github.io//pages/people.html",
    "Q: How can I contribute?",
    "A: Visit https://github.com/solislemuslab/sample-size-shinyapp/blob/master/CONTRIBUTING.md for instructions.",
    sep="<br/>"))
  })
  
  addTooltip(session = session, id = "calculate", title = "Values entered must be greater than 0 and within the specified range."
)
  
}
shinyApp(ui = ui, server = server)

