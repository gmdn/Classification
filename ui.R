
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

#library(shiny)

source("loadDataUI.R")

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Gamify Classification"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      
      fluidRow(
        column(6, textInput(inputId = "username",
                            label = "Username",
                            value = "user")),
        column(6, "")
      ),
      
      
      fluidRow(
        column(6, uiOutput(outputId = "startGame")),    
        column(6, actionButton(inputId = "nextCategory",
                               label   = "Next Category",
                               class = "btn-info",
                               width = "100%"))
      ),
      
      br(),
      
      verbatimTextOutput("resourcesAvailable"),
      
      br(),
      
      fluidRow(
        column(6, uiOutput(outputId = "addTraining")),
        column(6, uiOutput(outputId = "addValidation"))
      ),
      
      br(),
      
      verbatimTextOutput("clicksAvailable"),
      
#       fluidRow(
#         column(6, verbatimTextOutput("trainingInfo")),
#         column(6, verbatimTextOutput("validationInfo"))
#       ),
      
      br(),
      
      fluidRow(
        column(6, checkboxInput(inputId = "showPositive",
                                label = "Positive",
                                value = TRUE)),
        column(6, checkboxInput(inputId = "showNegative",
                                label = "Negative",
                                value = TRUE))
      ),

      br(),
      
      fluidRow(
        column(2, actionButton(inputId = "decreaseA",
                               label   ="-")),
        column(8, sliderInput(inputId = "intercept",
                              label   = "Parameter A",
                              min     = -50,
                              max     = 50,
                              value   = 0,
                              step    = 1)),
        column(2, actionButton(inputId = "increaseA",
                               label   ="+"))
      ),
      
      fluidRow(
        column(2, actionButton(inputId = "decreaseB",
                               label   ="-")),
        column(8, sliderInput(inputId = "slope",
                              label   = "Parameter B",
                              min     = 0.7,
                              max     = 2.2,
                              value   = 1.5,
                              step    = 0.01)),
        column(2, actionButton(inputId = "increaseB",
                               label   ="+"))
      ),
      
      br(),
      
      fluidRow(
        column(6, actionButton(inputId = "best",
                               label   = "Best",
                               class = "btn-info",
                               width = "100%")),
        column(6, uiOutput(outputId = "testModel"))
      )
      
    ), #sidebarPanel
    
    # Show a plot of the generated distribution
    mainPanel(

      plotOutput("coordinatesValidation", click = "plot_click"),

      #verbatimTextOutput("validationPerformance"),
      #verbatimTextOutput("testPerformance"),
      
      plotOutput("resourcesSpent")
      
    ) #mainPanel
  ) #sidebarLayout
))
