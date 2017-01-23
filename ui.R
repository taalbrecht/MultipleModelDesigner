
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(nlme)
library(MultiEqOptimizer)
library(corrplot)
library(plotly)
library(shinyjs)

shinyUI(fluidPage(
  
  #Use shinyjs to allow for advanced UI functionality
  useShinyjs(),

  # Application title
  titlePanel("Discrete Choice Experiment Planner"),
    # Show a plot of the generated distribution
  tabsetPanel(  
  tabPanel("Design Basics",
      
      #Numeric input that defines the number of basic variables
      numericInput(inputId = "varnums",
                   label = "Define the Number of Variables:",
                   value = 1,
                   min = 1),
      
      h3("Define Input Variable Characteristics"),
      # uiOutput("ui_varparams"),
      column(2,uiOutput("ui_dropdowns")),
      column(3,uiOutput("ui_varnames")),
      column(3,uiOutput("ui_varlevs")),
      column(2,uiOutput("ui_varmin")),
      column(2,uiOutput("ui_varmax")),
      
      hr(),
      h3("Specify the Models"),
      
      #Numeric input that defines the number of basic variables
      numericInput(inputId = "modelnums",
                   label = "Define the Number of Different Models to Use:",
                   value = 1,
                   min = 1),
      
      column(3,uiOutput("ui_modelnames")),
      column(3,uiOutput("ui_modelformulas")),
      column(3,uiOutput("ui_modelweights")),
      column(3,uiOutput("ui_modeltypes")),
      
      hr(),
      
      h3("Estimate Effect Size for Each Model"),
      
      uiOutput("ui_modeleffects")
      
    ),
  tabPanel("Single Design Optimization",
           
           #Sidebar for optimization controls
           sidebarPanel(
             
             #Options for Basic Model Creation
             h3("Basic Model Creation"),
             
             #Specify the number of model points
             numericInput(inputId = "modelquestions", label = "Number of Questions Per Survey", value = 1, min = 1),
             
             #Specify the number of alternatives per question
             numericInput(inputId = "alternatives", label = "Number of Choices Per Question", value = 2, min = 1),
             
             #Specify whether an opt-out should be included
             checkboxInput(inputId = "optout", label = "Include Opt-Out Alternative (i.e. 'None of the Above')", value = FALSE),
             
             #Specify number of blocks
             tags$div(title = "Block the survey into a number of different surveys. This is useful if you need a large number of different questions that exceeds the number of questions that can be reliably answered by a single respondant.",
                      numericInput(inputId = "blocks", label = "Number of Different Surveys", value = 1, min = 1)),
           
             #Specify model search procedure.
             tags$div(title = "The model searching strategy. Fedorov may take longer than columnwise for a large number of variables but typically finds more efficient designs.",
                      selectInput(inputId = "searchstrat", label = "Model Searching Strategy", choices = c("Fedorov", "Columnwise"), selected = "Fedorov")),
             
             #Specify number of random starts
             tags$div(title = "Increasing the number of random starts helps the optimizer avoid local minima and maxima.",
                      numericInput(inputId = "randomstarts", label = "Number of Random Starts to Find Design", value = 3, min = 1)),
             
             #Action button to run design
             actionButton(inputId = "createsingledesignbutton", label = "Create Design"),
             
             

             #Options for Advanced Model Creation
             h3("Advanced Options (May Leave Unchanged)"),
             
             #Specify number of random starts
             numericInput(inputId = "randomstartsbaseline", label = "Number of Random Starts to find Optimal Reference Designs", value = 3, min = 1),
             
             #Specify tolerance for reference design optimization
             numericInput(inputId = "tolerancebaseline", label = "Optimizer Tolerance for Optimal Reference Designs", value = 0.1),

             #Specify tolerance for joint model optimization
             numericInput(inputId = "tolerance", label = "Optimizer Tolerance for Design Optimization", value = 0.001)
             
           ),
           
           #Main panel to contain design outputs
           mainPanel(
             
             
             
             #Display correlation plot selector of variables for each model
             tags$div(title = "Select the formula to show the corresponding correlation plot for the predictor variables. Correlations closer to 0 are better.",
                      uiOutput("ui_corrplotselect")),
             
             #Display correlation plot
             plotOutput("out_corrplot"),
             
             #Display model efficiencies
             hr(),
             h3("Model Diagnostics and Efficiencies"),
             textOutput("out_diagnostics"),
             tableOutput("out_formuladetails"),
             
             #Display power and sample size table
             hr(),
             tags$div(title = "This table lists the required number of respondants to get the power desired to estimate model effects.",
                      h3("Power and Required Sample Sizes")),
             tableOutput("out_samplesizesingle"),
             
             #Display final model matrix
             hr(),
             tags$div(title = "Save model matrix as csv file",
                      textInput("ModelSaveName","File Name For Model Matrix:",value="ModelSaveName")),
             tags$div(title = "Save model matrix as csv file",
                      downloadButton("ModelSave","Download Model Matrix")),
             tags$div(title = "This table shows the final survey design to use. Question number is shown in the first column. Randomization of question order when administering the survey is typically recommended.",
                      h3("Design Matrix")),
             tableOutput("out_modelmatrix")
             
           )
           
  ),
  tabPanel("Multiple Design Search",
           
           #Sidebar for optimization controls
           sidebarPanel(
             
             #Options for searching across several design sizes
             
             h3("Search Across Different Model Sizes"),
             
             textInput(inputId = paste0("questionnumlist"), label = "List of Number of Questions Per Survey. Separate Multiple Values With a Comma and Space"),
             
             
             textInput(inputId = paste0("altnumlist"), label = "List of Number of Choices Per Question. Separate Multiple Values With a Comma and Space"),
             
             #Specify whether an opt-out should be included
             checkboxInput(inputId = "optoutmulti", label = "Include Opt-Out Alternative (i.e. 'None of the Above')", value = FALSE),
             
             #Specify number of blocks
tags$div(title = "Block the survey into a number of different surveys. This is useful if you need a large number of different questions that exceeds the number of questions that can be reliably answered by a single respondant.",
         numericInput(inputId = "blocksmulti", label = "Number of Different Surveys", value = 1, min = 1)),
             
             #Specify model search procedure. Add Gibbs later once it has been implemented
tags$div(title = "The model searching strategy. Fedorov may take longer than Gibbs for a large number of variables but typically finds more efficient designs.",
         selectInput(inputId = "searchstratmulti", label = "Model Searching Strategy", choices = c("Fedorov", "Columnwise"), selected = "Fedorov")),
             
             #Specify number of random starts
tags$div(title = "Increasing the number of random starts helps the optimizer avoid local minima and maxima.",
         numericInput(inputId = "randomstartsmulti", label = "Number of Random Starts to Find Design", value = 3, min = 1)),
             
             #Action button to run design
             actionButton(inputId = "searchdesignsbutton", label = "Search for Designs"),
             
             #Options for Advanced Model Creation
             h3("Advanced Options (May Leave Unchanged)"),
             
             #Specify number of random starts
             numericInput(inputId = "randomstartsbaseline", label = "Number of Random Starts to find Optimal Reference Designs", value = 3, min = 1),
             
             #Specify tolerance for reference design optimization
             numericInput(inputId = "tolerancebaseline", label = "Optimizer Tolerance for Optimal Reference Designs", value = 0.1),
             
             #Specify tolerance for joint model optimization
             numericInput(inputId = "tolerance", label = "Optimizer Tolerance for Design Optimization", value = 0.001)
             
           ),
           
           #Main panel to contain design outputs
           mainPanel(
             
             #Display correlation plot selector of variables for each model
             tags$div(title = "Select the formula to use for all of the plots and diagnostics below.",
                      uiOutput("ui_effplotselect")),
             
             #Display model efficiency plot
             hr(),
             tags$div(title = "Model d-efficiency. Higher numbers are better.",
                      h3("Model Efficiency Plot")),
             plotlyOutput("out_effcontour_multi", height = "600px"),
             
             #Display correlation plot number of alternatives selector
             uiOutput("ui_corrmultiplotalt"),
             
             #Display correlation plot based on selected formula and number of alternatives
             tags$div(title = "Correlations closer to 0 are better.",
                      h3("Correlation of Variables")),
             plotlyOutput("out_effcorr_multi", height = "600px"),
             
             #Display number of required samples based on selected formula and number of alternatives
             h3("Required Sample Sizes to Detect Specified Difference"),
             plotlyOutput("out_samplesize_multi", height = "600px"),
             
             #Display model efficiency table
             hr(),
             h3("Model Efficiencies"),
             tableOutput("out_formuladetails_multi")
             
  )
  )
)))
