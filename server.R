
# This is the server logic for a Shiny web application.
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

shinyServer(function(input, output) {

  
  ########################################################################################
  
  ##################INPUT GENERATORS
  
  ########################################################################################

  #Generate a drop-down selector for each variable in input$varnums
  output$ui_dropdowns <- renderUI({
    varnums <- as.integer(input$varnums)
    lapply(1:varnums, function(i) {
      selectInput(inputId = paste0("vartype", i), label = paste0("Variable ", i, ", Type:"), choices = c("Continuous", "Attribute"))
    })
  })
  
  
  #Collect user generated variable names
  output$ui_varnames <- renderUI({
    varnums <- as.integer(input$varnums)
    
    outlist <- lapply(1:varnums, function(i) {
      tags$div(title = "Accepts a continuous string of letters. Must match variable names used in formulas below.",
               textInput(inputId = paste0("varname", i), label = "Name"))
    })
    
    return(outlist)
    
  })
  
  #Collect variable levels/step sizes
  output$ui_varlevs <- renderUI({
    varnums <- as.integer(input$varnums)
    
    outlist <- lapply(1:varnums, function(i) {
      numericInput(inputId = paste0("varlevs", i), label = "Number of Levels", value = 2, min = 2, step = 1)
    })
    
    return(outlist)
    
  })
  
  #Collect variable minimums/number of attribute levels
  output$ui_varmin <- renderUI({
    varnums <- as.integer(input$varnums)

    #Define list that will contain input generators
    outlist <- list()
    
    for(i in 1:varnums){
      
      #Set input parameters for continuous variables
      if(input[[paste0("vartype", i)]] == "Continuous"){

        outlist[[i]] <- numericInput(inputId = paste0("varmin", i), label = "Min", value = 0)
        
        }
      
      #Set input parameters for attribute variables
      if(input[[paste0("vartype", i)]] == "Attribute"){

        outlist[[i]] <- tags$div(title = "Names of each variable level. Separate each level name with a comma followed by a space. E.g: Low, High",
                                 textInput(inputId = paste0("levnames", i), label = "Level Names"))
        
        }
      
    }
    
    return(outlist)
    
  })
  
  #Collect variable maximums/level name headers
  output$ui_varmax <- renderUI({
    varnums <- as.integer(input$varnums)
    
    #Define maximum level for each variable
    outlist <- lapply(1:varnums, function(i) {
      numericInput(inputId = paste0("varmax", i), label = "Max", value = 0)
    })
    
    return(outlist)
    
  })
  
  #Collect user generated model names
  output$ui_modelnames <- renderUI({
    modelnums <- as.integer(input$modelnums)
    lapply(1:modelnums, function(i) {
      tags$div(title = "This model name will be used as label on the analysis tabs",
               textInput(inputId = paste0("modelname", i), label = "Name"))
    })
  })
  
  #Collect user generated model formulas
  output$ui_modelformulas <- renderUI({
    modelnums <- as.integer(input$modelnums)
    lapply(1:modelnums, function(i) {
      tags$div(title = "The model formula. Format is A+B+A*B... Individual variable names must match variable names defined above. To combine multiple variables into one term, surround the term formula with I(term). E.g.: I(A/B)",
               textInput(inputId = paste0("modelformula", i), label = "Model Formula"))
    })
  })
  
  #Collect user generated model weights for optimization function
  output$ui_modelweights <- renderUI({
    modelnums <- as.integer(input$modelnums)
    lapply(1:modelnums, function(i) {
      tags$div(title = "Weighting percentage used when optimizing study across multiple models. Higher weight values will place more importance on this formula when compared to the other formulas.",
               textInput(inputId = paste0("modelweight", i), label = "Optimization Weight", value = 1/modelnums))
    })
  })
  
  #Generate a drop-down selector for each variable in input$varnums
  output$ui_modeltypes <- renderUI({
    modelnums <- as.integer(input$modelnums)
    lapply(1:modelnums, function(i) {
      selectInput(inputId = paste0("modeltype", i), label = paste0("Model ", i, ", Type:"), choices = c("Linear", "Choice"))
    })
  })
  
  #Collect user generated model effects
  output$ui_modeleffects <- renderUI({
    
    #Get list of effects
    effectlist <- factorgen()[["effectlist"]]

    
    #Generate list of numeric inputs; one for each effect per model
    outlist <- list()
    
    
    for(i in 1:length(effectlist)){
    
      outlist[[length(outlist)+1]] <- list(column(6,h3(paste0("Effects Sizes for Model: ", input[[paste0("modelname", i)]]))),
                                           column(6,h3(paste0("Detectable Difference for Power Calculations: ", input[[paste0("modelname", i)]]))))
      
      for(j in 1:length(effectlist[[i]])){
      
        #outlist[[length(outlist)+1]] <- numericInput(inputId = paste0("modeleffects", i,j), label = effectlist[[i]][j], value = 0)
        outlist[[length(outlist)+1]] <- list(column(6,tags$div(title = "Estimated size of the effect. Effect size is the natural log odds ratio which is calculated as the proportion of people selecting the highest setting of this variable divided by the proportion of people selecting the lowest level of this variable. If 70% of people would select the high level over the low level, the odds ratio would be ln(0.7/0.3) = 0.846",
                                                             numericInput(inputId = paste0("modeleffects", i,j), label = effectlist[[i]][j], value = 0))),
                                             column(6,tags$div(title = "The smallest effect size you would like to have enough samples to detect. A smaller value here translates to a larger required sample size",
                                                               numericInput(inputId = paste0("effectdiffdetect", i,j), label = effectlist[[i]][j], value = 0))))
                                                    
    
    }
      
    }
    return(outlist)
    
  })
  
  
  #Dropdown menu to select model name from list for correlation plot
  output$ui_corrplotselect <- renderUI({
    modelnums <- as.integer(input$modelnums)
    
     modelnames <- list()
    for(i in 1:modelnums){
      
      modelnames[[i]] <- i
      names(modelnames[[i]]) <- paste0(i,": ", input[[paste0("modelname", i)]])
      
    }
    
     
     #Generate selector input that will return model index (allows for no-name models or models with the same name without breaking program)
    selectInput(inputId = "corrmodel", label = paste0("Select Model to View Correlation Plot"), choices = unlist(modelnames))
    
  })
  
  #Dropdown menu to select model name from list for efficiency plot of multiple model search
  output$ui_effplotselect <- renderUI({
    modelnums <- as.integer(input$modelnums)
    
    modelnames <- list()
    for(i in 1:modelnums){
      
      modelnames[[i]] <- i
      names(modelnames[[i]]) <- paste0(i,": ", input[[paste0("modelname", i)]])
      
    }
    
    
    #Generate selector input that will return model index (allows for no-name models or models with the same name without breaking program)
    selectInput(inputId = "effplotmodel", label = paste0("Select Model to View Efficiency Plot"), choices = unlist(modelnames))
    
  })
  
  #Dropdown menu to select number of alternates from list for correlation plot of multiple model search
  output$ui_corrmultiplotalt <- renderUI({
    
    #Get correlation frames
    corrframes <- corrframes()
    
    altslist <- unique(corrframes$corrframelist[[as.numeric(input$effplotmodel)]]$alternatives)
    
    #Generate selector input that will return model index (allows for no-name models or models with the same name without breaking program)
    selectInput(inputId = "corrmultiplotalt", label = paste0("Select Number of Alternatives to Display for Correlation Plot"), choices = altslist)
    
  })
  
  
  ########################################################################################
  
  ##################OUTPUT GENERATORS
  
  ########################################################################################
  
  
  #Generate covariance plot
  output$out_corrplot <- renderPlot({

    # generate correlation matrix
    corrmat <- cov2cor(modeluserspec()[["CovList"]][[as.numeric(input$corrmodel)]])
    
    #Pull effects list from factorgen reactive function and use as correlation matrix names
    effectlist <- factorgen()[["effectlist"]][[as.numeric(input$corrmodel)]]
    colnames(corrmat)[(ncol(corrmat)-length(effectlist)+1):ncol(corrmat)] <- effectlist
    rownames(corrmat)[(nrow(corrmat)-length(effectlist)+1):nrow(corrmat)] <- effectlist
    
    #Set column names and row names based on factor names equal to row names
    #colnames(corrmat) <- rownames(corrmat)
    
    #generate correlation plot using corr
    corrplot.mixed(corrmat)

  })
  
  #Print model objective function
  output$out_diagnostics <- renderText({
    
    #Print overall optimization function
    return(paste0("Overall Optmization Function Result: ",  modeluserspec()[["ObjectiveFunction"]]))
    
  })
  
  output$out_formuladetails <- renderTable({
    
    modelout <- modeluserspec()
    
    #Get list of input formulas
    formulalist <- sapply(paste0("modelformula", seq(from = 1, to = input$modelnums)), function(i) {
      
      paste0(input[[i]])
    })
    
    #Get list of formula names
    formulanames <- sapply(paste0("modelname", seq(from = 1, to = input$modelnums)), function(i) {
      
      paste0(input[[i]])
    })
    
    #Get model weights
    weightlist <- sapply(paste0("modelweight", seq(from = 1, to = input$modelnums)), function(i) {
      
      as.numeric(input[[i]])
    })
    
    #Create table of formula specific diagnostics
    
    tableout <- data.frame(formulanames, formulalist, modelout$DeffvsOptim, modelout$DeffList, weightlist)
    
    colnames(tableout) <- c("Formula Name", "Formula", "D-Efficiency.Vs.Optimal", "D-Efficiency", "Optimization Weight")
    
    return(tableout)
    
  })
  
  #Return table of sample sizes
  output$out_samplesizesingle <- renderTable({
    
    singlecorrframe <- singlecorrframe()
    
    modelprep <- modelprep()
    
    output <- data.frame()
    
    for(i in 1:length(singlecorrframe$samplesizeframelist)){
    
      temp1 <- singlecorrframe$samplesizeframelist[[i]]
      
      temp1$Formula <- modelprep$formulanames[[i]]
      
      output<- rbind.data.frame(output, temp1)
    
    }
    
    return(output)
    
  })
  
  #Return model matrix
  output$out_modelmatrix <- renderTable({
    
    #Return Model Table
    modelmat <- modeluserspec()[["ModelFrame"]]
    
  })
  
  #Code to download model matrix
  output$ModelSave<-downloadHandler(
    filename = function(){paste(input$ModelSaveName,".csv",sep = "")},
    content = function(file){
      modelmat <- modeluserspec()[["ModelFrame"]]
      #saved_api <- reactiveValuesToList(tmp)
      write.csv(modelmat, file = file)
    })
  
  
  #Contour plot for design efficiencies for model search
  output$out_effcontour_multi <- renderPlotly({
    
    
    
    modelprep <- modelprep()
    
    modelout <- manymodelsearch()
    
    #Get list of input formulas
    formulalist <- paste(modelprep$formulalist)
    
    #Get list of formula names
    formulanames <- unlist(modelprep$formulanames)
    
    #Get model weights
    weightlist <- modelprep$weightvect
    
    #Create plot data.frame
    plot.df <- data.frame(modelout$DesignParamFrame, modelout$DeffFrame, modelout$DeffvsOptim)
    
    #colnames(plot.df) <- c(colnames(modelout$DesignParamFrame), c(paste0(formulanames, "Defficiency"), paste0(formulanames, ":D-Optimality")))
    
    plot.df <- plot.df[,c(1,2,(2+as.numeric(input$effplotmodel)))]
    colnames(plot.df)[3] <- "Defficiency"
    
    plot_ly(plot.df,
            x = as.formula(paste0("~",colnames(plot.df)[1])),
            y = as.formula(paste0("~",colnames(plot.df)[2])),
            z = as.formula(paste0("~",colnames(plot.df)[3])),
            type = "contour")
    
  })
  
  #Correlation plot for model search
  output$out_effcorr_multi <- renderPlotly({
    
    
    
    #modelprep <- modelprep()
    
    #modelout <- manymodelsearch()
    
    #Get correlation frames
    corrframes <- corrframes()
    
#     #Get list of input formulas
#     formulalist <- paste(modelprep$formulalist)
#     
#     #Get list of formula names
#     formulanames <- unlist(modelprep$formulanames)
#     
#     #Get model weights
#     weightlist <- modelprep$weightvect
    
    plot.df <- corrframes$corrframelist[[as.numeric(input$effplotmodel)]]
    
    plot.df <- reshape2::melt(plot.df[which(plot.df$alternatives == as.numeric(input$corrmultiplotalt)),-c(which(colnames(plot.df) == "alternatives"))], id.vars = c("questions"), value.name = "correlation")
    
    plot_ly(plot.df,
            x = ~questions,
            y = ~correlation,
            type = "scatter",
            mode = "lines",
            linetype = ~variable)
    
  })
  
  #Sample size plot for model search
  output$out_samplesize_multi <- renderPlotly({
    
    #Get correlation frames
    corrframes <- corrframes()
    
    plot.df <- corrframes$samplesizeframelist[[as.numeric(input$effplotmodel)]]
    
    plot.df <- reshape2::melt(plot.df[which(plot.df$alternatives == as.numeric(input$corrmultiplotalt)),-c(which(colnames(plot.df) == "alternatives"))], id.vars = c("questions"), value.name = "samples_required")
    
    plot_ly(plot.df,
            x = ~questions,
            y = ~samples_required,
            type = "scatter",
            mode = "lines",
            linetype = ~variable)
    
  })
  

  #Print details for multi formula optimizer
  output$out_formuladetails_multi <- renderTable({
    
    
    
    modelprep <- modelprep()
    
    modelout <- manymodelsearch()
    
    #Get list of input formulas
    formulalist <- paste(modelprep$formulalist)
    
    #Get list of formula names
    formulanames <- unlist(modelprep$formulanames)
    
    #Get model weights
    weightlist <- modelprep$weightvect
    
    #Create table of formula specific diagnostics
    
    tableout <- data.frame(modelout$DesignParamFrame, modelout$DeffFrame, modelout$DeffvsOptim)
    
    colnames(tableout) <- c(colnames(modelout$DesignParamFrame), c(paste0(formulanames, ":D-efficiency"), paste0(formulanames, ":D-Optimality")))
      
    return(tableout)
    
  })
  
  
  ########################################################################################
  
  ##################MODEL KERNEL CODE
  
  ########################################################################################
  
  
  #Factor level generator - separate function to allow factor names to be consistant across functions. Generates a list of factor names for each model
  factorgen <- reactive({
    
    #Get factor types and create single line data frame to generate model matrix
    ##Old line of code, would cause values to be cleared every time a new input was initialized since this reactive statement is used in input generators above
    #factortypes <- sapply(grep("vartype", names(input), value = TRUE), function(i){input[[i]]})
    factortypes <- sapply(paste0("vartype", c(1:input$varnums)), function(i){input[[i]]})
    
    factorframe <- data.frame(matrix(nrow = 1, ncol = length(factortypes), data = NA))
    ##Old line of code, would cause values to be cleared every time a new input was initialized since this reactive statement is used in input generators above
    #colnames(factorframe) <- sapply(grep("varname", names(input), value = TRUE), function(i){input[[i]]})
    colnames(factorframe) <- sapply(paste0("varname", c(1:input$varnums)), function(i){input[[i]]})
    
    
    
    #Populate data frame with numeric value for continuous values or a single factor level for attribute predictors
    for(i in 1:ncol(factorframe)){
      
      if(factortypes[i] == "Continuous"){factorframe[[i]] <- 1}
      if(factortypes[i] == "Attribute"){
        levellist <- paste0(input[[paste0("varname",i)]], seq(from = 1, to = input[[paste0("varlevs", i)]], by = 1), "_", unlist(strsplit(input[[paste0("levnames",i)]], split = ", ")))
        factorframe[[i]] <- factor(levellist[1], levels = levellist)
        
        #Set reference level to last factor level to match the naming convention used for contr.sum since contr.sum is used later
        contrasts(factorframe[[i]]) <- contr.treatment(levels(factorframe[[i]]), base=length(levels(factorframe[[i]])))}
    }
    
    #Get list of input formulas
    formulalist <- lapply(paste0("modelformula", seq(from = 1, to = input$modelnums)), function(i) {

      as.formula(paste0("~",input[[i]]))
    })
    
    #Create model matrix for each formula and retrieve effect names
    effectlist <- lapply(formulalist, function(i) {
      colnames(model.matrix(i, factorframe))[-1]
    })
    
    return(list("effectlist" = effectlist, "factorframe" =  factorframe))
    
  })
  
  
  #Model prep based on user parameters from first input page. These parameters do not vary based on model size, etc.
  modelprep <- reactive({
    
    #Pull factor levels from factorgen reactive function
    factorframe <- factorgen()[["factorframe"]]
    
    #Pull effects list from factorgen reactive function
    effectlist <- factorgen()[["effectlist"]]
    
    #       #Process formulas into list
    #       formulalist <- lapply(grep("modelformula", names(input), value = TRUE), function(i) {
    #         as.formula(paste0("~",input[[i]]))
    #       })
    
    #Get list of input formulas
    formulalist <- lapply(paste0("modelformula", seq(from = 1, to = input$modelnums)), function(i) {
      
      as.formula(paste0("~",input[[i]]))
    })
    
    #Get list of formula names
    formulanames <- lapply(paste0("modelname", seq(from = 1, to = input$modelnums)), function(i) {
      
      paste0(input[[i]])
    })
    
    #Collect input formula weights
    weightvect <- as.numeric(sapply(grep("modelweight", names(input), value = TRUE), function(i){input[[i]]}))
    
    #Get vector of formula types
    formulatypes <- sapply(paste0("modeltype", seq(from = 1, to = input$modelnums)), function(i) {
      
      paste0(input[[i]])
    })
    
    priorlist <- list()
    #Collect prior weights
    for(i in 1:length(formulalist)){
      
      priorlist[[i]] <- sapply(grep(paste0("modeleffects",i), names(input), value = TRUE), function(i){input[[i]]})[1:length(effectlist[[i]])]
      names(priorlist[[i]]) <- effectlist[[i]]
    }
    
    powerdifflist <- list()
    #Collect differences to be used for power calculations
    for(i in 1:length(formulalist)){
      
      powerdifflist[[i]] <- sapply(grep(paste0("effectdiffdetect",i), names(input), value = TRUE), function(i){input[[i]]})[1:length(effectlist[[i]])]
      names(powerdifflist[[i]]) <- effectlist[[i]]
    }
    
    #Create data frame of input ranges. Attribute variables will be coded as "NA" for min and max values
    looptargets <- grep("varname", names(input), value = TRUE)[1:input$varnums]
    
    #First loop to use for varlevs
    inputrange <- data.frame(sapply(c(1:length(looptargets)), function(i){if(input[[paste0("vartype",i)]] == "Continuous"){
      c(input[[paste0("varmin", i)]], input[[paste0("varmax",i)]])}else{c(-1,1)}}))
    
    colnames(inputrange) <- sapply(looptargets, function(i){input[[i]]})
    
    #Get levels of each input variable
    varlevs <- lapply(c(1:length(looptargets)), function(i){if(input[[paste0("vartype",i)]] == "Continuous"){
      seq(from = as.numeric(input[[paste0("varmin", i)]]), to = as.numeric(input[[paste0("varmax", i)]]), length.out = as.numeric(input[[paste0("varlevs",i)]]))}
      
      if(input[[paste0("vartype",i)]] == "Continuous"){
        
        outvect <- seq(from = inputrange[1,i], to = inputrange[2,i], by = (inputrange[2,i] - inputrange[1,i])/(input[[paste0("varlevs",i)]]-1))
        
        #as.factor(paste0(input[[paste0("levnames",i)]], seq(from = 1, to = as.numeric(input[[paste0("varlevs",i)]]), by = 1)))
      }
      
      if(input[[paste0("vartype",i)]] == "Attribute"){
        
        outvect <- levels(factorframe[[i]])
        
        #as.factor(paste0(input[[paste0("levnames",i)]], seq(from = 1, to = as.numeric(input[[paste0("varlevs",i)]]), by = 1)))
      }
      
      return(outvect)
      
    })
    names(varlevs) <- sapply(looptargets, function(i){input[[i]]})
    
    #Convert factor inputrange columns to actual factor levels
    for(i in 1:ncol(inputrange)){
      
      if(is.numeric(varlevs[[colnames(inputrange)[i]]]) == FALSE){
        
        inputrange[i] <- c(min(levels(factorframe[[i]])),max(levels(factorframe[[i]])))
        
      }
      
    }
    
    return(list("inputrange" = inputrange, "formulalist" = formulalist, "formulanames" = formulanames, "priorlist" = priorlist, "powerdifflist" = powerdifflist, "weightvect" = weightvect,"formulatypes" = formulatypes, "varlevs" = varlevs))
    
  })
  
  #Main model constructor for user specified parameters
  modeluserspec <- reactive({
    
    #React to action button. Add additional conditions here. Perhaps use "validate" function
    if(input$createsingledesignbutton==0){
    
      return(list())
      
  }else{
    #Isolate main modeling loop to prevent code from running unless run model button is pressed
    isolate({

      #Disable design creation buttons while code runs
      disable("createsingledesignbutton")
      disable("searchdesignsbutton")
      
      #Get model prep details
      modelprep <- modelprep()
      
      #Create placeholder list of determinants
      determrefs <- rep(1, length(modelprep$formulalist))
      
      #Fedorov-specific implementation details
      if(input$searchstrat == "Fedorov"){candset <- expand.grid(modelprep$varlevs)}else{candset <- NA}
        #Run through each base variable and expand the model matrix base
        
        #Create stacked diagonals matrix for a few random starts to find baseline d-efficiency values to use
        randommat <- matrix(nrow = length(modelprep$weightvect)*input$randomstartsbaseline,
                            ncol = length(modelprep$weightvect),
                            data = c(diag(length(modelprep$weightvect))),
                            byrow = TRUE)
        #Initial pass through model focusing on one formula only each time to establish optimal d-efficiencies
        optmodel <- apply(randommat, MARGIN = 1, MultipleModelOptimize,
                          base_input_range = modelprep$inputrange,
                          formulalist = modelprep$formulalist,
                          questions = input$modelquestions,
                          alts = input$alternatives,
                          blocks = input$blocks,
                          optout = input$optout,
                          det_ref_list = determrefs,
                          candset = candset,
                          priors = modelprep$priorlist,
                          mesh = modelprep$varlevs,
                          tolerance = input$tolerancebaseline,
                          searchstyle = input$searchstrat,
                          eqtype = modelprep$formulatypes)
#         #########Old code###
#         #Initial pass through model focusing on one formula only each time to establish optimal d-efficiencies
#         optmodel <- apply(randommat, MARGIN = 1, DiscChoiceMultipleModel,
#                            base_input_range = modelprep$inputrange,
#                            formulalist = modelprep$formulalist,
#                            questions = input$modelquestions,
#                            alts = input$alternatives,
#                            blocks = input$blocks,
#                            optout = input$optout,
#                            det_ref_list = determrefs,
#                            candset = candset,
#                            priors = modelprep$priorlist,
#                            mesh = modelprep$varlevs,
#                            tolerance = input$tolerancebaseline,
#                            searchstyle = input$searchstrat)
#         ########Old code######
        
        #Find maximum d-efficiencies to use as reference designs
        if(length(modelprep$formulalist) > 1){
          #Script for more than one formula
          determrefs <- apply(sapply(optmodel, "[[", "Deff"), MARGIN = 1, max)
          
          #Script for one formula
        }else{determrefs <- max(sapply(optmodel, "[[", "Deff"))}
        
        #Set zero determinants (from an insufficient model size) to a small positive number to allow for calculations to continue
        determrefs[determrefs == 0] <- 0.001
        
        #expand weight vector to matrix by number of random starts
        weight <- matrix(nrow = input$randomstarts, ncol = length(modelprep$weightvect), data = modelprep$weightvect, byrow = TRUE)
        
        #Full optimization pass using optimal d-efficiencies
        optmodel <- apply(weight, MARGIN = 1, MultipleModelOptimize,
                          base_input_range = modelprep$inputrange,
                          formulalist = modelprep$formulalist,
                          questions = input$modelquestions,
                          alts = input$alternatives,
                          blocks = input$blocks,
                          optout = input$optout,
                          det_ref_list = determrefs,
                          candset = candset,
                          priors = modelprep$priorlist,
                          mesh = modelprep$varlevs,
                          tolerance = input$tolerance,
                          searchstyle = input$searchstrat,
                          eqtype = modelprep$formulatypes)
        
        #Select best model
        optmodel <- optmodel[[which.max(sapply(optmodel, "[[", "ObjectiveFunction"))]]
      
        #Enable design creation buttons while code runs
        enable("createsingledesignbutton")
        enable("searchdesignsbutton")
        
    })
    
  }
    
    return(list("ModelFrame" = optmodel$ModelMatrix,
                "CovList" = optmodel$CovList,
                "DeffList" = optmodel$Deff,
                "DeffvsOptim" = optmodel$DeffvsOptimal,
                "ObjectiveFunction" = optmodel$ObjectiveFunction))
    
  })

  
  #Main model constructor for user specified parameters
  manymodelsearch <- reactive({
    
    #React to action button. Add additional conditions here. Perhaps use "validate" function
    if(input$searchdesignsbutton==0){
      
      return(list())
      
    }else{
      #Isolate main modeling loop to prevent code from running unless run model button is pressed
      isolate({
        
        #Disable design creation buttons while code runs
        disable("createsingledesignbutton")
        disable("searchdesignsbutton")
        
        #Get model prep details
        modelprep <- modelprep()
        
        questionvect <- as.numeric(unlist(strsplit(input$questionnumlist, split = ", ")))
        altvect <- as.numeric(unlist(strsplit(input$altnumlist, split = ", ")))
        
#         searchparams <- expand.grid(questions = seq(from = input$minmodelquestions, to = input$maxmodelquestions, by = 1),
#                                     alternatives = seq(from = input$minalternatives, to = input$maxalternatives, by = 1))
        
        searchparams <- expand.grid(questions = questionvect,
                                    alternatives = altvect)
        
        #Find the best model for each 
        
        modellist <- list()
        
        #Fedorov-specific implementation details
        if(input$searchstratmulti == "Fedorov"){candset <- expand.grid(modelprep$varlevs)}else{candset <- NA}
        
        for(i in 1:nrow(searchparams)){
        
        #Create placeholder list of determinants
        determrefs <- rep(1, length(modelprep$formulalist))
        
        #Create stacked diagonals matrix for a few random starts to find baseline d-efficiency values to use
          randommat <- matrix(nrow = length(modelprep$weightvect)*input$randomstartsbaseline,
                              ncol = length(modelprep$weightvect),
                              data = c(diag(length(modelprep$weightvect))),
                              byrow = TRUE)
          
          #Initial pass through model focusing on one formula only each time to establish optimal d-efficiencies
          optmodel <- try(apply(randommat, MARGIN = 1, MultipleModelOptimize,
                            base_input_range = modelprep$inputrange,
                            formulalist = modelprep$formulalist,
                            questions = searchparams$questions[i],
                            alts = searchparams$alternatives[i],
                            blocks = input$blocksmulti,
                            optout = input$optoutmulti,
                            det_ref_list = determrefs,
                            candset = candset,
                            priors = modelprep$priorlist,
                            mesh = modelprep$varlevs,
                            tolerance = input$tolerancebaseline,
                            searchstyle = input$searchstratmulti,
                            eqtype = modelprep$formulatypes))
          
          #Find maximum d-efficiencies to use as reference designs
          if(length(modelprep$formulalist) > 1){
            #Script for more than one formula
            determrefs <- apply(sapply(optmodel, "[[", "Deff"), MARGIN = 1, max)
            
            #Script for one formula
          }else{determrefs <- max(sapply(optmodel, "[[", "Deff"))}
          
          #Set zero determinants (from an insufficient model size) to a small positive number to allow for calculations to continue
          determrefs[determrefs == 0] <- 0.001
          
          #expand weight vector to matrix by number of random starts
          weight <- matrix(nrow = input$randomstartsmulti, ncol = length(modelprep$weightvect), data = modelprep$weightvect, byrow = TRUE)
          
          #Full optimization pass using optimal d-efficiencies
          optmodel <- try(apply(weight, MARGIN = 1, MultipleModelOptimize,
                            base_input_range = modelprep$inputrange,
                            formulalist = modelprep$formulalist,
                            questions = searchparams$questions[i],
                            alts = searchparams$alternatives[i],
                            blocks = input$blocksmulti,
                            optout = input$optoutmulti,
                            det_ref_list = determrefs,
                            candset = candset,
                            priors = modelprep$priorlist,
                            mesh = modelprep$varlevs,
                            tolerance = input$tolerance,
                            searchstyle = input$searchstratmulti,
                            eqtype = modelprep$formulatypes))
          
          #Select best model
          optmodel <- optmodel[[which.max(sapply(optmodel, "[[", "ObjectiveFunction"))]]
        
        modellist[[i]] <- optmodel
        
        }
        
        #Enable design creation buttons while code runs
        enable("createsingledesignbutton")
        enable("searchdesignsbutton")
        
      })
      
    }
    
    return(list("DesignParamFrame" = searchparams,
                "ModelFrameLists" = lapply(modellist, "[[", "ModelMatrix"),
                "CovLists" = lapply(modellist, "[[", "CovList"),
                "DeffFrame" = if(length(modelprep$formulalist) > 1){t(sapply(modellist, "[[", "Deff"))}else{(sapply(modellist, "[[", "Deff"))},
                "DeffvsOptimFrame" = if(length(modelprep$formulalist) > 1){t(sapply(modellist, "[[", "DeffvsOptimal"))}else{(sapply(modellist, "[[", "DeffvsOptimal"))},
                "ObjectiveFunctionVector" = sapply(modellist, "[[", "ObjectiveFunction")))
    
  })
    


  #Reactive statement to create correlation frame and sample size for single model search
  singlecorrframe <- reactive({
    
    modelprep <- modelprep()
    
    modelout <- modeluserspec()
    
    corrframelist <- list()
    samplesizeframelist <- list()
    
    #Loop through each formula
    for(i in 1:length(modelout$CovList)){
      
      #Get covariance matrix for only one model
      covlist <- modelout$CovList[[i]]
      
      #Non intercept row and column ids
      nonintids <- (rownames(covlist) != "(Intercept)")
      
      #Calculate sample sizes based on supplied power values (ignore intercept for these calculations)
      samplesizeframelist[[i]] <- vcovpower(vcovmat = covlist[nonintids, nonintids], detectdiff = modelprep$powerdifflist[[i]])
      
      #Translate covariance matrix to correlation matrix
      corrframelist[[i]] <- cov2cor(covlist)
      
    }
    
    return(list("corrframelist" = corrframelist, "samplesizeframelist" = samplesizeframelist))
    
  })
  

#Reactive statement to create correlation frame and sample size for multiple model search
corrframes <- reactive({
  
  modelprep <- modelprep()
  
  modelout <- manymodelsearch()
  
  
  corrframelist <- list()
  samplesizeframelist <- list()
  
  #Loop through each formula
  for(i in 1:length(modelout$CovLists[[1]])){
  
    #Get covariance matrix for only one model
    covlist <- lapply(modelout$CovLists, "[", i)
    covlist <- unlist(covlist, recursive = FALSE)
    
    #Remove intercept for power calcs
    covpower <- lapply(covlist, function(x) x[which(rownames(x) != "(Intercept)"),which(rownames(x) != "(Intercept)")])
    
    #Calculate power for each covariance matrix
    samplesizeframe <- data.frame(modelout$DesignParamFrame)
      
      #Calculate sample sizes based on supplied power values
      samplesizeframe[rownames(covpower[[1]])] <- t(data.frame(sapply(lapply(covpower, vcovpower, detectdiff = modelprep$powerdifflist[[i]]), "[", "Minimum Sample Size")))
      
    
  #Translate covariance matrix to correlation matrix
  
  corrlist <- lapply(covlist, cov2cor)
  
  #Collapse all covariance matrices into data frame with one column for each correlation interaction
  
  corrframe <- data.frame(modelout$DesignParamFrame)
  
  #Loop through correlation matrix list to translate into plottable data frame
  for(j in 1:(nrow(corrlist[[1]])-1)){
    
    for(k in (j+1):ncol(corrlist[[1]])){
      
      #Translate correlations to plotting friendly data frame
      corrframe[paste0(rownames(corrlist[[1]])[c(j,k)], collapse = ":")] <- sapply(corrlist, "[", j,k)
      
    }
    
  }
  
  samplesizeframelist[[i]] <- samplesizeframe
  corrframelist[[i]] <- corrframe
  
  }
  
  return(list("corrframelist" = corrframelist, "samplesizeframelist" = samplesizeframelist))
  
})

})