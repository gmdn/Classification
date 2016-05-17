
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)
library(grid)
library(rlist)
library(RCurl)
#library(reshape2)
source("nbEstimate.R")
source("nbClassify.R")
source("loadData.R")
source("helpers.R")


shinyServer(function(input, output, session) {
  
  ## Generate seed.
  setSeed <- runif(n = 1, min = 0, max = .Machine$integer.max)
  
  ## List of actions to be saved for analysis.
  actionsSaved <- list(initSeed = setSeed, ts = Sys.time())
  
  ## vector of class names
  classNames <- c("acq", "corn", "crude", "earn", "grain",
                  "interest", "money-fx", "ship", "trade", "wheat")
  
  ## vector of classes (from the easiest to the most difficult)
  classes <- c(4, 1, 5, 3, 7, 8, 10, 6, 9, 2)
  
  ## F1 goals per class.
  goalsF1 <- c(0.950, 0.600, 0.850, 0.950, 0.900,
               0.700, 0.750, 0.750, 0.650, 0.750)
  
  ## Index of current class.
  indexCurrentClass <- 1
  
  ## Set class (temporarily)
  currentClass <- classes[indexCurrentClass]
  
  ## Cost for training.
  trainingCost <- 10 #as.numeric(input$costTraining)
  
  ## Cost for validation.
  validationCost <- 10 #as.numeric(input$costValidation)
  
  ## Cost for test.
  testCost <- 0 #as.numeric(input$costTest)
  
  ## Number of clicks (training/validation) available.
  clicks <- reactiveValues(available = 18)
  
  ## Maximum resources.
  resourcesMax <- 1800
  
  ## Set proportion of the dataset that will be sampled at each request.
  proportion <- 0.05
  
  ## Number of objects to be sampled each time (round up to next integer).
  numSamples <- ceiling(numberOfObjects * proportion)
  
  # The color-blind-free palette with grey
  cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  
  ##### REACTIVE VALUES #####
  
  ## Set game status.
  game <- reactiveValues(started = FALSE,
                         tested  = FALSE)
  
  ## Set initial resources (experience not used now).
  resources <- reactiveValues(credits = resourcesMax,
                              training = rep(0, 10),
                              validation = rep(0, 10),
                              experience = 0)
  
  ## Set maximum validation F1.
  maxF1 <- 0
  
  ## Set parameters for max F1
  intercept <- 0
  slope <- 1.5
  
  ## Set initial test metrics.
  metricsTest <- reactiveValues(recall = rep(0, 10),
                                precision = rep(0, 10),
                                f1 = rep(0, 10))
  
  ## Set initial validation metrics.
  metricsValidation <- reactiveValues(recall = NULL,
                                      precision = NULL,
                                      f1 = NULL)
  
  ## Set the initial list of objects that have not been sampled yet.
  ## 1: not sampled, 0: sampled.
  #objectsNotSampled <- rep(1, objects)
  objects <- reactiveValues(sampled = rep(1, numberOfObjects))
  
  ## Set initial vector of active features.
  ## 0: inactive, 1: active
  activeFeatures <- rep(0, numberOfFeatures)
  
  ## Set initial vector of active objects.
  ## 0: inactive, 1: active.
  activeObjects <- rep(0, numberOfObjects)
  
  ## Set initial vector of labels.
  ## 0: not selected, 1: traininig, 2: validation.
  activeLabels <- rep(0, numberOfObjects)
  
  ## Set initial proportion of positive objects.
  proportionPositive <- 0.5
  
  ## Set initial training, validation, and test set.
  training <- NULL
  validation <- NULL
  test <- NULL
  
  ############ REACTIVE CODE HERE #############
  
  ## Change category (reset)
  observeEvent(input$nextCategory, {
    
    indexCurrentClass <<- indexCurrentClass + 1

    ## IF game has ended
    if(indexCurrentClass == 11) {
      
      ## Save average f1 (just to double check results)
      actionsSaved <<- list.append(actionsSaved,
                                   averageF1 = mean(metricsTest$f1),
                                   ts = Sys.time())
      
      ## build filename.
      fileName <- paste("./",
                        paste(input$username,
                              setSeed,
                              sep = "_"),
                        ".RData",
                        sep = "")
      
      ## save data
      save(actionsSaved, file = fileName)
      
      ## upload to FTP
      ftpUpload(fileName, paste("ftp://gmdn:gamifir@gmdn.altervista.org/", fileName, sep = ""))
      
    }
    
    ## update game status
    game$started <- FALSE
    game$tested <- FALSE
    
    clicks$available <- 18
    
    currentClass <<- classes[indexCurrentClass]
    
    objects$sampled <- rep(1, numberOfObjects)
    
    activeFeatures <<- rep(0, numberOfFeatures)
    
    activeObjects <<- rep(0, numberOfObjects)
    
    activeLabels <<- rep(0, numberOfObjects)
    
    metricsTest$recall <<- NULL
    
    training <<- NULL
    validation <<- NULL
    test <<- NULL
    
    maxF1 <<- 0
    
    intercept <<- 0
    slope <<- 1
    
    updateSliderInput(session,
                      inputId = "intercept",
                      value = 0,
                      min = -50,
                      max = 50)
    
    updateSliderInput(session,
                      inputId = "slope",
                      value = 1.5,
                      min = 0.7,
                      max = 2.2)
    
  })
  
  ## When game starts sample for training and validation.
  observeEvent(input$startGame, {
    
    ## If game has not already started
    if(!game$started & indexCurrentClass > 0) {
      
      ## Save actions
      actionsSaved <<- list.append(actionsSaved,
                                   startClass = indexCurrentClass,
                                   ts = Sys.time())
      
      ## update status of the game.
      game$started <- TRUE
      
      ## Sample from available objects.
      newSample <- sample(x = which(objects$sampled == 1),
                          size = numSamples * 2,
                          replace = FALSE)
      
      ## Set this sample of objects as active.
      activeObjects[newSample] <<- 1
      
      ## Assign the training (1) label to the first half.
      activeLabels[newSample[1:numSamples]] <<- 1
      
      ## Assign the validation (2) label to the second half.
      activeLabels[newSample[(numSamples + 1):(numSamples * 2)]] <<- 2
      
      ## This sample will not be available next time.
      objects$sampled[newSample] <<- 0
      
    }
    
  })
  
  ## If new training samples are requested.
  observeEvent(input$addTraining, {
    
    ## Check game status.
    if(game$started & !game$tested & (clicks$available > 0)) {
      
      actionsSaved <<- list.append(actionsSaved,
                                   addTraining = 1,
                                   ts = Sys.time())
      
      ## Update click available.
      clicks$available <- clicks$available - 1
      
      ## Reset F1.
      maxF1 <<- 0
      
      ## Update status.
      game$tested <- FALSE
      
      ## Update resources.
      resources$credits <- resources$credits - trainingCost
      
      resources$training[indexCurrentClass] <- resources$training[indexCurrentClass] + trainingCost
      
      ## If we have enough objects then sample.
      if(sum(objects$sampled) > numSamples) {
        
        ## Sample from available objects.
        newSample <- sample(x = which(objects$sampled == 1),
                            size = numSamples,
                            replace = FALSE)
        
        ## Set this sample of objects as active.
        activeObjects[newSample] <<- 1
        
        ## Assign the training (1) label.
        activeLabels[newSample] <<- 1
        
        ## This sample will not be available next time.
        objects$sampled[newSample] <<- 0
        
      } #if(sum(objectsNotSampled) > numSamples)
      
      ## Otherwise add the last elements without sampling.
      else {
        
        ## Sample from available objects.
        lastSample <- which(objects$sampled == 1)
        
        ## Set this sample of objects as active.
        activeObjects[lastSample] <<- 1
        
        ## Assign the training (1) label.
        activeLabels[lastSample] <<- 1
        
        ## This sample will not be available next time.
        objects$sampled[lastSample] <<- 0
        
      } #else
      
    }
    
  })
  
  ## If new validation samples are requested.
  observeEvent(input$addValidation, {
    
    ## Check game status.
    if(game$started & !game$tested & (clicks$available > 0)) {
      
      actionsSaved <<- list.append(actionsSaved,
                                   addValidation = 1,
                                   ts = Sys.time())
      
      ## Update click available.
      clicks$available <- clicks$available - 1
      
      ## Reset F1.
      maxF1 <<- 0
      
      ## Update status.
      game$tested <- FALSE
      
      ## Update resources
      resources$credits <- resources$credits - validationCost
      
      resources$validation[indexCurrentClass] <- resources$validation[indexCurrentClass] + trainingCost
      
      ## If we have enough objects then sample.
      if(sum(objects$sampled) > numSamples) {
        
        ## Sample from available objects.
        newSample <- sample(x = which(objects$sampled == 1),
                            size = numSamples,
                            replace = FALSE)
        
        ## Set this sample of objects as active.
        activeObjects[newSample] <<- 1
        
        ## Assign the validation (2) label.
        activeLabels[newSample] <<- 2
        
        ## This sample will not be available next time.
        objects$sampled[newSample] <<- 0
        
      } #if(input$addValidation > 0)
      
      ## Otherwise add the last elements without sampling.
      else {
        
        ## Sample from available objects.
        lastSample <- which(objects$sampled == 1)
        
        ## Set this sample of objects as active.
        activeObjects[lastSample] <<- 1
        
        ## Assign the validation (2) label.
        activeLabels[lastSample] <<- 2
        
        ## This sample will not be available next time.
        objects$sampled[lastSample] <<- 0
        
      } #else
      
    }
    
  })
  
  
  ## Compute estimates of the parameters of the model.
  ## This function is called only when a new training
  ## sample is available.
  ## Return a list of two vectors of parameters.
  estimateParameters <- reactive({
    
    ## If game starts.
    input$startGame
    
    ## Check whether a new sample is available.
    input$addTraining
    
    ## Build (temporary) training set with training
    ## objects only (label 1).
    training <<- datasetTraining[activeLabels == 1, ]
    
    ## Find active features within the temporary
    ## training, and update the set of active features.
    activeFeatures[colSums(training) > 5] <<- 1
    
    ## Consolidate training set.
    training <<- training[, activeFeatures == 1]
    
    ## Find training Labels of the chosen class.
    classLabels <- labelsTraining[activeLabels == 1, currentClass]
    
    ## Update proportion of positive class (with laplacian smoothing).
    proportionPositive <<- (sum(classLabels) + 1) / (length(classLabels) + 2)
    
    ## Compute parameters estimate.
    parameters <- nbEstimate(training, classLabels)
    
    ## Return a list of two elements:
    ## 1) a vector parametersPositive;
    ## 2) a vector parametersNegative.
    return(parameters)
    
  })
  
  ## Compute the coordinates of the objects in the validation
  ## set.
  ## This function is called only when a new estimate of
  ## the parameters is available or when a new sample
  ## of validation objects is requested.
  ## Return a list of two vectors of coordinates.
  coordinatesValidation <- reactive({
    
    ## Check whether new parameters are available.
    parameters <- estimateParameters()
    
    ## Check whether a new validation sample is available.
    input$addValidation
    
    ## Build validation set with the validation labels (2),
    ## and the active features in the training set.
    validation <<- datasetTraining[activeLabels == 2,
                                   activeFeatures == 1]
    
    ## Compute coordinate of positive class.
    coordinateX <- computeCoordinate(validation,
                                     parameters$positive,
                                     proportionPositive)
    
    ## Compute coordinate of negative class.
    coordinateY <- computeCoordinate(validation,
                                     parameters$negative,
                                     1 - proportionPositive)
    
    ## Return a list of two vectors of coordinates.
    return(list(x = coordinateX, y = coordinateY))
    
  })
  
  ## Observe click on test and update performance.
  observeEvent(input$testModel, {
    
    ## Check game status.
    if(!game$tested & game$started) {
      
      actionsSaved <<- list.append(actionsSaved,
                                   testClass = indexCurrentClass,
                                   ts = Sys.time())
      
      ## Update game status.
      game$tested <- TRUE
      
      # First, update resources.
      resources$credits <- resources$credits - testCost
      
      ## Check whether new parameters are available.
      parameters <- estimateParameters()
      
      ## Build test with active features in the training set.
      test <<- datasetTest[, activeFeatures == 1]
      
      ## Find test Labels of the chosen class.
      classLabels <- labelsTest[, currentClass]
      
      ## Compute coordinate of positive class.
      coordinateX <- computeCoordinate(test,
                                       parameters$positive,
                                       proportionPositive)
      
      ## Compute coordinate of negative class.
      coordinateY <- computeCoordinate(test,
                                       parameters$negative,
                                       1 - proportionPositive)
      
      ## Build list of coordinates.
      coordinates <- list(x = coordinateX, y = coordinateY)
      
      ## Use line parameters (intercept, slope).
      line <- list(q = input$intercept, m = input$slope)
      
      ## Compute classification performance.
      metrics <- nbClassify(coordinates, classLabels, line)
      
      metricsTest$recall[indexCurrentClass] <- metrics$recall
      metricsTest$precision[indexCurrentClass] <- metrics$precision
      metricsTest$f1[indexCurrentClass] <- metrics$f1
      
      actionsSaved <<- list.append(actionsSaved,
                                   recall = metrics$recall,
                                   precision = metrics$precision,
                                   f1 = metrics$f1,
                                   ts = Sys.time())
      
    }
    
  })
  
  
  
  ############******* END REACTIVE HERE *******############
  
  ############******* OUTPUT CODE HERE *******#############
  
  ## Create a string to display the number of
  ## documents still available.
  output$availableObjects <- renderText({
    
    ## Create info when
    input$startGame
    input$clear
    input$nextCategory
    input$addTraining
    input$addValidation
    
    ## Available objects.
    availableObjects <- paste("Number of available objects =",
                              sum(activeLabels == 0))
    
    ## Return a string
    return(availableObjects)
    
  })
  
  ## Compute the classification performance.
  #output$validationPerformance <- renderText({
  observe({
    
    ## Activate when
    input$nextCategory
    
    ## Check whether new samples are available.
    coordinates <- coordinatesValidation()
    
    ## Get class labels.
    classLabels <- labelsTraining[activeLabels == 2, currentClass]
    
    ## Use line parameters (intercept, slope).
    line <- list(q = input$intercept, m = input$slope)
    
    ## Compute classification performance.
    performance <- nbClassify(coordinates,
                              classLabels,
                              line)
    
    metricsValidation$recall <- performance$recall
    metricsValidation$precision <- performance$precision
    metricsValidation$f1 <- performance$f1
    
    ## Update best parameters if necessary
    if(performance$f1 > maxF1) {
      maxF1 <<- performance$f1
      intercept <<- input$intercept
      slope <<- input$slope
    }
    
  })
  
  
  
  ## Create a scatter plot with the coordinates of the objects
  ## in the validation set.
  ## This function is called whenever a new training sample or validation
  ## sample is available.
  output$coordinatesValidation <- renderPlot({
    
    ## Activate when
    input$clear
    input$nextCategory
    
    ## Check whether new samples are available
    ## through coordinatesValidation().
    coordinates <- coordinatesValidation()
    
    ## Check whether there are some objects.
    if(sum(activeObjects) == 0) {
      
      if(indexCurrentClass == 0) {
        
        ggplot(data.frame()) + geom_point() +
          ggtitle(paste("Start game"))
        
      } else if (indexCurrentClass > 10) {
        
        df <- data.frame()
        ggplot(df) + geom_point() + xlim(0, 10) + ylim(0, 100) +
          ggtitle("End of game") +
          annotate("text",
                   x = 4,
                   y = 55, 
                   label = "Your average score was",
                   color = cbPalette[2],
                   size = 10
          ) +
          annotate("text",
                   x = 8,
                   y = 55, 
                   label = format(round(mean(metricsTest$f1), 3), nsmall = 3),
                   color = cbPalette[2],
                   size = 10
          ) +
          
          annotate("text",
                   x = 4,
                   y = 40, 
                   label = "The average goal was",
                   color = cbPalette[8],
                   size = 10
          ) +
          annotate("text",
                   x = 8,
                   y = 40, 
                   label = format(round(mean(goalsF1), 3), nsmall = 3),
                   color = cbPalette[8],
                   size = 10
          )
        
        
      } else {
        
        ggplot(data.frame()) + geom_point() +
          ggtitle(paste("Current class :",
                        classNames[classes[indexCurrentClass]]))
        
      } 
      
    }
    else{
      
      ## Build string with current F1.
      currentF1 <- format(round(metricsValidation$f1, 3), nsmall = 3)
      bestF1 <- format(round(maxF1, 3), nsmall = 3)
      goalF1 <- format(round(goalsF1[classes[indexCurrentClass]], 3), nsmall = 3)
      
      cF1 <- grobTree(textGrob(paste(currentF1, "(current)"),
                               x = 0.1,
                               y = 0.85,
                               hjust = 0,
                               gp = gpar(col = cbPalette[2],
                                         fontsize = 20,
                                         fontface = "plain")))
      
      bF1 <- grobTree(textGrob(paste(bestF1, "(best)"),
                               x = 0.1,
                               y = 0.75,
                               hjust = 0,
                               gp = gpar(col = cbPalette[6],
                                         fontsize = 20,
                                         fontface = "bold")))
      
      gF1 <- grobTree(textGrob(paste(goalF1, "(goal)"),
                               x = 0.1,
                               y = 0.65,
                               hjust = 0,
                               gp = gpar(col = cbPalette[8],
                                         fontsize = 20,
                                         fontface = "italic")))
      
      ## build data frame for ggplot
      coords <- data.frame(x = as.vector(coordinates$x),
                           y = as.vector(coordinates$y),
                           classes = as.factor(labelsTraining[activeLabels == 2, currentClass]))
      
      ## Create plot with corresponding colors.
      if(input$showPositive & input$showNegative) {
        
        ggplot(coords, aes(x = x, y = y, colour = classes)) +
          ggtitle(paste("Current class :",
                        classNames[classes[indexCurrentClass]])) +
          scale_color_manual(values = cbPalette[c(7, 3)]) +
          geom_point(alpha = 0.4) +
          geom_abline(slope = input$slope,
                      intercept = input$intercept,
                      colour = "blue") +
          annotation_custom(grob = cF1) + 
          annotation_custom(grob = bF1) +
          annotation_custom(grob = gF1) 
        
      }
      else if(input$showPositive) {
        
        classLabels <- labelsTraining[activeLabels == 2, currentClass]
        
        ggplot(coords[classLabels == 1, ],
               aes(x = x, y = y, colour = classes)) +
          ggtitle(paste("Current class :",
                        classNames[classes[indexCurrentClass]])) +
          scale_color_manual(values = cbPalette[c(3)]) +
          geom_point(alpha = 0.3) + 
          #geom_abline(slope = 1, intercept = 0, colour = "green") +
          geom_abline(slope = input$slope,
                      intercept = input$intercept,
                      colour = "blue") +
          annotation_custom(grob = cF1) + 
          annotation_custom(grob = bF1) +
          annotation_custom(grob = gF1) 
        
      }
      else if(input$showNegative) {
        
        classLabels <- labelsTraining[activeLabels == 2, currentClass]
        
        ggplot(coords[classLabels == 0, ],
               aes(x = x, y = y, colour = classes)) +
          ggtitle(paste("Current class :",
                        classNames[classes[indexCurrentClass]])) +
          scale_color_manual(values = cbPalette[c(7)]) +
          geom_point(alpha = 0.3) + 
          geom_abline(slope = input$slope,
                      intercept = input$intercept,
                      colour = "blue") +
          annotation_custom(grob = cF1) + 
          annotation_custom(grob = bF1) +
          annotation_custom(grob = gF1) 
        
      }
      
    }
    
  })
  
  
  output$resourcesSpent <- renderPlot(height = 250, {
    
    resourcesSpent <- data.frame(class = 1:10,
                                 training = resources$training,
                                 validation = resources$validation)
    
    resourcesReshape <- melt(resourcesSpent, id.vars = "class")
    
    ggplot(resourcesReshape, aes(x = class,
                                 y = value,
                                 fill = variable,
                                 colours = variable)) +
      scale_y_continuous(limits = c(-10, 180)) +
      scale_fill_manual(values = cbPalette[c(4, 8)]) +
      geom_bar(stat = "identity") +
      scale_x_continuous(breaks = 1:10) +
      annotate("text",
               x = 1:10, indexCurrentClass,
               y = -7,
               label = format(round(metricsTest$f1,
                                    digits = 3),
                              nsmall = 3),
               size = 5)
    
  })
  
  
  
  ## Observe click in the plot and update sliders.
  observe({
    
    ## If there is a click on the plot.
    if(!is.null(input$plot_click)) {
      
      ## Get coordinates of the point.
      clickY <- input$plot_click$y
      clickX <- input$plot_click$x
      
      ## Compute the new intercept (q = y - mx).
      newIntercept <- clickY - clickX * input$slope
      
      ## Set the new limits of the slider.
      sliderMin <- round(newIntercept) - 50
      sliderMax <- round(newIntercept) + 50
      
      ## Update the slider of the intercept.
      updateSliderInput(session,
                        inputId = "intercept",
                        value = newIntercept,
                        min = sliderMin,
                        max = sliderMax)
      
    }
    
  })
  
  ## Update resources available.
  output$resourcesAvailable <- renderText({
    
    return(paste("Resources available :", resources$credits))
    
  })
  
  ## Create a string to display the number of clicks
  ## still available.
  output$clicksAvailable <- renderText({
    
    return(paste("Clicks available :", clicks$available))
    
  })
  
  
  ## Observe increase/decrease clicks on parameter A and B.
  observeEvent(input$increaseA, {
    newValue <- input$intercept + 1
    updateSliderInput(session,
                      inputId = "intercept",
                      value = newValue)
  })
  
  observeEvent(input$decreaseA, {
    newValue <- input$intercept - 1
    updateSliderInput(session,
                      inputId = "intercept",
                      value = newValue)
  })
  
  observeEvent(input$increaseB, {
    newValue <- input$slope + 0.01
    updateSliderInput(session,
                      inputId = "slope",
                      value = newValue)
  })
  
  observeEvent(input$decreaseB, {
    newValue <- input$slope - 0.01
    updateSliderInput(session,
                      inputId = "slope",
                      value = newValue)
  })
  
  ## Observe clicks on set the best parameters
  observeEvent(input$best, {
    
    updateSliderInput(session,
                      inputId = "intercept",
                      value = intercept)
    
    updateSliderInput(session,
                      inputId = "slope",
                      value = slope)
    
  })
  
  observe({
    
    if(game$started) {
      
      output$startGame <- renderUI({
        actionButton(inputId = "startGame",
                     label   = "Start Game", 
                     class = "btn-danger",
                     width = "100%")
      })
      
      if(game$tested) {
        
        output$addTraining <- renderUI({
          actionButton(inputId = "addTraining",
                       label   = "Training", 
                       class = "btn-danger",
                       width = "100%")
        })
        
        output$addValidation <- renderUI({
          actionButton(inputId = "addValidation",
                       label   = "Validation", 
                       class = "btn-danger",
                       width = "100%")
        })
        
        output$testModel <- renderUI({
          actionButton(inputId = "testModel",
                       label   = "Test", 
                       class = "btn-danger",
                       width = "100%")
        })
        
      } else {
        
        if(clicks$available > 0) {
          
          output$addTraining <- renderUI({
            actionButton(inputId = "addTraining",
                         label   = "Training", 
                         class = "btn-info",
                         width = "100%")
          })
          
          output$addValidation <- renderUI({
            actionButton(inputId = "addValidation",
                         label   = "Validation", 
                         class = "btn-info",
                         width = "100%")
          })
          
        } else {
          
          output$addTraining <- renderUI({
            actionButton(inputId = "addTraining",
                         label   = "Training", 
                         class = "btn-danger",
                         width = "100%")
          })
          
          output$addValidation <- renderUI({
            actionButton(inputId = "addValidation",
                         label   = "Validation", 
                         class = "btn-danger",
                         width = "100%")
          })
          
        }
        
        output$testModel <- renderUI({
          actionButton(inputId = "testModel",
                       label   = "Test", 
                       class = "btn-info",
                       width = "100%")
        })
        
      }
      
    }
    else {
      
      if(indexCurrentClass == 0 | indexCurrentClass > 10) {
        
        output$startGame <- renderUI({
          actionButton(inputId = "startGame",
                       label   = "Start Game", 
                       class = "btn-danger",
                       width = "100%")
        })
        
      } else {
        
        output$startGame <- renderUI({
          actionButton(inputId = "startGame",
                       label   = "Start Game", 
                       class = "btn-info",
                       width = "100%")
        })
      }
      
      output$addTraining <- renderUI({
        actionButton(inputId = "addTraining",
                     label   = "Training", 
                     class = "btn-danger",
                     width = "100%")
      })
      
      output$addValidation <- renderUI({
        actionButton(inputId = "addValidation",
                     label   = "Validation", 
                     class = "btn-danger",
                     width = "100%")
      })
      
      output$testModel <- renderUI({
        actionButton(inputId = "testModel",
                     label   = "Test", 
                     class = "btn-danger",
                     width = "100%")
      })
      
    }
    
  })
  
  ############ END OUTPUT CODE #############
  
})
