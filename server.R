library("shiny")
source("DataLayer.R")
source("Plotter.R")
source("ModelBank.R")

shinyServer(function(input, output, session) {
  options(shiny.maxRequestSize=1024^3)
  trainset <- reactive({
    if(is.null(input$trainset)) {
      return(NULL)
    }
    # Reset Previous Models
    Reset_Bank()
    # read.csv(input$trainset$datapath)
    ReadData(input$trainset)
  })
  # trainset <- reactive({
  #   p <- NULL
  #   p$datapath <- "/Users/Mehdi/Desktop/Mehdi/Health Informatics/Capstone/CCPV/trainset.csv"
  #   ReadData(p)
  # })
  testset <- reactive({
    if(is.null(input$testset)) {
      return(NULL)
    }
    ReadData(input$testset)
  })
  # testset <- reactive({
  #   p <- NULL
  #   p$datapath <- "/Users/Mehdi/Desktop/Mehdi/Health Informatics/Capstone/CCPV/testset.csv"
  #   ReadData(p)
  # })
  selected.data <- reactive( {
    if(input$dataset == "First Year") {
      trainset()
    }
    else {
      testset()
    }
  })
  output$train_stat <- renderUI({
    if(!is.null(trainset())) {
      icon("ok", lib = "glyphicon")
    }
    else {
      icon("minus", lib = "glyphicon")
    }
  })
  output$test_stat <- renderUI({
    if(!is.null(testset())) {
      icon("ok", lib = "glyphicon")
    }
    else {
      icon("minus", lib = "glyphicon")
    }
  })
  #*************************************DATA ANALYSIS*************************************

  #-----------------POPULATION ANALYSIS-----------------
  output$age_dist <- renderPlot({
    if(!is.null(selected.data())) {
      plotDistribution.Factor(selected.data()$age, x.name = "Age")
    }
  })
  output$gender_dist <- renderPlot({
    if(!is.null(selected.data())) {
      plotDistribution.Factor(selected.data()$gender, x.name = "Gender", levels = c("Male", "Female"))
    }
  })
  output$hcc_dist <- renderPlot({
    if(!is.null(selected.data())) {
      valid.ind <- which(selected.data()$hcc != 0)
      plotTopNDistribution.Factor(selected.data()$hcc[valid.ind], n = 20, x.name = "HCC", vertical = T)
    }
  })
  output$cost_dist <- renderPlot({
    if(!is.null(selected.data())) {
      min.cost <- input$cost_range[1]
      max.cost <- input$cost_range[2]
      isHist <- input$cost_isHist == "Histogram"
      plotDistribution.Numeric(selected.data()$cost, xlab = "cost",
                               
                               min = min.cost, max = max.cost, hist = isHist)
    }
  })
  
  #-----------------COST ANALYSIS-----------------
  output$cost_age_dist <- renderPlot({
    if(!is.null(selected.data())) {
      plotTopNDistribution.Numeric_Factor(num = selected.data()$cost, fact = selected.data()$age, "Age",n = 10)
    }
  })
  output$cost_gender_dist <- renderPlot({
    plotDistribution.Numeric_Factor(num = selected.data()$cost, fact = selected.data()$gender
                                     , "Gender", c("Male", "Female"))
  })
  output$cost_hcc_dist <- renderPlot({
    if(!is.null(selected.data())) {
      valid.ind <- which(selected.data()$hcc != 0)
      plotTopNDistribution.Numeric_Factor(num = selected.data()$cost[valid.ind],
                                          fact = selected.data()$hcc[valid.ind], "HCC",
                                          n = 10)
    }
  })
  
  #*************************************DATA COMPARISON*************************************
  output$comp_age <- renderPlot({
    if(!is.null(trainset()) && !is.null(testset())) {
      plotComparision.Factor(trainset()$age, testset()$age, "Age", 
                             year1 = trainset()$year[1],
                             year2 = testset()$year[1]
                             )
    }
  })
  output$comp_gender <- renderPlot({
    if(!is.null(trainset()) && !is.null(testset())) {
      plotComparision.Factor(trainset()$gender, testset()$gender, "Gender",
                             year1 = trainset()$year[1],
                             year2 = testset()$year[1],
                             levels = c("Male", "Female")
                             )
    }
  })
  output$comp_cost <- renderPlot({
    if(!is.null(trainset()) && !is.null(testset())) {
      min.cost <- input$comp_cost_range[1]
      max.cost <- input$comp_cost_range[2]
      isHist <- input$comp_isHist == "Histogram"
      plotComparision.Numeric(trainset()$cost, x.year2 = testset()$cost, 
                              x.name = "Cost", hist = isHist,
                              year1 = trainset()$year[1],
                              year2 = testset()$year[1],
                              min = min.cost, max = max.cost
                              )
    }
  })
  observeEvent(input$plot_hcc_comp, {
    output$comp_hcc <- renderPlot({
      if(!is.null(trainset()) && !is.null(testset())) {
        plotComparision.Factor(trainset()$hcc, testset()$hcc, "HCC",
                               year1 = trainset()$year[1],
                               year2 = testset()$year[1],
                               filter = isolate({input$hcc_select_comp})
        )
      }
    })
  })
  #*************************************COST PREDICTION*************************************
  observeEvent(input$btn_train, {
    if(!Is_Model_NULL(isolate({input$model}))) {
      output$status <- renderText({"Model is Already Trained"})
    }
    else if(!is.null(trainset())) {
      output$status <- renderText({"Training Started. Please Wait..."})
      Create_Predictive_Model(trainset(), isolate(input$model))
      output$status <- renderText({paste(isolate(input$model), "Model Trained Successfully")})
    }
  })
  observeEvent(input$btn_evaluate, {
    if(is.null(trainset()) || Is_Model_NULL(isolate({input$model}))) {
      return()
    }
    Predict_Cost(testset(), isolate({input$model}))
    Evaluate_Model(testset(), isolate({input$model}))
    
    output$predict_plot <- renderPlot({
      if(!is.null(testset()) && !Is_Model_NULL(isolate({input$model}))) {
        isHist <- input$eval_isHist == "Histogram"
        min.val <- input$eval_cost_range[1]
        max.val <- input$eval_cost_range[2]
        if(isolate({input$model}) == "demographic") {
          plotPredictedCost(actual = testset()$cost, predicted = demographic.res,
                            hist = isHist, min = min.val, max = max.val)
        }
        else if(isolate({input$model}) == "dt"){
          plotPredictedCost(actual = testset()$cost[ind.hcc.valid], predicted = dtree.res,
                            hist = isHist, min = min.val, max = max.val)
        }
        else {
          plotPredictedCost(actual = testset()$cost[ind.hcc.valid], predicted = linear.res,
                            hist = isHist, min = min.val, max = max.val)
        }
      }
    })
    
    output$predict_res <- renderDataTable({
      data <- NULL
      if(!is.null(testset()) && !Is_Model_NULL(isolate({input$model}))) {
        
        data <- cbind.data.frame('User Id' = testset()$id, "Age" = testset()$age, "Gender" = testset()$gender)
        
        if(isolate({input$model}) == "demographic") {
          data <- cbind.data.frame(data, "Cost" = demographic.res)
        }
        else if(isolate({input$model}) == "dt") {
          data <- cbind.data.frame(data, "HCC" = testset()$hcc, "Cost" = dtree.res)
        }
        else {
          data <- cbind.data.frame(data, "HCC" = testset()$hcc, "Cost" = linear.res)
        }
      }
      data
    })
    
    output$predict_eval <- renderDataTable({
      data <- NULL
      if(isolate({input$model}) == "demographic") {
        data <- cbind.data.frame(HCC = 1:177, "R2" = demographic.eval)
      }
      else if(isolate({input$model}) == "dt") {
        data <- cbind.data.frame(HCC = 1:177, "R2" = dtree.eval)
      }
      else {
        data <- cbind.data.frame(HCC = 1:177, "R2" = linear.eval)
      }
      data
    })
    
  })
  
  output$download_res <- downloadHandler(
    filename = function() { paste(input$model, '_res.csv', sep='') },
    content = function(file) {
      data <- NULL
      
      if(!Is_Resul_NULL(input$model)) {
        data <- data.frame('User Id' = testset()$id, "Age" = testset()$age, "Gender" = testset()$gender)
        
        if(isolate({input$model}) == "demographic") {
          data <- cbind.data.frame(data, "Predicted" = demographic.res)
        }
        if(isolate({input$model}) == "dt") {
          data <- cbind.data.frame(data, "HCC" = testset()$hcc, "Predicted" = dtree.res)
        }
        if(isolate({input$model}) == "linear") {
          data <- cbind.data.frame(data, "HCC" = testset()$hcc, "Predicted" = linear.res)
        }
      }
      
      write.csv(data, file)
    }
  )
  
  output$download_eval <- downloadHandler(
    filename = function() { paste(input$model, '_evaluation.csv', sep='') },
    content = function(file) {
      data <- NULL
      
      if(!Is_Evaluation_NULL(input$model)) {
        data <- data.frame('HCC' = 1:177)
        
        if(isolate({input$model}) == "demographic") {
          data <- cbind.data.frame(data, "R2" = demographic.eval)
        }
        if(isolate({input$model}) == "dt") {
          data <- cbind.data.frame(data, "R2" = dtree.eval)
        }
        if(isolate({input$model}) == "linear"){
          data <- cbind.data.frame(data, "R2" = linear.eval)
        }
      }
      
      write.csv(data, file)
    }
  )
})
