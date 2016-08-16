#---------------------------PACKAGES---------------------------
library(e1071)
library(rpart)

##************************************VARIABLES#************************************
features <- c("age", "gender", "hcc", "cost")
variables <- c("age", "gender", "hcc")
ctrl <- rpart.control(minsplit=2, minbucket=1, cp=0.0001)

ind.hcc.valid <- NULL

demographic.model <- NULL
demographic.res <- NULL
demographic.eval <- NULL

linear.model <- NULL
linear.res <- NULL
linear.eval <- NULL

dtree.model <- NULL
dtree.res <- NULL
dtree.eval <- NULL
#************************************FUNCTIONS************************************
#----------------MODELING----------------
Reset_Bank <- function() {
  
  ind.hcc.valid <- NULL
  
  demographic.model <- NULL
  demographic.res <- NULL
  demographic.eval <- NULL
  
  linear.model <- NULL
  linear.res <- NULL
  linear.eval <- NULL
  
  dtree.model <- NULL
  dtree.res <- NULL
  dtree.eval <- NULL
}

Create_Predictive_Model <- function(dataset, algorithm = "linear") {
  
  ind.valid <- which(dataset$hcc != 0)
  
  if(is.null(demographic.model)) {
    demographic.model <<- lm(cost ~ age + gender, data = dataset)
  }
  
  if(algorithm == "dt") {
    dtree.model <<- rpart(cost ~ ., data = dataset[ind.valid , features], method = "anova", control = ctrl)
  }
  else {
    linear.model <<- lm(cost ~ ., data = dataset[ind.valid , features])
  }                           
}

#----------------EVALUATION----------------
Predict_Cost <- function(test.data, algorithm = "linear") {
  
  if(!Is_Resul_NULL(algorithm)) {
    return()
  }
  
  if(is.null(ind.hcc.valid)) {
    ind.hcc.valid <<- which(test.data$hcc != 0)
  }
  
  # Based on the algorithm selected, predict costs
  if(algorithm == "demographic") {
    demographic.res <<- predict(demographic.model, test.data)
  }
  else if(algorithm == "dt") {
    dtree.res <<- rep(0, nrow(test.data))
    dtree.res[ind.hcc.valid] <<- predict(dtree.model, test.data[ind.hcc.valid, variables])
    dtree.res[-ind.hcc.valid] <<- predict(demographic.model, test.data[-ind.hcc.valid, ])
  }
  else {
    linear.model$xlevels[["hcc"]] <- union(linear.model$xlevels[["hcc"]], levels(test.data$hcc)) 
    linear.model$xlevels[["age"]] <- union(linear.model$xlevels[["age"]], levels(test.data$age))
    
    linear.res <<- rep(0, nrow(test.data))
    linear.res[ind.hcc.valid] <<- predict(linear.model, test.data[ind.hcc.valid, variables])
    linear.res[-ind.hcc.valid] <<- predict(demographic.model, test.data[-ind.hcc.valid, ])
  }
}

Evaluate_Model <- function(test.data, algorithm) {
  if(!Is_Evaluation_NULL(algorithm) || Is_Resul_NULL(algorithm)) {
    return()
  }
  
  if(algorithm == "demographic") {
    demographic.eval <<- sapply(1:177, function(x) {
      ind <- which(test.data$hcc == x)
      R2(test.data$cost[ind], demographic.res[ind])
    })
  }
  
  if(algorithm == "linear") {
    linear.eval <<- sapply(1:177, function(x) {
      ind <- which(test.data$hcc == x)
      R2(test.data$cost[ind], linear.res[ind])
    })
  } 
  if(algorithm == "dt") {
    dtree.eval <<- sapply(1:177, function(x) {
      ind <- which(test.data$hcc == x)
      R2(test.data$cost[ind], dtree.res[ind])
    })
  }
}

Is_Model_NULL <- function(algorithm) {
  if(algorithm == "demographic") {
    return (is.null(demographic.model))
  }
  if(algorithm == "dt") {
    return (is.null(dtree.model))
  }
  if(algorithm == "linear") {
    return (is.null(linear.model))
  }
  
  return (TRUE)
}
Is_Resul_NULL <- function(algorithm){
  if(algorithm == "demographic") {
    return (is.null(demographic.res))
  }
  if(algorithm == "dt") {
    return (is.null(dtree.res))
  }
  if(algorithm == "linear") {
    return (is.null(linear.res))
  }
  
  return (TRUE)
}
Is_Evaluation_NULL <- function(algorithm) {
  if(algorithm == "demographic") {
    return (is.null(demographic.eval))
  }
  if(algorithm == "dt") {
    return (is.null(dtree.eval))
  }
  if(algorithm == "linear") {
    return (is.null(linear.eval))
  }
  
  return (TRUE)
}

R2 <- function(actual, predicted) { 
  RSS <- sum( (actual - predicted)^2 )
  TSS <- sum( (actual - mean(actual))^2 )
  r2 <- 1 - (RSS / TSS)
  return(r2)
}

RMSE <- function(actual, predicted) {
  rmse <- sqrt( mean( (actual - predicted)^2 ) )
  return (rmse)
}
