
mapping <- NULL

ReadData <- function(file) {
  # file.data <- read.csv(file$datapath, header = T, nrows = 20000)
  file.data <- read.csv(file$datapath, header = T)
  
  # modify column types
  file.data$diagnostic <- as.character(file.data$diagnostic)
  file.data$id <- as.character(file.data$id)
  file.data$age <- as.integer(file.data$age)
  file.data$gender <- factor(file.data$gender, c(0,1))
  file.data$cost <- as.numeric(file.data$cost)
  file.data$year <- as.integer(file.data$year)
  
  # Remove invalid rows
  file.data <- CleanData(file.data)
  
  # Convert age to factor
  age <- cut(file.data$age, 
             breaks = c(-Inf, 5, 12, 17, 24, 34, 44, 54, 64, 69, 74, 79, 84, 89, Inf),
             labels = c("-5", "6-12", "13-17", "18-24", "25-34", "35-44", "45-54", "55-64", 
                        "65-69", "70-74", "75-79", "80-84", "85-89", "90+" ),
             right = T)
  file.data$age <- age
  
  # Assign HCC codes
  hcc <- GetHCCMapping(file.data$diagnostic)
  file.data <- cbind.data.frame(file.data, hcc)
  
  rm(age, hcc)
  
  return(file.data)
}

CleanData <- function(data) {
  
  # find rows whith invalid cost, age, diagnostic or gender
  invalid.ind <- NULL
  
  ind.cost.inv <- which(data$cost == 0)
  if(length(ind.cost.inv) != 0) {
    invalid.ind <- c(invalid.ind, ind.cost.inv)
  }
  
  ind.age.inv <- which(data$age < 0 )
  if(length(ind.age.inv) != 0) {
    invalid.ind <- c(invalid.ind, ind.age.inv)
  }
  
  ind.diag.inv <- which(data$diagnostic == "" || is.null(data$diagnostic))
  if(length(ind.diag.inv) != 0) {
    invalid.ind <- c(invalid.ind, ind.diag.inv)
  }
  
  ind.gender.inv <- which(is.na(data$gender))
  if(length(ind.gender.inv) != 0) {
    invalid.ind <- c(invalid.ind, ind.gender.inv)
  }
  
  if(length(invalid.ind) != 0) {
    return(data[-invalid.ind, ])
  }
  
  return(data)
}

GetHCCMapping <- function(icd9_codes) {
  col.classes <- c("character", rep("numeric", 2), "character")
  mapping <<- read.csv("MAPPINGS.csv", header = T, colClasses = col.classes)
  
  #remove NA hccs
  na.ind <- which(is.na(mapping$HCC) == TRUE)
  mapping <<- mapping[-na.ind, ]

  # set the row names to easily query rows by ICD9 Codes
  rownames(mapping) <- mapping$ICD9

  # Mapp ICD9 to HCC
  HCC <- rep(0, length(icd9_codes))
  hcc.ind <- which(icd9_codes %in% mapping$ICD9)
  HCC[hcc.ind] <- mapping[icd9_codes[hcc.ind] ,2]
  # HCC <- as.factor(HCC)
  HCC <- factor(HCC, levels = as.character(0:177))
  return (HCC)
}
