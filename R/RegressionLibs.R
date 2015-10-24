#' Diagnostic Data Generator
#'
#' Calculate different error types on the given data and return an object of class 
#' data frame ready for usage in different diagnostics plots.
#' 
#' @param fit an object of class lm that contains the result to fit a linear model 
#' regression.
#' @return a data frame object with origin values, fitted values, and differents 
#' error types for make some diagnostics plots.
#' @seealso ResidualsFitted, StResidualsFitted, NormalQQ, StResidualsLeverange
#' @examples
#' iris.x <- iris[,1:3]
#' Petal.Width <- iris[,4]
#' ir.pca <- prcomp(iris.x, center = TRUE, scale. = TRUE)
#' PCA <- as.data.frame(ir.pca$x)
#' 
#' PC1 <- PCA[,1]
#' PC2 <- PCA[,2]
#' PC3 <- PCA[,3]
#' 
#' fit <- lm(Petal.Width ~ PC1 + PC2 + PC3, data = PCA)
#' diagnostic <- diagnosticData(fit)
diagnosticData <- function(fit) {
  
  if (missing(fit)) {
    stop("Need to specify fit value")
  }
  else {
    if (!('model' %in% names(fit))) {
      stop("fit must be a closure type")
    }
  }
  
  dependentVariableName <- "Dependent Variable"
  
  #All parameters are OK
  diagnostic <- data.frame(fit$model[,2:ncol(fit$model)])
  dependentVariable <- data.frame(fit$model[,1])
  names(dependentVariable) <- c(dependentVariableName)
  
  diagnostic <- data.frame(diagnostic, dependentVariable)
  
  resid <- resid(fit)
  diagnostic <- data.frame(diagnostic, resid)
  stz.r <- rstandard(fit)
  diagnostic <- data.frame(diagnostic, stz.r)
  stu.r <- rstudent(fit)
  diagnostic <- data.frame(diagnostic, stu.r)
  cooks <- cooks.distance(fit)
  diagnostic <- data.frame(diagnostic, cooks)
  dfbeta <- dfbeta(fit)
  diagnostic <- data.frame(diagnostic, dfbeta)
  dffit <- dffits(fit)
  diagnostic <- data.frame(diagnostic, dffit)
  leverage <- hatvalues(fit)
  diagnostic <- data.frame(diagnostic, leverage)
  cov.rat <- covratio(fit)
  diagnostic <- data.frame(diagnostic, cov.rat)
  fitted <- fitted(fit)
  diagnostic <- data.frame(diagnostic, fitted)
  sqrt.abs.stz.r <- sqrt(abs(diagnostic$stz.r))
  diagnostic <- data.frame(diagnostic, sqrt.abs.stz.r)
  
  return (diagnostic)
}

#' Find Missing Values
#'
#' Check out all dataset to find missing values. Return a data frame, with the 
#' possition of each missing value.
#' 
#' @param dataSet an object of class Data.Frame with a dataset.
#' @return an integer(0) when no null values, or a Data.Frame with all null values
#' identified by their positions (i,j)
#' @seealso removeRowsMissing
#' @examples
#' NA_values <- findMissingValues(iris)
#' 
#' if (any(NA_values)) { #Validation if missing values exist
#'    #Do something with your missing values
#' }
findMissingValues <- function (dataSet) {
  
  if (missing(dataSet)) {
    stop("Need to specify dataSet")
  }
  
  #All parameters are OK!
  data <- data.frame(i=integer(), j=integer(), stringsAsFactors=FALSE)
  count <- 0
  for (i in 1:nrow(dataSet)) {
    vector <- which(is.na(dataSet[i,]))
    if (any(vector)) {
      #The vector has null values!!!
      for (j in 1:length(vector)) {
        data[nrow(data)+1,1] <- i
        data[nrow(data),2] <- vector[j]
      }
      count <- count + length(vector)
    }
  }
  
  if (count == 0) {
    return (integer(0))
  }
  else {
    return (data)
  }
}

#' Remove Missing Values of Data Set
#'
#' Delete all rows of data set that contains a missing value.
#' 
#' @param missingValues an object of class Data.Frame with the possition (i,j) of 
#' each missing value.
#' @param dataSet an object of class Data.Frame with the original Data Set.
#' @return an object of class Data.Frame with a modified Data Set without missing 
#' values.
#' @seealso findMissingValues
#' @examples
#' NA_values <- findMissingValues(iris)
#' 
#' if (any(NA_values)) { #Validation if missing values exist
#'    iris <- removeRowsMissing(NA_values, iris) #Remove all rows with missing values
#' }
removeRowsMissing <- function (missingValues, dataSet) {
  
  if (missing(missingValues)) {
    stop("Need to specify missingValues!")
  }
  
  if (missing(dataSet)) {
    stop("Need to specify dataSet")
  }
  
  #All parameters are OK!
  for (i in 1:nrow(missingValues)) {
    dataSet <- dataSet[-missingValues[i,1],] #Remove row
  }
  
  return (dataSet)
}