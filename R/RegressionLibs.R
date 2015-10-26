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
    stop("Need to specify fit!")
  }
  else {
    if (!('model' %in% names(fit))) {
      stop("fit must be a closure type")
    }
  }
  
  dependentVariableName <- "Dependent Variable"
  
  #All parameters are OK!
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
#' Check out all data set to find missing values. Return a data frame, with the 
#' possition of each missing value.
#' 
#' @param dataSet an object of class data frame with a data set.
#' @return an integer(0) when no null values, or a data frame with all null values
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
    stop("Need to specify dataSet!")
  }
  
  #All parameters are OK!
  data <- data.frame(i=integer(), j=integer(), row.name=integer(), stringsAsFactors=FALSE)
  count <- 0
  for (i in 1:nrow(dataSet)) {
    vector <- which(is.na(dataSet[i,]))
    if (any(vector)) {
      #The vector has null values!!!
      for (j in 1:length(vector)) {
        data[nrow(data)+1,1] <- i
        data[nrow(data),2] <- vector[j]
        data[nrow(data),3] <- as.integer(row.names(dataSet)[i])
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
#' @param missingValues an object of class data frame with the possition (i,j) of 
#' each missing value.
#' @param dataSet an object of class data frame with the original data set.
#' @return an object of class data frame with a modified data set without missing 
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
    stop("Need to specify dataSet!")
  }
  
  #All parameters are OK!
  names <- row.names(dataSet)
  j <- 1
  i <- 1
  while (i<=length(names) && j<=nrow(missingValues)) {
    if (as.integer(names[i])==missingValues[j,3]) { #Remove row
      dataSet <- dataSet[-i,] #Remove row
      j <- j + 1
    }
    else {
      if (as.integer(names[i]) > missingValues[j,3]) {
        j <- j + 1
      }
      i <- i + 1
    }
  }
  
  return (dataSet)
}

#' Normalize Function
#'
#' Function to normalize data set in range of 0 to 1.
#' 
#' @param dataSet an object of class data frame with the original data set.
#' @return an object of class data frame with a modified data set normalized 
#' in range of 0 to 1.
#' @seealso scaleData, normalizeData
#' @examples
#' iris.x <- iris[,1:4]
#' normedIris <- as.data.frame(lapply(iris.x, normalize)) #In range [0,1]
normalize <- function(dataSet){
  
  if (missing(dataSet)) {
    stop("Need to specify dataSet!")
  }
  
  #All parameters are OK!
  (dataSet - min(dataSet, na.rm=TRUE))/(max(dataSet,na.rm=TRUE) - min(dataSet, na.rm=TRUE))
}

#' Scale Function
#'
#' Function to scale the data set. If you use normalize function first and then 
#' scaleData function you could obtain a normalize dataset in a range [x,y]
#' 
#' @param dataSet an object of class data frame with the original data set.
#' @param min an integer with the min value that you want scale the data set.
#' @param max an integer with the max value that you want scale the data set.
#' @return an object of class data frame with a modified data set scaled 
#' in the defined range.
#' @seealso normalize, normalizeData
#' @examples
#' iris.x <- iris[,1:4]
#' normed <- as.data.frame(lapply(iris.x, normalize))
#' normedIris <- as.data.frame(lapply(normed, 1, 10, scaleData)) #In range [1,10]
scaleData <- function(dataSet, min, max){
  
  if (missing(dataSet)) {
    stop("Need to specify dataSet!")
  }
  if (missing(min)) {
    stop("Need to specify min!")
  }
  if (missing(max)) {
    stop("Need to specify max!")
  }
  if (min >= max) {
    stop("Min must be less strict than max!")
  }
  
  #All parameters are OK!
  (dataSet * (max - min)) + min
}

#' Normalize Data Set
#'
#' Function to normalize a data set. You could normalize in any range [min,max], but
#' if you don't specify this values, this function make a normalization in range
#' [0,1].
#' 
#' @param dataSet an object of class data frame with the original data set that you 
#' want normalize.
#' @param min an integer with the min value that you want normalize the data set.
#' @param max an integer with the max value that you want normalize the data set.
#' @return an object of class data frame with a modified data set normalized 
#' in the defined range.
#' @seealso normalize, scaleData
#' @examples
#' iris.x <- iris[,1:4]
#' normedIris <- normalizeData(iris) #In range [0,1]
#' normedIris <- normalizeData(iris, 1, 10) #In ragen [1,10]
normalizeData<- function(dataSet, min, max){
  
  if (missing(dataSet)) {
    stop("Need to specify dataSet!")
  }
  
  #All parameters are OK!
  try(
    if (!missing("x")) {
      try(
        if (!missing("y")) {
          normed <- as.data.frame(lapply(dataSet, normalize))
          as.data.frame(lapply(normed, min, max, scaleData))
        }, silent = TRUE)
    }, silent = TRUE)
  
  as.data.frame(lapply(dataSet, normalize))
}

#' Calculate Variance Function
#'
#' Function to calculate de variance of an specific column.
#' 
#' @param dataSet an object of class data frame with a data set that you want 
#' calculate the variance.
#' @param col an integer that represent the column that you want calculate the
#' variance.
#' @return an object of class data frame with a modified data set with variance
#' calculated.
#' @seealso linePlot
#' @examples
#' iris.x <- iris[,1:4]
#' ir.pca <- prcomp(iris.x, center = TRUE, scale. = TRUE)
#' rowsData <- length(ir.pca$sdev)
#' seqRow <- seq(from = 1, to = rowsData, length.out = rowsData)
#' dataPlot <- data.frame(seqRow, ir.pca$sdev)
#' dataPlot <- CalculateVariance(dataPlot, 2)
CalculateVariance <- function(dataSet, col) {
  
  if (missing(dataSet)) {
    stop("Need to specify dataSet!")
  }
  if (missing(col)) {
    stop("Need to specify col!")
  }
  
  #All parameters are OK!
  for (i in 1:nrow(dataSet)) {
    dataSet[i,col] <- dataSet[i,col]*dataSet[i,col]
  }
  return (dataSet)
}

#' Make Pairs Function
#'
#' Function that generate a data frame with the data used for ggplot function for
#' make a scatterplot matrix.
#' 
#' @param dataSet an object of class data frame with a data set.
#' @return an object of class data frame with the data used for ggplot function.
#' @seealso ScatterplotMatrix
#' @source https://gastonsanchez.wordpress.com/2012/08/27/scatterplot-matrices-with-ggplot/
#' @examples
#' iris.x <- iris[,1:4]
#' ir.pca <- prcomp(iris.x, center = TRUE, scale. = TRUE)
#' gg1 <- makePairs(ir.pca$x)
makePairs <- function(dataSet){
  
  if (missing(dataSet)) {
    stop("Need to specify dataSet!")
  }
  
  #All parameters are OK!
  grid <- expand.grid(x = 1:ncol(dataSet), y = 1:ncol(dataSet))
  grid <- subset(grid, x != y)
  all <- do.call("rbind", lapply(1:nrow(grid), function(i) {
    xcol <- grid[i, "x"]
    ycol <- grid[i, "y"]
    data.frame(xvar = names(dataSet)[ycol], yvar = names(dataSet)[xcol], 
               x = dataSet[, xcol], y = dataSet[, ycol], dataSet)
  }))
  all$xvar <- factor(all$xvar, levels = names(dataSet))
  all$yvar <- factor(all$yvar, levels = names(dataSet))
  densities <- do.call("rbind", lapply(1:ncol(dataSet), function(i) {
    data.frame(xvar = names(dataSet)[i], yvar = names(dataSet)[i], x = dataSet[, i])
  }))
  list(all = all, densities = densities)
}

removeRowsByRowName <- function (remove, dataSet) {
  
  if (missing(remove)) {
    stop("Need to specify remove!")
  }
  if (missing(dataSet)) {
    stop("Need to specify dataSet!")
  }
  
  #All parameters are OK!
  names <- row.names(dataSet)
  sort(remove)
  j <- 1
  i <- 1
  while (i<=length(names) && j<=length(remove)) {
    if (as.integer(names[i])==as.integer(remove[j])) { #Remove row
      dataSet <- dataSet[-i,] #Remove row
      j <- j + 1
    }
    else {
      if (as.integer(names[i]) > as.integer(remove[j])) {
        j <- j + 1
      }
      i <- i + 1
    }
  }
  
  return (dataSet)
}