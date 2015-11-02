#' Calculate Diff
#'
#' Function to calculate de variation of a dataSet.
#' 
#' @param dataSet an object of class data frame with a data set.
#' @return an object of class data frame with a variation of all data set.
#' @seealso getColumnsNoise
#' @examples
#' iris.x <- iris[,1:4]
#' diffValues <- calculateDiff(iris.x)
calculateDiff <- function(dataSet) {
  
  if (missing(dataSet)) {
    stop("Need to specify dataSet!")
  }
  
  #All parameters are OK!
  resp <- data.frame(matrix(ncol = ncol(dataSet), nrow = nrow(dataSet)))
  
  for (i in 1:nrow(dataSet)) {
    before <- dataSet[i,1]
    for (j in 1:ncol(dataSet)) {
      resp[i,j] <- abs(dataSet[i,j] - before) / before
      
      before <- dataSet[i,j]
    }
  }
  
  return (resp)
}

#' Columns Noise
#'
#' Function to calculate de variation of a dataSet.
#' 
#' @param data an object of class data frame with a data set of variation.
#' @param limit an double that represent the umbral for detect columns variation.
#' @return an object of class data frame all columns that contains noise.
#' @seealso getColumnsNoise
#' @examples
#' iris.x <- iris[,1:4]
#' diffValues <- calculateDiff(iris.x)
#' limit = 0.15
#' columnsNoise <- getColumnsNoise(diffValues, limit)
getColumnsNoise <- function(data, limit) {
  
  if (missing(data)) {
    stop("Need to specify data!")
  }
  if (missing(limit)) {
    stop("Need to specify limit!")
  }
  
  #All parameters are OK!
  noise <- data.frame(j=integer(),stringsAsFactors=FALSE)
  
  for (j in 1:ncol(data)) {
    x <- data[,j]
    aux <- count(x >= limit)
    
    for (i in 1:nrow(aux)) {
      if (aux[i,1]==TRUE) {
        noise[nrow(noise)+1,1] <- j
      }
    }
  }
  
  return (noise)
}