#' Calculate Diff
#'
#' Function to calculate de variation of a dataSet.
#' 
#' @param dataSet an object of class data frame with a data set.
#' @param inf an integer that represent the first column that you want review.
#' @param sup an integer that represent the last column that you want review.
#' @return an object of class data frame with a variation of all data set.
#' @seealso getColumnsNoise
#' @examples
#' iris.x <- iris[,1:4]
#' diffValues <- calculateDiff(iris.x)
calculateDiff <- function(dataSet, inf, sup) {
  
  if (missing(dataSet)) {
    stop("Need to specify dataSet!")
  }
  if (missing(inf) || missing(sup)) {
    inf <- 1
    sup <- ncol(dataSet)
  }
  if (inf > sup) {
    stop("inf must be less than sup!")
  }
  
  #All parameters are OK!
  resp <- data.frame(matrix(ncol = ncol(dataSet), nrow = nrow(dataSet)))
  
  for (i in 1:nrow(dataSet)) {
    before <- dataSet[i,1]
    for (j in 1:ncol(dataSet)) {
      if (j >= inf && j <= sup) {
        resp[i,j] <- abs(dataSet[i,j] - before) / before
      }
      
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
#' @param inf an integer that represent the first column that you want review.
#' @param sup an integer that represent the last column that you want review.
#' @return an object of class data frame all columns that contains noise.
#' @seealso getColumnsNoise
#' @examples
#' iris.x <- iris[,1:4]
#' diffValues <- calculateDiff(iris.x)
#' limit = 0.15
#' columnsNoise <- getColumnsNoise(diffValues, limit)
getColumnsNoise <- function(data, limit, inf, sup) {
  
  if (missing(data)) {
    stop("Need to specify data!")
  }
  if (missing(limit)) {
    stop("Need to specify limit!")
  }
  if (missing(inf) || missing(sup)) {
    inf <- 1
    sup <- ncol(dataSet)
  }
  if (inf > sup) {
    stop("inf must be less than sup!")
  }
  
  #All parameters are OK!
  noise <- data.frame(j=integer(),stringsAsFactors=FALSE)
  
  for (j in 1:ncol(data)) {
    if (j >= inf && j <= sup) {
      x <- data[,j]
      aux <- count(x >= limit)
      
      for (i in 1:nrow(aux)) {
        if (aux[i,1]==TRUE) {
          noise[nrow(noise)+1,1] <- j
        }
      }
    }
  }
  
  return (noise)
}