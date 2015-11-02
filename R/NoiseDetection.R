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

getColumnsNoise <- function(dataSet, limit) {
  
  if (missing(dataSet)) {
    stop("Need to specify dataSet!")
  }
  if (missing(limit)) {
    stop("Need to specify limit!")
  }
  
  #All parameters are OK!
  noise <- data.frame(j=integer(),stringsAsFactors=FALSE)
  
  for (i in 1:nrow(dataSet)) {
    for (j in 1:ncol(dataSet)) {
      if (dataSet[i,j] >= limit) {
        noise[nrow(noise)+1,1] <- j
      }
    }
  }
  
  return (noise)
}