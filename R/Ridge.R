#' Ridge Model
#'
#' Generate a model using the methodology defined in PAPER.
#' 
#' @param X an object of class "matrix" with a data set containing all dependent 
#' variables.
#' @param Y an object of "numeric", "factor" or "integer" is a list of values 
#' containig the dependent variable.
#' @param 
#' 
#' @seealso RidgePredict
#' 
#' @examples
#' #Example 1
RidgeModel <- function(X, Y, lambdas, percent) {
  
  #Validation for missing parameters
  if (missing(X)) {
    stop("Need to specify X!")
  }
  if (missing(Y)) {
    stop("Need to specify Y!")
  }
  if (missing(lambdas)) {
    #We define a set of lambda values
    lambdas <- c(0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1, 2, 3, 4, 5)
  }
  if (missing(percent)) {
    percent <- 0.8
  }
  
  if (class(X) != "matrix") {
    stop("X must be a matrix class!")
  }
  if (!(class(Y) == "numeric" || class(Y) == "factor" || class(Y) == "integer")) {
    stop("Y must be a numeric, factor or integer class!")
  }
  if (!(class(lambdas) == "numeric" || class(lambdas) == "factor" || class(lambdas) == "integer")) {
    stop("Lambdas must be a numeric class!")
  }
  if (!(class(percent) == "numeric" || class(percent) == "factor" || class(percent) == "integer")) {
    if (percent>1 || percent<0) {
      stop("Percent must be a value between 0 and 1!")
    }
  }
  
  bestLambdas <- c() #Assign the empty list. Here the optimal lambdas accumulate for each training set-validation
  bestARMSE <- c()
  bestAMEAN <- c()
  bestRRMSE <- c()
  
  instances <- round(nrow(X)*percent)
  
  set.seed(2015) #The seed for random values
  
  for (i in (1:20)) #Loop for generate 20 train-test set's
  {
    indices <- sample(seq(nrow(X)), instances) # obtiene un con junto de 500 indices 
    #para seleccionar los datos de entrenamiento
    XT <- X[indices,] #Select the regressors matrix for the training sample
    YT <- Y[indices] #Select the vector of dependent variable for the training sample
    XV <- cbind(1,X[-indices,]) #Select the matrix of regressors for the validation
    #We add a columns of 1's for the constant
    YV <- Y[-indices] #Select the vector of dependent variable for the validation
    
    #Generate a ridge regression model usign the lambas provided
    myModel <- lm.ridge(YT~XT, na.action = na.omit, lambda = lambdas)
    
    results <- c() #Assign the empty list. Here we will save the correlation 
    #coefficients between YV and Y predicted obtained by applying the previous 
    #estimated model by the BETA vector for each lambda
    myCoef <- coefficients(myModel) #extracting the coefficients in a matrix with as
    #many rows as lambdas have been assessed and as many columns as regressors
    
    for (j in (1:nrow(myCoef))) #Loop for each row of myCoef
    {
      BETA <- matrix(myCoef[j,], ncol = 1) #Extract the coefficients for the j-th
      #value of lambda
      YHat <- XV%*%BETA #We obtain the predicted values for the validation set
      
      #Here is keeping the best
      xD <- YV
      yD <- YHat
      ARMSE <- sqrt(sum((yD-xD)^2)/length(yD))
      AMEAN <- mean(c(xD,yD))
      RRMSE <- 100*ARMSE/AMEAN
      
      print(paste("Instances",nrow(X)))
      print(paste("train:",length(indices)))
      print(paste("test:",(nrow(X)-length(indices))))
      
      print(paste("XT",nrow(XT)))
      print(paste("YT",length(YT)))
      print(paste("XV",nrow(XV)))
      print(paste("YV",length(YV)))

      print(paste("length(YHat):",length(YHat)))
      
      results <- rbind(results, c(cor(YHat,YV), ARMSE, AMEAN, RRMSE)) #We accumulate
      #on results the correlations between YV and YHat, and the others statistics
      
    }
    #For each lambda
    #accumulates the value of lambda that maximizes the correlation between YV and YHat
    bestLambdas <- c(bestLambdas, lambdas[which.max(results[,1])])
    bestARMSE <- c(bestARMSE, lambdas[which.max(results[,2])])
    bestAMEAN <- c(bestAMEAN, lambdas[which.max(results[,3])])
    bestRRMSE <- c(bestRRMSE, lambdas[which.max(results[,4])])
  }
  #We override the columns name of results matrix
  colnames(results) <- c("R2", "ARMSE", "AMEAN", "RRMSE")
  
  print(mean(bestLambdas))
  print(mean(bestARMSE))
  print(mean(bestAMEAN))
  print(mean(bestRRMSE))
  
  #The ridge estimates for all data model using the average of the optimal lambda
  myModel <- lm.ridge(Y~X, na.action = na.omit, lambda = mean(bestLambdas))
  
  #Calculates the predicted values for the previous model
  YHat <- cbind(1,X)%*%coefficients(myModel)
  #plot(Y,YHat) #muestra graficamente la relaciÃ³n entre el Y observado y el Y predicho (YHat)
  cor(Y,YHat)^2 #Calcultaes the coefficient of determination
  
  print(cor(Y,YHat)^2)
  
  return(myModel)
}