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
      BETA <- matrix(myCoef[j,], ncol = 1) #Extract the coefficients for the i-th
      #value of lambda
      YHat <- XV%*%BETA #We obtain the predicted values for the validation set
      
      #Here is keeping the best
      xD <- YV
      yD <- YHat
      ARMSE <- sqrt(sum((yD-xD)^2)/length(yD))
      AMEAN <- mean(c(xD,yD))
      RRMSE <- 100*ARMSE/AMEAN
      
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

RidgeModel2 <- function(X, Y, lambdas, percent) {
  X=as.matrix(X) #transforma X de un data.frame a una matriz
  #DatosB <- paste(DataPath,"Tablatrigo.txt",sep = "")
  #MatB <- read.table(paste(DatosB),header=TRUE)
  
  #RDTO <- MatB$Rdto  #selecciona la variable dependiente y la pone en el vector Y
  RDTO=Y
  Y <- RDTO
  
  milambda=c(0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,2,3,4,5)   #define un conjunto de valores de lambda (hay que jugar un poco para establecer este conjunto)
  
  #milambda=c(90)   #define un conjunto de valores de lambda (hay que jugar un poco para establecer este conjunto)
  
  # OJO OJO OJO AQUI TAMBIEN CAMBIEN c(0) por c()
  mejoreslambdas=c() #pone la lista mejoreslambdas en vacio. Aqui se iran acumulando los lambdas optimos para cada set de entrenamiento-validacion
  
  ################################################## MODIFICADO POR JULIO ##############################################
  mimejoresARMSE <- c()
  mimejoresAMEAN <- c()
  mimejoresRRMSE <- c()
  ######################################################################################################################
  #j=1
  set.seed(2015)
  
  instances <- round(nrow(X)*0.8)
  for (j in (1:20))  #inicia un loop para generar 20 sets de entrenamiento y validaciÃ³n
  {
    
    indices=sample(seq(nrow(X)),instances) # obtiene un con junto de 500 indices para seleccionar los datos de entrenamiento
    XT=X[indices,] #selecciona la matrix de regresoras  para la muestra de entrenamiento a partir de la matriz X utilizando los indices previamente creados
    YT=Y[indices]  #selecciona el vector de la variable dependiente para la muestra de entrenamiento a partir del vector Y utilizando los indices previamente creados
    XV=cbind(1,X[-indices,]) #selecciona la matrix de regresoras  para la muestra de validaciÃ³n (utiliza -indices para eliminar esos casos desde la matriz X)
    #le agrega una columna de 1's para la constante
    YV=Y[-indices] #selecciona el vector de la variable dependiente para la muestra de validaciÃ³n (utiliza -indices para eliminar esos casos desde el vecto Y)
    
    
    mimodelo=lm.ridge(YT~XT, na.action=na.omit,lambda=milambda)  #estima un modelo lineal mediante regresiÃ³n ridge, utilizando los lambdas alternativos
    #provistos en milambda
    
    resultados=c() #pone la lista resultados en vacio. Aqui se van a guardar los coeficientes de correlaciÃ³n entre el Y observado en el el conjunto de validaciÃ³n (YV) y el
    #Y predicho (YHat) obtenido aplicando el vector BETA estimado por el modelo anterior, para cada lambda
    miscoef=coefficients(mimodelo) #extrae los coeficientes en una matriz con tantas filas como lambdas se haya evaludo y tantas columnas como
    #regresoras (+1 por la constante)
    for (k in (1:nrow(miscoef)))   #loop para cada fila de miscoef
    {
      BETA=matrix(miscoef[k,],ncol=1) #extrae los coeficientes para el i-esimo valor de lambda
      YHat=XV%*%BETA                  #obtiene los valores predichos para el set de validaciÃ³n
      
      ############################################ MODIFICADO POR JULIO #################################
      # aqui esta guardando los mejores
      xD <- YV
      yD <- YHat
      ARMSE <- sqrt(sum((yD-xD)^2)/length(yD))
      AMEAN <- mean(c(xD,yD))
      RRMSE <- 100*ARMSE/AMEAN
      
      resultados=rbind(resultados,c(cor(YHat,YV),ARMSE,AMEAN,RRMSE)) #acumula en resultados las correlaciones entre el YV e YHat + lo otros estadisticos
      #         resultados=c(resultados,cor(YHat,YV)) #acumula en resultados las correlaciones entre el YV e YHat
      
    }                                      #esto cierra el loop iniciado en la linea 26. Cuando esto termina, resultados tienen los coeficientes de correlaciÃ³n
    #para cada lambda
    mejoreslambdas=c(mejoreslambdas,milambda[which.max(resultados[,1])]) #acumula el valor de lambda que maximiza la correlaciÃ³n entre YV e YHat
    mimejoresARMSE <- c(mimejoresARMSE,milambda[which.max(resultados[,2])])
    mimejoresAMEAN <- c(mimejoresAMEAN,milambda[which.max(resultados[,3])])
    mimejoresRRMSE <- c(mimejoresRRMSE,milambda[which.max(resultados[,3])])
    ############################################ MODIFICADO POR JULIO #################################
    
    
    #plot(log(milambda),resultados,ylim=c(0,1),col=3)
    
    #png("Lambdas.png")
    #par(pty="s")
    #plot(milambda,resultados)
    #lines(milambda,cor(YHat,YV))
    #dev.off()
    
  }
  
  ###############################################MODIFICADO POR JULIO
  
  #mean(mejoreslambdas)  #muestra el valor de lambda medio
  
  print(mean(mejoreslambdas))
  print(mean(mimejoresARMSE))
  print(mean(mimejoresAMEAN))
  print(mean(mimejoresRRMSE))
  #####################################################################
  
  print(mejoreslambdas)
  
  mimodelo=lm.ridge(Y~X, na.action=na.omit,lambda=mean(mejoreslambdas)) #estima el modelo ridge para todos los datos utilizando el promedio de los lambda optimos
  
  YHat=cbind(1,X)%*%coefficients(mimodelo) #calcula los valores predichos para el modelo anterior
  #plot(Y,YHat) #muestra graficamente la relaciÃ³n entre el Y observado y el Y predicho (YHat)
  cor(Y,YHat)^2 #calcula el coeficiente de determinaciÃ³n
  
  print(cor(Y,YHat)^2)
  
  return(mimodelo)
  #FIN#
}
