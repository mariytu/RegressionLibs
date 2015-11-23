#' Elbow Plot for PCA (Plot)
#'
#' Generate a plot of 10 first variances of Principal Components. This is useful to 
#' determinate which are the most important components.
#' 
#' @param data.pca a list with class "prcomp" containing all principal components 
#' calculated.
#' @seealso CalculateVariance, plotPC
#' @examples
#' #Example 1
#' iris.x <- iris[,1:4] # These are the independent variables
#' # We know that there are no missing values in the data set
#' 
#' # performing prcomp
#' ir.pca <- prcomp(iris.x, center = TRUE, scale. = TRUE) 
#' 
#' # Generating elbow plot to detect the most important principal components
#' elbowPlot(ir.pca)
#' 
#' 
#' #Example 2
#' # Getting a clean data set (without missing values)
#' cars <- read.csv("https://dl.dropboxusercontent.com/u/12599702/autosclean.csv", sep = ";", dec = ",")
#' cars.x <- cars[,1:16] # These are the independent variables
#' 
#' # Performing prcomp
#' cars.pca <- prcomp(cars.x, center = TRUE, scale. = TRUE)
#' 
#' # Generating elbow plot to detect the most important principal components
#' elbowPlot(cars.pca)
elbowPlot <- function(data.pca) {
  
  if (missing(data.pca)) {
    stop("Need to specify data.pca!")
  }
  if (class(data.pca) != "prcomp") {
    stop("data.pca must be a prcomp class!")
  }
  
  #All parameters are OK!
  rowsData <- length(data.pca$sdev)
  seqRow <- seq(from = 1, to = rowsData, length.out = rowsData)
  
  dataPlot <- data.frame(seqRow, data.pca$sdev)
  names(dataPlot) <- c("PCA", "Variances")
  if (nrow(dataPlot)>10) {
    dataPlot <- dataPlot[1:10,]
  }
  dataPlot <- CalculateVariance(dataPlot, 2)
  
  p <- ggplot(data = dataPlot, aes(x = PCA, y = Variances, group = 1)) +
    geom_line(colour = "dodgerblue4", alpha = 0.5, size = 1) +
    geom_point(colour = "dodgerblue4", size = 2, alpha = 0.5) +
    expand_limits(y = 0) +
    xlab("PCs") + ylab("Variances") +
    scale_x_continuous(breaks = dataPlot$PCA) +
    theme(panel.grid.minor = element_blank(), #remove gridlines
          legend.position = "bottom" #legend at the bottom
    )#end theme
  
  return (p)
}

#' Scatterplot Matrix (Plot)
#'
#' Generate a Scatterplot Matrix of some columns of data set using ggplot.
#' 
#' @param data an object of class "data.frame" containing just numerical columns.
#' @param columns an object of class "numeric" containing the list of columns
#' that you want in your scatterplot.
#' @param dependentVariable an object of class "numeric", "factor" or "integer" is 
#' a list of values containig the dependent variable.
#' @param dependentVariableName is an optional parameter. It's an string that
#' contains the name of your dependent variable.
#' @param pointSize is an optional parameter of class numeric with a single value 
#' that represent the point size of plot.
#' @param alphaPoint is an optional parameter of class numeric with a single value 
#' that represent the alpha of points in the plot.
#' @seealso makePairs
#' @source https://gastonsanchez.wordpress.com/2012/08/27/scatterplot-matrices-with-ggplot/
#' @examples
#' #Example 1
#' iris.x <- iris[,1:4] # These are the independent variables
#' Species <- iris[,5] # This is the dependent variable
#' 
#' # An Scatterplot of all columns
#' ScatterplotMatrix(iris.x, c(1,2,3,4), Species, "Species")
#' # An Scatterplot of somes columns and different point size and alpha point
#' ScatterplotMatrix(iris.x, c(2,4), Species, "Species", 2, 1)
#' 
#' 
#' #Example 2
#' # Getting a clean data set (without missing values)
#' cars <- read.csv("https://dl.dropboxusercontent.com/u/12599702/autosclean.csv", sep = ";", dec = ",")
#' cars.x <- cars[,1:16] # These are the independent variables
#' cars.y <- cars[,17] # This is the dependent variable
#' 
#' # An Scatterplot of some columns
#' ScatterplotMatrix(cars.x, seq(3, 8, 1), cars.y, "Price")
#' # An Scatterplot of somes columns and different point size and alpha point
#' ScatterplotMatrix(cars.x, c(2,4), cars.y, "Price", 2, 1)
#' 
#' 
#' #Example 3
#' # Getting a clean data set (without missing values)
#' cars <- read.csv("https://dl.dropboxusercontent.com/u/12599702/autosclean.csv", sep = ";", dec = ",")
#' cars.x <- cars[,1:16] # These are the independent variables
#' cars.y <- cars[,17] # This is the dependent variable
#' 
#' # Performing prcomp
#' cars.pca <- prcomp(cars.x, center = TRUE, scale. = TRUE)
#' 
#' # An Scatterplot of some columns of principal components
#' ScatterplotMatrix(as.data.frame(cars.pca$x), seq(1, 4, 1), cars.y, "Price")
ScatterplotMatrix <- function(data, columns, dependentVariable, dependentVariableName, pointSize, alphaPoint){
  
  if (missing(data)) {
    stop("Need to specify data!")
  }
  if (class(data) != "data.frame") {
    stop("data must be a data.frame class!")
  }
  if (missing(columns)) {
    stop("Need to specify columns!")
  }
  if (class(columns) != "numeric") {
    stop("columns must be a numeric class!")
  }
  if (missing(dependentVariable)) {
    stop("Need to specify dependentVariable!")
  }
  if (!(class(dependentVariable) == "numeric" || class(dependentVariable) == "factor" || class(dependentVariable) == "integer")) {
    stop("dependentVariable must be a numeric, factor or integer class!")
  }
  if (missing(dependentVariableName)) {
    dependentVariableName <- "Dependent Variable"
  }
  if (class(dependentVariableName) != "character") {
    stop("dependentVariableName must be a character class!")
  }
  if (missing(pointSize)) {
    pointSize <- 1
  }
  if (class(pointSize) != "numeric") {
    stop("pointSize must be a numeric class!")
  }
  if (missing(alphaPoint)) {
    alphaPoint <- 0.5
  }
  if (class(alphaPoint) != "numeric") {
    stop("alphaPoint must be a numeric class!")
  }
  
  #All parameters are OK!
  # expand data frame for pairs plot
  subData <- as.data.frame(data[,columns])
  gg1 <- makePairs(subData)
  
  #New data frame mega Data from..to
  mega_Data <- data.frame(gg1$all, DependentVariable = rep(dependentVariable, length = nrow(gg1$all)))
  DependentVariable <- rep(dependentVariable, length = nrow(gg1$all))
  
  # pairs plot
  if (class(dependentVariable) == "numeric" || class(dependentVariable) == "integer") {
    p <- ggplot(mega_Data, aes_string(x = "x", y = "y")) + 
      facet_grid(xvar ~ yvar, scales = "free") + 
      geom_point(aes(colour = DependentVariable), na.rm = TRUE, alpha = alphaPoint, size = pointSize) + 
      stat_density(aes(x = x, y = ..scaled.. * diff(range(x)) + min(x)), 
                   data = gg1$densities, position = "identity", 
                   colour = "dodgerblue4", geom = "line", size = 1, alpha = 0.5) + 
      scale_color_gradientn(name = dependentVariableName,
                            colours = c("darkred", "yellow", "darkgreen")) + #set the pallete
      theme(panel.grid.minor = element_blank(), #remove gridlines
            legend.position = "bottom", #legend at the bottom
            axis.title.x = element_blank(), #remove x label
            axis.title.y = element_blank()  #remove y label
      )#end theme
  }
  else {
    p <- ggplot(mega_Data, aes_string(x = "x", y = "y")) + 
      facet_grid(xvar ~ yvar, scales = "free") + 
      geom_point(aes(colour = DependentVariable), na.rm = TRUE, alpha = alphaPoint, size = pointSize) + 
      stat_density(aes(x = x, y = ..scaled.. * diff(range(x)) + min(x)), 
                   data = gg1$densities, position = "identity", 
                   colour = "dodgerblue4", geom = "line", size = 1, alpha = 0.5) + 
      scale_color_discrete(name = dependentVariableName) +
      theme(panel.grid.minor = element_blank(), #remove gridlines
            legend.position = "bottom", #legend at the bottom
            axis.title.x = element_blank(), #remove x label
            axis.title.y = element_blank()  #remove y label
      )#end theme
  }
  
  return (p)
}

#' Parallel Plot (Plot)
#'
#' Generate a plot of the columns of a data set for all or a range of instances. In 
#' some cases this is useful to identify some patron.
#' 
#' @param data an object of class "data.frame" containing just numerical columns.
#' @param rows an object of class "numeric" containing the list of rows
#' that you want in your parallel plot.
#' @param columns an object of class "numeric" containing the list of columns
#' that you want in your parallel plot.
#' @param dependentVariable an object of class "numeric", "factor" or "integer" is 
#' a list of values containig the dependent variable.
#' @param dependentVariableName is an optional parameter. It's an string that
#' contains the name of your dependent variable.
#' @param lineSize is an optional parameter of class numeric with a single value 
#' that represent the line size of plot.
#' @param alphaLine is an optional parameter of class numeric with a single value 
#' that represent the alpha of lines in the plot.
#' @param x_lab a boolean that represent if you want or not the x axis scale. In 
#' some cases, when you have many columns the plot could be ugly! The default value 
#' is False.
#' 
#' @examples
#' #Example 1
#' iris.x <- iris[,1:4] # These are the independent variables
#' Species <- iris[,5] # This is the dependent variable
#' 
#' # An ParallelPlot of all rows
#' ParallelPlot(iris.x, seq(1,nrow(iris.x),1), Species, "Species", 1, 0.5, TRUE)
ParallelPlot <- function(data, rows, columns, dependentVariable, dependentVariableName, lineSize, alphaLine, x_lab) {
  if (missing(data)) {
    stop("Need to specify data!")
  }
  if (class(data) != "data.frame") {
    stop("data must be a data.frame class!")
  }
  if (missing(rows)) {
    stop("Need to specify rows!")
  }
  if (class(rows) != "numeric") {
    stop("rows must be a numeric class!")
  }
  if (missing(columns)) {
    stop("Need to specify rows!")
  }
  if (class(columns) != "numeric") {
    stop("rows must be a numeric class!")
  }
  if (missing(dependentVariable)) {
    stop("Need to specify dependentVariable!")
  }
  if (!(class(dependentVariable) == "numeric" || class(dependentVariable) == "factor" || class(dependentVariable) == "integer")) {
    stop("dependentVariable must be a numeric or factor class!")
  }
  if (missing(dependentVariableName)) {
    dependentVariableName <- "Dependent Variable"
  }
  if (class(dependentVariableName) != "character") {
    stop("dependentVariableName must be a character class!")
  }
  if (missing(lineSize)) {
    lineSize <- 1
  }
  if (class(lineSize) != "numeric") {
    stop("lineSize must be a numeric class!")
  }
  if (missing(alphaLine)) {
    alphaLine <- 0.9
  }
  if (class(alphaLine) != "numeric") {
    stop("alphaLine must be a numeric class!")
  }
  if (missing(x_lab)) {
    x_lab = FALSE
  }
  if(class(x_lab) != "logical") {
    stop("x_lab must be a logical class!")
  }
  
  #All parameters are OK!
  x_name = "Columns"
  subData <- data[rows,columns]
  dependentVariable <- dependentVariable[rows]
  rowsNum <- nrow(subData)
  x <- seq(from = 1, to = rowsNum, length.out = rowsNum)
  data <- data.frame(x, subData)
  dataPlot <- melt(data, id = "x")
  dataPlot <- data.frame(dataPlot, dependentVariable)
  
  if (class(dependentVariable) == "numeric" || class(dependentVariable) == "integer") {
    if (x_lab) {
      p <- ggplot(dataPlot, aes(variable, value, group = x, colour = dependentVariable)) +
        geom_line(size = lineSize, alpha = alphaLine) +
        scale_color_gradientn(name = dependentVariableName,
                              colours = c("darkred", "yellow", "darkgreen")) +
        xlab(x_name) + ylab("Values")
    }
    else {
      p <- ggplot(dataPlot, aes(variable, value, group = x, colour = dependentVariable)) +
        geom_line(size = lineSize, alpha = alphaLine) +
        scale_color_gradientn(name = dependentVariableName,
                              colours = c("darkred", "yellow", "darkgreen")) +
        scale_x_discrete(breaks = c()) +
        xlab(x_name) + ylab("Values")
    }
  }
  else {
    if (x_lab) {
      p <- ggplot(dataPlot, aes(variable, value, group = x, colour = dependentVariable)) +
        geom_line(size = lineSize, alpha = alphaLine) +
        scale_color_discrete(name = dependentVariableName) +
        xlab(x_name) + ylab("Values")
    }
    else {
      p <- ggplot(dataPlot, aes(variable, value, group = x, colour = dependentVariable)) +
        geom_line(size = lineSize, alpha = alphaLine) +
        scale_color_discrete(name = dependentVariableName) +
        scale_x_discrete(breaks = c()) +
        xlab(x_name) + ylab("Values")
    }
  }
  
  return (p)
}

#' Density Plot (Plot)
#'
#' Generate a density plot for a specific column of the data.
#' 
#' @param data an object of class data frame with the a data.
#' @param col an integer that specify the column that you want for make the plot.
#' @seealso http://www.rdatamining.com/examples/outlier-detection
#' @examples
#' library(RegressionLibs)
#' library(Rlof) #Outlier detection library
#' 
#' iris.x <- iris[,1:4] #Get just numercial data
#' outlier.scores <- lof(iris.x, k = 5) #applying outlier detection
#' outlier.scores<-data.frame(outlier.scores)
#' DensityPlot(outlier.scores, 1) #Generating a plot of outliers scores
#' 
#' 
#' library(Rlof) #Outlier detection library
#' iris.x <- iris[,1:4] #Get just numercial data
#' outlier.scores <- lof(iris.x, k = c(5:10)) #applying outlier detection
#' mean <- rowMeans(outlier.scores) #Calculating the mean of every execution
#' outlier.scores<-data.frame(outlier.scores, mean) #adding mean to data frame
#' DensityPlot(outlier.scores, ncol(outlier.scores)) #Generating a plot of outliers scores
DensityPlot <- function(data, col) {
  
  if (missing(data)) {
    stop("Need to specify data!")
  }
  if (missing(col)) {
    stop("Need to specify col!")
  }
  if (col > ncol(data)) {
    stop("Col value must be less than ncol of data!")
  }
  
  #All parameters are OK!
  names(data)[col] <- "mean"
  
  p <- ggplot(data, aes(x = mean)) +
    geom_density(colour="darkgreen", fill="darkgreen", alpha=0.3) + 
    xlab("Values") + 
    ylab("Density")
  
  return (p)
}

#' Plot PC 3D (3DPlot)
#'
#' Generate a 3D plot for a range of principal components.
#' 
#' @param data.pca a list with class "prcomp" containing all principal components 
#' calculated.
#' @param from an integer that represent the first principal component that you 
#' want in the plot.
#' @param to an integer that represent the last principal component that you 
#' want in the plot.
#' @param dependentVariable is a list of values containig the dependent variable 
#' of your regression model.
#' @examples
#' iris.x <- iris[,1:3]
#' Petal.Width <- iris[,4]
#' ir.pca <- prcomp(iris.x, center = TRUE, scale. = TRUE)
#' PlotPC3D(ir.pca, 1, 3, Petal.Width)
PlotPC3D<- function(data.pca, from, to, dependentVariable){
  PCAfromTO<-as.data.frame(data.pca$x[,from:to])
  
  PC1 <- PCAfromTO[,1]
  PC2 <- PCAfromTO[,2]
  PC3 <- PCAfromTO[,3]
  x_lab <- paste(c("PC", from), collapse = "")
  y_lab <- paste(c("PC", (from+1)), collapse = "")
  z_lab <- paste(c("PC", to), collapse = "")
  
  cols <- myColorRamp(c("darkred", "yellow", "darkgreen"), dependentVariable)
  plot3d(x = PC1, y = PC2, z = PC3, col = cols, size = "4", xlab = x_lab, ylab = y_lab, zlab = z_lab)
}

#' Plot PCA (Plot)
#'
#' Generate a plot of 2 Principal Components using ggplot. You must indicate which 
#' PC you want in the graph.
#' 
#' @param data.pca a list with class "prcomp" containing all principal components 
#' calculated.
#' @param dependentVariable is a list of values containig the dependent variable 
#' of your regression model.
#' @param x_axis an integer that represent the number of the principal component 
#' that you want in your x axis.
#' @param y_axis an integer that represent the number of the principal component 
#' that you want in your y axis.
#' @param dependentVariableName is an optional parameter. It's an string that
#' contains de name of your dependent variable of your regression model.
#' @seealso linePlot
#' @examples
#' iris.x <- iris[,1:3]
#' Petal.Width <- iris[,4]
#' ir.pca <- prcomp(iris.x, center = TRUE, scale. = TRUE)
#' 
#' plotPC(ir.pca, Petal.Width, 1, 2, "Petal Width")
plotPC <- function(data.pca, dependentVariable, x_axis, y_axis, dependentVariableName) {
  
  if (missing("data.pca")) {
    stop("Need to specify data.pca!")
  }
  if (missing("dependentVariable")) {
    stop("Need to specify dependentVariable!")
  }
  if (missing("x_axis")) {
    stop("Need to specify x_axis!")
  }
  if (missing("y_axis")) {
    stop("Need to specify y_axis!")
  }
  if (missing("dependentVariableName")) {
    dependentVariableName <- "Dependent Variable"
  }
  
  #All parameters are OK!
  PCs <- data.frame(data.pca$x[,x_axis], data.pca$x[,y_axis], dependentVariable)
  x_axis <- paste(c("PC", x_axis), collapse = "")
  y_axis <- paste(c("PC", y_axis), collapse = "")
  names(PCs) <- c(x_axis, y_axis, "DependentVariable")
  
  p <- ggplot(PCs, aes_string(x = x_axis, y = y_axis)) + 
    geom_point(aes(colour = dependentVariable), na.rm = TRUE, alpha = 0.8, size = 2) + 
    scale_color_gradientn(name = PCs$DependentVariable,
                          colours = c("darkred", "yellow", "darkgreen")) + #set the pallete
    theme(panel.grid.minor = element_blank(), #remove gridlines
          legend.position = "bottom" #legend at the bottom
    )#end theme
  
  return (p)
}