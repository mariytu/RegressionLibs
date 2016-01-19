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
#' @param colours is an optional parameter of class character with a list of colours 
#' to use in the plot. The default value for continuos dependent variable is 
#' c("darkred", "yellow", "darkgreen") and for categorical dependent variable is 
#' "dodgerblue4"
#' @seealso makePairs
#' @source https://gastonsanchez.wordpress.com/2012/08/27/scatterplot-matrices-with-ggplot/
#' @examples
#' #Example 1
#' iris.x <- iris[,1:4] # These are the independent variables
#' Species <- iris[,5] # This is the dependent variable
#' 
#' # A Scatterplot of all columns
#' ScatterplotMatrix(iris.x, c(1,2,3,4), Species, "Species")
#' # A Scatterplot of somes columns and different point size and alpha point
#' ScatterplotMatrix(iris.x, c(2,4), Species, "Species", 2, 1)
#' 
#' 
#' #Example 2
#' # Getting a clean data set (without missing values)
#' cars <- read.csv("https://dl.dropboxusercontent.com/u/12599702/autosclean.csv", sep = ";", dec = ",")
#' cars.x <- cars[,1:16] # These are the independent variables
#' cars.y <- cars[,17] # This is the dependent variable
#' 
#' # A Scatterplot of some columns
#' ScatterplotMatrix(cars.x, seq(3, 8, 1), cars.y, "Price")
#' # A Scatterplot of somes columns and different point size and alpha point
#' ScatterplotMatrix(cars.x, c(2,4), cars.y, "Price", 2, 1)
#' # A Scatterplot with a different colours palette
#' myPalette <- c("darkolivegreen4", "goldenrod1", "dodgerblue4")
#' ScatterplotMatrix(cars.x, c(2,4), cars.y, "Price", colours = myPalette)
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
#' # A Scatterplot of some columns of principal components
#' ScatterplotMatrix(as.data.frame(cars.pca$x), seq(1, 4, 1), cars.y, "Price")
ScatterplotMatrix <- function(data, columns, dependentVariable, dependentVariableName, pointSize, alphaPoint, colours){
  
  if (missing(data)) {
    stop("Need to specify data!")
  }
  if (class(data) != "data.frame") {
    stop("data must be a data.frame class!")
  }
  if (missing(columns)) {
    stop("Need to specify columns!")
  }
  if (!(class(columns) == "numeric" || class(columns) == "integer")) {
    stop("columns must be a numeric or integer class!")
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
  if (missing(colours)) {
    if (class(dependentVariable) == "numeric" || class(dependentVariable) == "integer") {
      colours <- c("darkred", "yellow", "darkgreen")
    }
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
                            colours = colours) + #set the pallete
      theme(panel.grid.minor = element_blank(), #remove gridlines
            legend.position = "bottom", #legend at the bottom
            axis.title.x = element_blank(), #remove x label
            axis.title.y = element_blank()  #remove y label
      )#end theme
  }
  else {
    if (missing(colours)) {
      p <- ggplot(mega_Data, aes_string(x = "x", y = "y")) + 
        facet_grid(xvar ~ yvar, scales = "free") + 
        geom_point(aes(colour = DependentVariable), na.rm = TRUE, alpha = alphaPoint, size = pointSize) + 
        stat_density(aes(x = x, y = ..scaled.. * diff(range(x)) + min(x)), 
                     data = gg1$densities, position = "identity", 
                     colour = "dodgerblue4", geom = "line", size = 1, alpha = 0.5) + 
        scale_color_discrete(name = dependentVariableName) +
        #scale_color_brewer(palette = 1) + 
        #scale_color_manual(values = colours) +
        theme(panel.grid.minor = element_blank(), #remove gridlines
              legend.position = "bottom", #legend at the bottom
              axis.title.x = element_blank(), #remove x label
              axis.title.y = element_blank()  #remove y label
        )#end theme
    } else {
      p <- ggplot(mega_Data, aes_string(x = "x", y = "y")) + 
        facet_grid(xvar ~ yvar, scales = "free") + 
        geom_point(aes(colour = DependentVariable), na.rm = TRUE, alpha = alphaPoint, size = pointSize) + 
        stat_density(aes(x = x, y = ..scaled.. * diff(range(x)) + min(x)), 
                     data = gg1$densities, position = "identity", 
                     colour = "dodgerblue4", geom = "line", size = 1, alpha = 0.5) + 
        scale_color_manual(values = colours) +
        theme(panel.grid.minor = element_blank(), #remove gridlines
              legend.position = "bottom", #legend at the bottom
              axis.title.x = element_blank(), #remove x label
              axis.title.y = element_blank()  #remove y label
        )#end theme
    }
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
#' # A ParallelPlot of all rows and all columns
#' ParallelPlot(iris.x, seq(1,nrow(iris.x),1), seq(1,ncol(iris.x),1), Species, "Species", 1, 0.5, TRUE)
#' # A ParallelPlot of all rows and some columns
#' ParallelPlot(iris.x, seq(1,nrow(iris.x),1), c(3,4), Species, "Species", 1, 0.5, TRUE)
#' 
#' 
#' #Example 2
#' # Getting a clean data set (without missing values)
#' cars <- read.csv("https://dl.dropboxusercontent.com/u/12599702/autosclean.csv", sep = ";", dec = ",")
#' cars.x <- cars[,1:16] # These are the independent variables
#' cars.y <- cars[,17] # This is the dependent variable
#' 
#' # A ParallelPlot of all rows and all columns
#' ParallelPlot(cars.x, seq(1,nrow(cars.x),1), seq(1,ncol(cars.x),1), cars.y, "Price", 1, 0.5, TRUE)
#' # A ParallelPlot of all rows and some columns
#' ParallelPlot(cars.x, seq(1,nrow(cars.x),1), c(1,2,5,8,13,14), cars.y, "Price", 1, 0.8, TRUE)
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
  if (!(class(rows) == "numeric" || class(rows) == "integer")) {
    stop("rows must be a numeric or integer class!")
  }
  if (missing(columns)) {
    stop("Need to specify columns!")
  }
  if (!(class(columns) == "numeric" || class(columns) == "integer")) {
    stop("columns must be a numeric or integer class!")
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
  x_name = "Wavelength"
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
#' @param data an object of class data frame with the data.
#' @param col an integer that specify the column that you want for make the plot.
#' 
#' @seealso http://www.rdatamining.com/examples/outlier-detection
#' 
#' @examples
#' #Example 1
#' #install.packages("Rlof")
#' library(Rlof) #for outlier detection
#' 
#' iris.x <- iris[,1:4] # These are the independent variables
#' Species <- iris[,5] # This is the dependent variable
#' 
#' DensityPlot(iris.x,1)
#' 
#' 
#' #Example 2
#' #install.packages("Rlof")
#' library(Rlof) #Outlier detection library
#' 
#' iris.x <- iris[,1:4] # These are the independent variables
#' Species <- iris[,5] # This is the dependent variable
#' 
#' outlier.scores <- lof(iris.x, k = 5) #applying outlier detection
#' outlier.scores <- data.frame(outlier.scores)
#' DensityPlot(outlier.scores, 1) #Generating a plot of outliers scores
#' 
#' 
#' #Example 3
#' #install.packages("Rlof")
#' library(Rlof) #Outlier detection library
#' 
#' iris.x <- iris[,1:4] # These are the independent variables
#' Species <- iris[,5] # This is the dependent variable
#' 
#' outlier.scores <- lof(iris.x, k = c(5:10)) #applying outlier detection
#' mean <- rowMeans(outlier.scores) #Calculating the mean of every execution
#' outlier.scores <- data.frame(outlier.scores, mean) #adding mean to data frame
#' DensityPlot(outlier.scores, ncol(outlier.scores)) #Generating a plot of outliers scores
#' 
#' 
#' #Example 4
#' #install.packages("Rlof")
#' library(Rlof) #Outlier detection library
#' library(plyr)
#' # Getting a data set without missing values
#' cars <- read.csv("https://dl.dropboxusercontent.com/u/12599702/autosclean.csv", sep = ";", dec = ",")
#' 
#' cars.x <- cars[,1:16] # These are the independent variables
#' cars.y <- cars[,17] # This is the dependent variable
#' 
#' outlier.scores <- lof(cars.x, k = c(5:10)) #applying outlier detection
#' mean <- rowMeans(outlier.scores) #Calculating the mean of every execution
#' outlier.scores <- data.frame(outlier.scores, mean) #adding mean to data frame
#' DensityPlot(outlier.scores, ncol(outlier.scores)) #Generating a plot of outliers scores
#' 
#' aux <- outlier.scores[,7]>1.7 #1.7 is the threshold selected
#' count(aux)[2,2] #Number of outliers found
#' outliers <- order(outlier.scores[,7], decreasing=T)[1:count(aux)[2,2]] #Getting the values that are on the threshold
#' Score <- outlier.scores[outliers,7] #Getting outliers scores
#' outliers <- data.frame(outliers,Score)
#' names(outliers) <- c("Position","Score")
#' View(outliers)
#' 
#' auxOutliers <- outlier.scores[-outliers[1:3,1],] #Eliminating the 3 most remote instances!
#' DensityPlot(auxOutliers, ncol(outlier.scores)) #Generating a plot of outliers scores
DensityPlot <- function(data, col) {
  
  if (missing(data)) {
    stop("Need to specify data!")
  }
  if (class(data) != "data.frame") {
    stop("data must be a data frame class!")
  }
  if (missing(col)) {
    stop("Need to specify col!")
  }
  if (!(class(col) == "numeric" || class(col) == "integer")) {
    stop("col must be a numeric or integer class!")
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
#' Generate a 3D plot Generates a 3D graphic for a set of 3 columns of the data set.
#' 
#' @param data an object of class data frame with the data.
#' @param columns an object of class "numeric" containing the list of columns
#' that you want in your parallel plot.
#' @param dependentVariable an object of class "numeric", "factor" or "integer" is 
#' a list of values containig the dependent variable.
#' 
#' @examples
#' #Example 1
#' iris.x <- iris[,1:4] # These are the independent variables
#' Species <- iris[,5] # This is the dependent variable
#' 
#' # 3D Plot of 3 first columns of data set
#' Plot3D(iris.x, c(1,2,3), Species)
#' 
#' 
#' #Example 2
#' iris.x <- iris[,1:4] # These are the independent variables
#' Species <- iris[,5] # This is the dependent variable
#' 
#' ir.pca <- prcomp(iris.x, center = TRUE, scale. = TRUE) # performin prcomp
#' 
#' # 3D Plot of 3 first columns of data set
#' Plot3D(as.data.frame(ir.pca$x), c(1,2,3), Species)
#' 
#' 
#' #Example 3
#' # Getting a clean data set (without missing values)
#' cars <- read.csv("https://dl.dropboxusercontent.com/u/12599702/autosclean.csv", sep = ";", dec = ",")
#' cars.x <- cars[,1:16] # These are the independent variables
#' cars.y <- cars[,17] # This is the dependent variable
#' 
#' # 3D Plot of 3 first columns of data set
#' Plot3D(cars.x, c(1,2,3), cars.y)
#' 
#' 
#' #Example 4
#' # Getting a clean data set (without missing values)
#' cars <- read.csv("https://dl.dropboxusercontent.com/u/12599702/autosclean.csv", sep = ";", dec = ",")
#' cars.x <- cars[,1:16] # These are the independent variables
#' cars.y <- cars[,17] # This is the dependent variable
#' 
#' cars.pca <- prcomp(cars.x, center = TRUE, scale. = TRUE) # performin prcomp
#' 
#' # 3D Plot of 3 first columns of data set
#' Plot3D(as.data.frame(cars.pca$x), c(1,2,3), cars.y)
Plot3D<- function(data, columns, dependentVariable){
  
  if (missing(data)) {
    stop("Need to specify data!")
  }
  if (class(data) != "data.frame") {
    stop("data must be a data.frame class!")
  }
  if (missing(columns)) {
    stop("Need to specify columns!")
  }
  if (!(class(columns) == "numeric" || class(columns) == "integer")) {
    stop("columns must be a numeric or integer class!")
  }
  if (length(columns) != 3) {
    stop("The number of selected columns must be 3!")
  }
  if (missing(dependentVariable)) {
    stop("Need to specify dependentVariable!")
  }
  if (!(class(dependentVariable) == "numeric" || class(dependentVariable) == "factor" || class(dependentVariable) == "integer")) {
    stop("dependentVariable must be a numeric, factor or integer class!")
  }
  
  #All parameters are OK!
  subData<-data[,columns]
  
  col1 <- subData[,1]
  col2 <- subData[,2]
  col3 <- subData[,3]
  x_lab <- colnames(subData)[1]
  y_lab <- colnames(subData)[2]
  z_lab <- colnames(subData)[3]
  
  if (class(dependentVariable) == "numeric" || class(dependentVariable) == "integer") {
    cols <- myColorRamp(c("darkred", "yellow", "darkgreen"), dependentVariable)
    plot3d(x = col1, y = col2, z = col3, col = cols, size = "4", xlab = x_lab, ylab = y_lab, zlab = z_lab)
  }
  else {
    cols <- myColorRamp(c("darkred", "yellow", "darkgreen"), as.numeric(dependentVariable))
    plot3d(x = col1, y = col2, z = col3, col = cols, size = "4", xlab = x_lab, ylab = y_lab, zlab = z_lab)
  }
}

#' Simple Plot of 2 columns (Plot)
#'
#' Generate a plot of 2 columns of data set using ggplot. You must indicate which 
#' columns you want in the graph.
#' 
#' @param data an object of class data frame with the data.
#' @param DependentVariable an object of class "numeric", "factor" or "integer" is 
#' a list of values containig the dependent variable.
#' @param x_axis an integer that represent the number of the column that you want
#' in your x axis.
#' @param y_axis an integer that represent the number of the column that you want
#' in your y axis.
#' @
#' param dependentVariableName is an optional parameter. It's an string that
#' contains the name of your dependent variable.
#' @param pointSize is an optional parameter of class numeric with a single value 
#' that represent the point size of plot.
#' @param alphaPoint is an optional parameter of class numeric with a single value 
#' that represent the alpha of points in the plot.
#' 
#' @seealso elbowPlot
#' 
#' @examples
#' #Example 1
#' iris.x <- iris[,1:4] # These are the independent variables
#' Species <- iris[,5] # This is the dependent variable
#' 
#' # Plot of first 2 columns of data set
#' simplePlot(iris.x, Species, 1, 2, "Species", 2, 0.9)
#' 
#' 
#' #Example 2
#' iris.x <- iris[,1:4] # These are the independent variables
#' Species <- iris[,5] # This is the dependent variable
#' 
#' ir.pca <- prcomp(iris.x, center = TRUE, scale. = TRUE) #performing prcomp
#' 
#' # Plot of first 2 columns of principal components
#' simplePlot(as.data.frame(ir.pca$x), Species, 1, 2, "Species", 2, 0.9)
#' 
#' 
#' #Example 3
#' # Getting a clean data set (without missing values)
#' cars <- read.csv("https://dl.dropboxusercontent.com/u/12599702/autosclean.csv", sep = ";", dec = ",")
#' cars.x <- cars[,1:16] # These are the independent variables
#' cars.y <- cars[,17] # This is the dependent variable
#' 
#' cars.pca <- prcomp(cars.x, center = TRUE, scale. = TRUE) #performing prcomp
#' 
#' # Plot of first 2 columns of principal components
#' simplePlot(as.data.frame(cars.pca$x), cars.y, 1, 2, "Price", 2, 0.9)
simplePlot <- function(data, DependentVariable, x_axis, y_axis, dependentVariableName, pointSize, alphaPoint) {
  
  if (missing(data)) {
    stop("Need to specify data!")
  }
  if (class(data) != "data.frame") {
    stop("data must be a data.frame class!")
  }
  if (missing(DependentVariable)) {
    stop("Need to specify DependentVariable!")
  }
  if (!(class(DependentVariable) == "numeric" || class(DependentVariable) == "factor" || class(DependentVariable) == "integer")) {
    stop("DependentVariable must be a numeric, factor or integer class!")
  }
  if (missing("x_axis")) {
    stop("Need to specify x_axis!")
  }
  if (!(class(x_axis) == "numeric" || class(x_axis) == "integer")) {
    stop("x_axis must be a numeric or integer class!")
  }
  if (missing("y_axis")) {
    stop("Need to specify y_axis!")
  }
  if (!(class(y_axis) == "numeric" || class(y_axis) == "integer")) {
    stop("y_axis must be a numeric or integer class!")
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
  subData <- data.frame(data[,x_axis], data[,y_axis], DependentVariable)
  x_axis <- colnames(data)[x_axis]
  y_axis <- colnames(data)[y_axis]
  names(subData) <- c(x_axis, y_axis, "DependentVariable")
  
  if (class(DependentVariable) == "numeric" || class(DependentVariable) == "integer") {
    p <- ggplot(subData, aes_string(x = x_axis, y = y_axis)) + 
      geom_point(aes(colour = DependentVariable), na.rm = TRUE, alpha = alphaPoint, size = pointSize) + 
      scale_color_gradientn(name = dependentVariableName,
                            colours = c("darkred", "yellow", "darkgreen")) + #set the pallete
      theme(panel.grid.minor = element_blank(), #remove gridlines
            legend.position = "bottom" #legend at the bottom
      ) + #end theme
      xlab(x_axis) + ylab(y_axis)
  }
  else {
    p <- ggplot(subData, aes_string(x = x_axis, y = y_axis)) + 
      geom_point(aes(colour = DependentVariable), na.rm = TRUE, alpha = alphaPoint, size = pointSize) + 
      scale_color_discrete(name = dependentVariableName) +
      theme(panel.grid.minor = element_blank(), #remove gridlines
            legend.position = "bottom" #legend at the bottom
      ) + #end theme
      xlab(x_axis) + ylab(y_axis)
  }
  
  return (p)
}