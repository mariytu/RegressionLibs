#' LinePlot for PCA (Plot)
#'
#' Generate a plot of 10 first variances of Principal Components. This is useful to 
#' determinate which are the most important components.
#' 
#' @param data.pca a list with class "prcomp" containing all principal components 
#' calculated.
#' @seealso CalculateVariance, plotPC
#' @examples
#' iris.x <- iris[,1:4]
#' ir.pca <- prcomp(iris.x, center = TRUE, scale. = TRUE)
#' 
#' linePlot(ir.pca)
linePlot <- function(data.pca) {
  
  if (missing(data.pca)) {
    stop("Need to specify data.pca!")
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
    scale_color_gradientn(name = dependentVariableName,
                          colours = c("darkred", "yellow", "darkgreen")) + #set the pallete
    theme(panel.grid.minor = element_blank(), #remove gridlines
          legend.position = "bottom" #legend at the bottom
    )#end theme
  
  return (p)
}

#' Scatterplot Matrix (Plot)
#'
#' Generate a Scatterplot Matrix between a range using ggplot.
#' 
#' @param data.pca a list with class "prcomp" containing all principal components 
#' calculated.
#' @param from an integer that represent the first principal component that you 
#' want in the scatterplot matrix.
#' @param to an integer that represent the last principal component that you 
#' want in the scatterplot matrix.
#' @param dependentVariable is a list of values containig the dependent variable 
#' of your regression model.
#' @param dependentVariableName is an optional parameter. It's an string that
#' contains de name of your dependent variable of your regression model.
#' @seealso makePairs
#' @source https://gastonsanchez.wordpress.com/2012/08/27/scatterplot-matrices-with-ggplot/
#' @examples
#' iris.x <- iris[,1:3]
#' Petal.Width <- iris[,4]
#' ir.pca <- prcomp(iris.x, center = TRUE, scale. = TRUE)
#' 
#' ScatterplotMatrix(ir.pca, 1, 3, Petal.Width, "Petal Width")
ScatterplotMatrix<- function(data.pca, from, to, dependentVariable, dependentVariableName){
  
  if (missing(data.pca)) {
    stop("Need to specify data.pca!")
  }
  if (missing(dependentVariable)) {
    stop("Need to specify dependentVariable!")
  }
  if (missing(from) || missing(to)) {
    from <- 1
    to <- 3
    if (ncol(data.pca$x) > to) {
      to <- ncol(data.pca$x)
    }
  }
  if (missing(dependentVariableName)) {
    dependentVariableName <- "Dependent Variable"
  }
  
  #All parameters are OK!
  # expand data frame for pairs plot
  PCAfromTo <- as.data.frame(data.pca$x[,from:to])
  gg1 <- makePairs(PCAfromTo)
  
  #New data frame mega PCA from..to
  mega_PCA <- data.frame(gg1$all, DependentVariable = rep(dependentVariable, length = nrow(gg1$all)))
  DependentVariable <- rep(dependentVariable, length = nrow(gg1$all))
  
  # pairs plot
  p <- ggplot(mega_PCA, aes_string(x = "x", y = "y")) + 
    facet_grid(xvar ~ yvar, scales = "free") + 
    geom_point(aes(colour = DependentVariable), na.rm = TRUE, alpha = 0.5, size = 1) + 
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
  
  return (p)
}

#' Plot Columns of Matrices (Plot)
#'
#' Generate a plot of the columns of a data set for all or a range of instances. In 
#' some cases this is useful to identify some patron.
#' 
#' @param dataSet an object of class data frame with the a data set (Independent 
#' variables).
#' @param dependentVariable is a list of values containig the dependent variable 
#' of your regression model.
#' @param dependentVariableName is an optional parameter. It's an string that
#' contains de name of your dependent variable of your regression model.
#' @param from an integer that represent the first instance that you want in the plot.
#' @param to an integer that represent the last instance that you want in the plot.
#' @param x_lab a boolean that represent if you want or not the x axis scale. In 
#' some cases, when you have many columns the plot could be ugly! The default value 
#' is False.
#' @examples
#' iris.x <- iris[,1:4]
#' dependetVariable <- iris[,5]
#' MatPlot(iris.x, dependetVariable, "Species")
#' MatPlot(iris.x, dependetVariable, "Species", x_lab = TRUE)
MatPlot <- function(dataSet, dependentVariable, dependentVariableName, from, to, x_lab) {
  if (missing(dataSet)) {
    stop("Need to specify dataSet!")
  }
  if (missing(dependentVariable)) {
    stop("Need to specify dependentVariable!")
  }
  if (missing(dependentVariableName)) {
    dependentVariableName <- "Dependent Variable"
  }
  if (missing(from) || missing(to)) {
    from <- 1
    to <- nrow(dataSet)
  }
  if (from > to) {
    stop("from must be less than to!")
  }
  if (missing(x_lab)) {
    x_lab = FALSE
  }
  
  #All parameters are OK!
  x_name = "Columns"
  dataSet <- dataSet[from:to,]
  dependentVariable <- dependentVariable[from:to]
  rows <- nrow(dataSet)
  x <- seq(from = 1, to = rows, length.out = rows)
  data <- data.frame(x, dataSet)
  dataPlot <- melt(data, id = "x")
  dataPlot <- data.frame(dataPlot, dependentVariable)
  
  if (class(dependentVariable)=="numeric") {
    if (x_lab) {
      p <- ggplot(dataPlot, aes(variable, value, group = x, colour = dependentVariable)) +
        geom_line(size = 1) +
        scale_color_gradientn(name = dependentVariableName,
                              colours = c("darkred", "yellow", "darkgreen")) +
        xlab(x_name) + ylab("Values")
    }
    else {
      p <- ggplot(dataPlot, aes(variable, value, group = x, colour = dependentVariable)) +
        geom_line(size = 1) +
        scale_color_gradientn(name = dependentVariableName,
                              colours = c("darkred", "yellow", "darkgreen")) +
        scale_x_discrete(breaks = c()) +
        xlab(x_name) + ylab("Values")
    }
  }
  else {
    if (x_lab) {
      p <- ggplot(dataPlot, aes(variable, value, group = x, colour = dependentVariable)) +
        geom_line(size = 1) +
        scale_color_discrete(name = dependentVariableName) +
        xlab(x_name) + ylab("Values")
    }
    else {
      p <- ggplot(dataPlot, aes(variable, value, group = x, colour = dependentVariable)) +
        geom_line(size = 1) +
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
#' @examples
#' iris.x <- iris[,1:4]
#' outlier.scores <- lof(iris.x, k = c(5:10))
#' DensityPlot(outlier.scores, 1)
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