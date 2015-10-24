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
  rowsData <- length(data.pca$sdev)
  seqRow <- seq(from = 1, to = rowsData, length.out = rowsData)
  
  dataPlot <- data.frame(seqRow, data.pca$sdev)
  names(dataPlot) <- c("PCA", "Variances")
  dataPlot <- dataPlot[1:10,]
  dataPlot <- CalculateVariance(dataPlot, 2)
  
  ggplot(data = dataPlot, aes(x = PCA, y = Variances, group = 1)) +
    geom_line(colour = "dodgerblue4", alpha = 0.5, size = 1) +
    geom_point(colour = "dodgerblue4", size = 2, alpha = 0.5) +
    expand_limits(y = 0) +
    xlab("PCs") + ylab("Variances") +
    scale_x_continuous(breaks = dataPlot$PCA) +
    theme(panel.grid.minor = element_blank(), #remove gridlines
          legend.position = "bottom" #legend at the bottom
    )#end theme
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
  
  PCs <- data.frame(data.pca$x[,x_axis], data.pca$x[,y_axis], dependentVariable)
  x_axis <- paste(c("PC", x_axis), collapse = "")
  y_axis <- paste(c("PC", y_axis), collapse = "")
  names(PCs) <- c(x_axis, y_axis, "DependentVariable")
  
  ggplot(PCs, aes_string(x = x_axis, y = y_axis)) + 
    geom_point(aes(colour = PCs$DependentVariable), na.rm = TRUE, alpha = 0.8, size = 2) + 
    scale_color_gradientn(name = dependentVariableName,
                          colours = c("darkred", "yellow", "darkgreen")) + #set the pallete
    theme(panel.grid.minor = element_blank(), #remove gridlines
          legend.position = "bottom" #legend at the bottom
    )#end theme
}