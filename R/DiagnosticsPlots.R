#' Residuals v/s Fitted Values (Plot)
#'
#' Generate a plot of fitted values v/s residuals of a regression model.
#' 
#' @param diagnostic an object of class data frame with differents error types 
#' calculated for make the graph.
#' @param dependentVariableName is an optional parameter. It's an string that
#' contains de name of your dependent variable of your regression model.
#' @param colours is an optional parameter of class character with a list of colours 
#' to use in the plot. The default value for continuos dependent variable is 
#' c("darkred", "yellow", "darkgreen") and for categorical dependent variable are 
#' the default colours defined by ggplot.
#' 
#' @seealso diagnosticData, StResidualsFitted, NormalQQ, StResidualsLeverange
#' 
#' @examples
#' #Example 1
#' iris.x <- iris[,1:3] # These are the independent variables
#' Petal.Width <- iris[,4] # This is the dependent variable
#' 
#' ir.pca <- prcomp(iris.x, center = TRUE, scale. = TRUE) # Performing prcomp
#' 
#' PCA <- as.data.frame(ir.pca$x)
#' PC1 <- PCA[,1]
#' PC2 <- PCA[,2]
#' PC3 <- PCA[,3]
#' 
#' fit <- lm(Petal.Width ~ PC1 + PC2 + PC3, data = PCA) # Perfoming linear regression
#' 
#' diagnostic <- diagnosticData(fit) # Generating data for differents plots
#' ResidualsFitted(diagnostic, "Petal Width") # Generating Residuals v/s Fitted Values plot
#' StResidualsFitted(diagnostic, "Petal Width") #Generating Standarized Residuals v/s Fitted Values plot
#' NormalQQ(diagnostic, "Petal Width") # Generating Normal-QQ plot
#' StResidualsLeverange(diagnostic, "Petal Width") # Generating Leverange v/s Standarized Residuals plot
#' 
#' # Plots with a different colours palette
#' myPalette <- c("darkolivegreen4", "goldenrod1", "dodgerblue4")
#' ResidualsFitted(diagnostic, "Petal Width", colours = myPalette) # Generating Residuals v/s Fitted Values plot
#' StResidualsFitted(diagnostic, "Petal Width", colours = myPalette) #Generating Standarized Residuals v/s Fitted Values plot
#' StResidualsLeverange(diagnostic, "Petal Width",colours = myPalette) # Generating Leverange v/s Standarized Residuals plot
#' 
#' 
#' #Example 2
#' # Getting a clean data set (without missing values)
#' cars <- read.csv("https://dl.dropboxusercontent.com/u/12599702/autosclean.csv", sep = ";", dec = ",")
#' cars.x <- cars[,1:16] # These are the independent variables
#' cars.y <- cars[,17] # This is the dependent variable
#' 
#' cars.pca <- prcomp(cars.x, center = TRUE, scale. = TRUE)
#' 
#' PCA <- as.data.frame(cars.pca$x)
#' PC1 <- PCA[,1]
#' PC2 <- PCA[,2]
#' PC3 <- PCA[,3]
#' 
#' fit <- lm(cars.y ~ PC1 + PC2 + PC3, data = PCA) # Perfoming linear regression
#' 
#' diagnostic <- diagnosticData(fit) # Generating data for differents plots
#' ResidualsFitted(diagnostic, "Price") # Generating Residuals v/s Fitted Values plot
#' StResidualsFitted(diagnostic, "Price") #Generating Standarized Residuals v/s Fitted Values plot
#' NormalQQ(diagnostic, "Price") # Generating Normal-QQ plot
#' StResidualsLeverange(diagnostic, "Price") # Generating Leverange v/s Standarized Residuals plot
#' 
#' # Plots with a different colours palette
#' myPalette <- c("darkolivegreen4", "goldenrod1", "dodgerblue4")
#' ResidualsFitted(diagnostic, "Price", colours = myPalette) # Generating Residuals v/s Fitted Values plot
#' StResidualsFitted(diagnostic, "Price", colours = myPalette) #Generating Standarized Residuals v/s Fitted Values plot
#' StResidualsLeverange(diagnostic, "Price",colours = myPalette) # Generating Leverange v/s Standarized Residuals plot
ResidualsFitted <- function(diagnostic, dependentVariableName, colours) {
  
  if (missing(diagnostic)) {
    stop("Need to specify diagnostic!")
  }
  if (class(diagnostic) != "data.frame") {
    stop("diagnostic must be a data frame class!")
  }
  if (missing("dependentVariableName")) {
    dependentVariableName <- "Dependent Variable"
  }
  if (class(dependentVariableName) != "character") {
    stop("dependentVariableName must be a character class!")
  }
  if (missing(colours)) {
    colours <- c("darkred", "yellow", "darkgreen")
  }
  
  #All parameters are OK!
  min <- min(diagnostic$Dependent.Variable)
  max <- max(diagnostic$Dependent.Variable)
  p <- ggplot(diagnostic, aes(fitted, resid)) +
    geom_hline(yintercept = 0, colour = "grey50", size = 0.5, linetype="dashed") +
    geom_point(aes(colour = diagnostic$Dependent.Variable), na.rm = TRUE) + 
    scale_color_gradientn(name = dependentVariableName,
                          colours = colours, breaks = c(min, max),
                          labels = c(min, max)) + #set the pallete
    geom_smooth(method = "auto", size = 0.7, se = F, colour = "#299E98") +
    xlab("Fitted Values") +
    ylab("Residuals") +
    theme(legend.position = "bottom" #legend at the bottom
    )#end theme
  
  return (p)
}

#' Standarized Residuals v/s Fitted Values (Plot)
#'
#' Generate a plot of fitted values v/s Standarized Residuals of a regression model.
#' 
#' @param diagnostic an object of class data frame with differents error types 
#' calculated for make the graph.
#' @param dependentVariableName is an optional parameter. It's an string that
#' contains de name of your dependent variable of your regression model.
#' @param colours is an optional parameter of class character with a list of colours 
#' to use in the plot. The default value for continuos dependent variable is 
#' c("darkred", "yellow", "darkgreen") and for categorical dependent variable are 
#' the default colours defined by ggplot.
#' 
#' @seealso diagnosticData, ResidualsFitted, NormalQQ, StResidualsLeverange
#' 
#' @examples
#' #Example 1
#' iris.x <- iris[,1:3] # These are the independent variables
#' Petal.Width <- iris[,4] # This is the dependent variable
#' 
#' ir.pca <- prcomp(iris.x, center = TRUE, scale. = TRUE) # Performing prcomp
#' 
#' PCA <- as.data.frame(ir.pca$x)
#' PC1 <- PCA[,1]
#' PC2 <- PCA[,2]
#' PC3 <- PCA[,3]
#' 
#' fit <- lm(Petal.Width ~ PC1 + PC2 + PC3, data = PCA) # Perfoming linear regression
#' 
#' diagnostic <- diagnosticData(fit) # Generating data for differents plots
#' ResidualsFitted(diagnostic, "Petal Width") # Generating Residuals v/s Fitted Values plot
#' StResidualsFitted(diagnostic, "Petal Width") #Generating Standarized Residuals v/s Fitted Values plot
#' NormalQQ(diagnostic, "Petal Width") # Generating Normal-QQ plot
#' StResidualsLeverange(diagnostic, "Petal Width") # Generating Leverange v/s Standarized Residuals plot
#' 
#' # Plots with a different colours palette
#' myPalette <- c("darkolivegreen4", "goldenrod1", "dodgerblue4")
#' ResidualsFitted(diagnostic, "Petal Width", colours = myPalette) # Generating Residuals v/s Fitted Values plot
#' StResidualsFitted(diagnostic, "Petal Width", colours = myPalette) #Generating Standarized Residuals v/s Fitted Values plot
#' StResidualsLeverange(diagnostic, "Petal Width",colours = myPalette) # Generating Leverange v/s Standarized Residuals plot
#' 
#' 
#' #Example 2
#' # Getting a clean data set (without missing values)
#' cars <- read.csv("https://dl.dropboxusercontent.com/u/12599702/autosclean.csv", sep = ";", dec = ",")
#' cars.x <- cars[,1:16] # These are the independent variables
#' cars.y <- cars[,17] # This is the dependent variable
#' 
#' cars.pca <- prcomp(cars.x, center = TRUE, scale. = TRUE)
#' 
#' PCA <- as.data.frame(cars.pca$x)
#' PC1 <- PCA[,1]
#' PC2 <- PCA[,2]
#' PC3 <- PCA[,3]
#' 
#' fit <- lm(cars.y ~ PC1 + PC2 + PC3, data = PCA) # Perfoming linear regression
#' 
#' diagnostic <- diagnosticData(fit) # Generating data for differents plots
#' ResidualsFitted(diagnostic, "Price") # Generating Residuals v/s Fitted Values plot
#' StResidualsFitted(diagnostic, "Price") #Generating Standarized Residuals v/s Fitted Values plot
#' NormalQQ(diagnostic, "Price") # Generating Normal-QQ plot
#' StResidualsLeverange(diagnostic, "Price") # Generating Leverange v/s Standarized Residuals plot
#' 
#' # Plots with a different colours palette
#' myPalette <- c("darkolivegreen4", "goldenrod1", "dodgerblue4")
#' ResidualsFitted(diagnostic, "Price", colours = myPalette) # Generating Residuals v/s Fitted Values plot
#' StResidualsFitted(diagnostic, "Price", colours = myPalette) #Generating Standarized Residuals v/s Fitted Values plot
#' StResidualsLeverange(diagnostic, "Price",colours = myPalette) # Generating Leverange v/s Standarized Residuals plot
StResidualsFitted <- function(diagnostic, dependentVariableName, colours) {
  
  if (missing(diagnostic)) {
    stop("Need to specify diagnostic!")
  }
  if (class(diagnostic) != "data.frame") {
    stop("diagnostic must be a data frame class!")
  }
  if (missing("dependentVariableName")) {
    dependentVariableName <- "Dependent Variable"
  }
  if (class(dependentVariableName) != "character") {
    stop("dependentVariableName must be a character class!")
  }
  if (missing(colours)) {
    colours <- c("darkred", "yellow", "darkgreen")
  }
  
  #All parameters are OK!
  min <- min(diagnostic$Dependent.Variable)
  max <- max(diagnostic$Dependent.Variable)
  p <- ggplot(diagnostic, aes(fitted, sqrt.abs.stz.r)) +
    geom_point(aes(colour = diagnostic$Dependent.Variable), na.rm = TRUE) + 
    scale_color_gradientn(name = dependentVariableName,
                          colours = colours, breaks = c(min, max),
                          labels = c(min, max)) + #set the pallete
    geom_smooth(method = "auto", size = 0.7, se = F, colour = "#299E98") +
    xlab("Fitted Values") +
    ylab(expression(sqrt("|Standarized Residuals|"))) +
    theme(legend.position = "bottom" #legend at the bottom
    )#end theme
  
  return (p)
}

#' Normal-QQ (Plot)
#'
#' Generate a plot of theoretical quantiles v/s Standarized Residuals of a 
#' regression model.
#' 
#' @param diagnostic an object of class data frame with differents error types 
#' calculated for make the graph.
#' @param dependentVariableName is an optional parameter. It's an string that
#' contains de name of your dependent variable of your regression model.
#' 
#' @note Part of this code, it's from 
#' http://librestats.com/2012/06/11/autoplot-graphical-methods-with-ggplot2/
#' 
#' @seealso diagnosticData, ResidualsFitted, StResidualsFitted, StResidualsLeverange
#' 
#' @examples
#' #Example 1
#' iris.x <- iris[,1:3] # These are the independent variables
#' Petal.Width <- iris[,4] # This is the dependent variable
#' 
#' ir.pca <- prcomp(iris.x, center = TRUE, scale. = TRUE) # Performing prcomp
#' 
#' PCA <- as.data.frame(ir.pca$x)
#' PC1 <- PCA[,1]
#' PC2 <- PCA[,2]
#' PC3 <- PCA[,3]
#' 
#' fit <- lm(Petal.Width ~ PC1 + PC2 + PC3, data = PCA) # Perfoming linear regression
#' 
#' diagnostic <- diagnosticData(fit) # Generating data for differents plots
#' ResidualsFitted(diagnostic, "Petal Width") # Generating Residuals v/s Fitted Values plot
#' StResidualsFitted(diagnostic, "Petal Width") #Generating Standarized Residuals v/s Fitted Values plot
#' NormalQQ(diagnostic, "Petal Width") # Generating Normal-QQ plot
#' StResidualsLeverange(diagnostic, "Petal Width") # Generating Leverange v/s Standarized Residuals plot
#' 
#' # Plots with a different colours palette
#' myPalette <- c("darkolivegreen4", "goldenrod1", "dodgerblue4")
#' ResidualsFitted(diagnostic, "Petal Width", colours = myPalette) # Generating Residuals v/s Fitted Values plot
#' StResidualsFitted(diagnostic, "Petal Width", colours = myPalette) #Generating Standarized Residuals v/s Fitted Values plot
#' StResidualsLeverange(diagnostic, "Petal Width",colours = myPalette) # Generating Leverange v/s Standarized Residuals plot
#' 
#' 
#' #Example 2
#' # Getting a clean data set (without missing values)
#' cars <- read.csv("https://dl.dropboxusercontent.com/u/12599702/autosclean.csv", sep = ";", dec = ",")
#' cars.x <- cars[,1:16] # These are the independent variables
#' cars.y <- cars[,17] # This is the dependent variable
#' 
#' cars.pca <- prcomp(cars.x, center = TRUE, scale. = TRUE)
#' 
#' PCA <- as.data.frame(cars.pca$x)
#' PC1 <- PCA[,1]
#' PC2 <- PCA[,2]
#' PC3 <- PCA[,3]
#' 
#' fit <- lm(cars.y ~ PC1 + PC2 + PC3, data = PCA) # Perfoming linear regression
#' 
#' diagnostic <- diagnosticData(fit) # Generating data for differents plots
#' ResidualsFitted(diagnostic, "Price") # Generating Residuals v/s Fitted Values plot
#' StResidualsFitted(diagnostic, "Price") #Generating Standarized Residuals v/s Fitted Values plot
#' NormalQQ(diagnostic, "Price") # Generating Normal-QQ plot
#' StResidualsLeverange(diagnostic, "Price") # Generating Leverange v/s Standarized Residuals plot
#' 
#' # Plots with a different colours palette
#' myPalette <- c("darkolivegreen4", "goldenrod1", "dodgerblue4")
#' ResidualsFitted(diagnostic, "Price", colours = myPalette) # Generating Residuals v/s Fitted Values plot
#' StResidualsFitted(diagnostic, "Price", colours = myPalette) #Generating Standarized Residuals v/s Fitted Values plot
#' StResidualsLeverange(diagnostic, "Price",colours = myPalette) # Generating Leverange v/s Standarized Residuals plot
NormalQQ <- function(diagnostic, dependentVariableName) {
  
  if (missing(diagnostic)) {
    stop("Need to specify diagnostic!")
  }
  if (class(diagnostic) != "data.frame") {
    stop("diagnostic must be a data frame class!")
  }
  if (missing("dependentVariableName")) {
    dependentVariableName <- "Dependent Variable"
  }
  if (class(dependentVariableName) != "character") {
    stop("dependentVariableName must be a character class!")
  }
  
  #All parameters are OK!
  a <- quantile(diagnostic$stz.r, c(0.25, 0.75))
  b <- qnorm(c(0.25, 0.75))
  slope <- diff(a)/diff(b)
  int <- a[1] - slope * b[1]
  
  p <- ggplot(diagnostic, aes(sample = diagnostic$stz.r)) +
    stat_qq() +
    geom_abline(slope = slope, intercept = int, colour = "#299E98", linetype="dashed") +
    geom_abline(slope = 0, intercept = -2, colour = "#299E98", linetype="dashed") +
    geom_abline(slope = 0, intercept = 2, colour = "#299E98", linetype="dashed") +
    scale_x_continuous("Theoretical Quantiles") +
    scale_y_continuous("Standardized Residuals")
  
  return (p)
}

#' Leverange v/s Standarized Residuals (Plot)
#'
#' Generate a plot of Standarized Residuals v/s Leverange of a regression model.
#' 
#' @param diagnostic an object of class data frame with differents error types 
#' calculated for make the graph.
#' @param dependentVariableName is an optional parameter. It's an string that
#' contains de name of your dependent variable of your regression model.
#' @param colours is an optional parameter of class character with a list of colours 
#' to use in the plot. The default value for continuos dependent variable is 
#' c("darkred", "yellow", "darkgreen") and for categorical dependent variable are 
#' the default colours defined by ggplot.
#' 
#' @seealso diagnosticData, ResidualsFitted, StResidualsFitted, NormalQQ
#' 
#' @examples
#' #Example 1
#' iris.x <- iris[,1:3] # These are the independent variables
#' Petal.Width <- iris[,4] # This is the dependent variable
#' 
#' ir.pca <- prcomp(iris.x, center = TRUE, scale. = TRUE) # Performing prcomp
#' 
#' PCA <- as.data.frame(ir.pca$x)
#' PC1 <- PCA[,1]
#' PC2 <- PCA[,2]
#' PC3 <- PCA[,3]
#' 
#' fit <- lm(Petal.Width ~ PC1 + PC2 + PC3, data = PCA) # Perfoming linear regression
#' 
#' diagnostic <- diagnosticData(fit) # Generating data for differents plots
#' ResidualsFitted(diagnostic, "Petal Width") # Generating Residuals v/s Fitted Values plot
#' StResidualsFitted(diagnostic, "Petal Width") #Generating Standarized Residuals v/s Fitted Values plot
#' NormalQQ(diagnostic, "Petal Width") # Generating Normal-QQ plot
#' StResidualsLeverange(diagnostic, "Petal Width") # Generating Leverange v/s Standarized Residuals plot
#' 
#' # Plots with a different colours palette
#' myPalette <- c("darkolivegreen4", "goldenrod1", "dodgerblue4")
#' ResidualsFitted(diagnostic, "Petal Width", colours = myPalette) # Generating Residuals v/s Fitted Values plot
#' StResidualsFitted(diagnostic, "Petal Width", colours = myPalette) #Generating Standarized Residuals v/s Fitted Values plot
#' StResidualsLeverange(diagnostic, "Petal Width",colours = myPalette) # Generating Leverange v/s Standarized Residuals plot
#' 
#' 
#' #Example 2
#' # Getting a clean data set (without missing values)
#' cars <- read.csv("https://dl.dropboxusercontent.com/u/12599702/autosclean.csv", sep = ";", dec = ",")
#' cars.x <- cars[,1:16] # These are the independent variables
#' cars.y <- cars[,17] # This is the dependent variable
#' 
#' cars.pca <- prcomp(cars.x, center = TRUE, scale. = TRUE)
#' 
#' PCA <- as.data.frame(cars.pca$x)
#' PC1 <- PCA[,1]
#' PC2 <- PCA[,2]
#' PC3 <- PCA[,3]
#' 
#' fit <- lm(cars.y ~ PC1 + PC2 + PC3, data = PCA) # Perfoming linear regression
#' 
#' diagnostic <- diagnosticData(fit) # Generating data for differents plots
#' ResidualsFitted(diagnostic, "Price") # Generating Residuals v/s Fitted Values plot
#' StResidualsFitted(diagnostic, "Price") #Generating Standarized Residuals v/s Fitted Values plot
#' NormalQQ(diagnostic, "Price") # Generating Normal-QQ plot
#' StResidualsLeverange(diagnostic, "Price") # Generating Leverange v/s Standarized Residuals plot
#' 
#' # Plots with a different colours palette
#' myPalette <- c("darkolivegreen4", "goldenrod1", "dodgerblue4")
#' ResidualsFitted(diagnostic, "Price", colours = myPalette) # Generating Residuals v/s Fitted Values plot
#' StResidualsFitted(diagnostic, "Price", colours = myPalette) #Generating Standarized Residuals v/s Fitted Values plot
#' StResidualsLeverange(diagnostic, "Price",colours = myPalette) # Generating Leverange v/s Standarized Residuals plot
StResidualsLeverange <- function(diagnostic, dependentVariableName, colours) {
  
  if (missing(diagnostic)) {
    stop("Need to specify diagnostic!")
  }
  if (class(diagnostic) != "data.frame") {
    stop("diagnostic must be a data frame class!")
  }
  if (missing("dependentVariableName")) {
    dependentVariableName <- "Dependent Variable"
  }
  if (class(dependentVariableName) != "character") {
    stop("dependentVariableName must be a character class!")
  }
  if (missing(colours)) {
    colours <- c("darkred", "yellow", "darkgreen")
  }
  
  #All parameters are OK!
  min <- min(diagnostic$Dependent.Variable)
  max <- max(diagnostic$Dependent.Variable)
  p <- ggplot(diagnostic, aes(leverage, stz.r)) +
    geom_hline(yintercept = 0, colour = "grey50", size = 0.5, linetype="dashed") +
    geom_point(aes(colour = diagnostic$Dependent.Variable), na.rm = TRUE) + 
    scale_color_gradientn(name = dependentVariableName,
                          colours = colours, breaks = c(min, max),
                          labels = c(min, max)) + #set the pallete
    geom_smooth(method = "auto", size = 0.7, se = F, colour = "#299E98") +
    xlab("Leverange") +
    ylab("Standarized Residuals") +
    theme(legend.position = "bottom" #legend at the bottom
    )#end theme
  
  return (p)
}