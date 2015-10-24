#' Residuals v/s Fitted Values (Plot)
#'
#' Generate a plot of fitted values v/s residuals of a regression model.
#' 
#' @param diagnostic an object of class data frame with differents error types 
#' calculated for make the graph.
#' @param dependentVariableName is an optional parameter. It's an string that
#' contains de name of your dependent variable of your regression model.
#' @seealso diagnosticData, StResidualsFitted, NormalQQ, StResidualsLeverange
#' @examples
#' iris.x <- iris[,1:3]
#' Petal.Width <- iris[,4]
#' ir.pca <- prcomp(iris.x, center = TRUE, scale. = TRUE)
#' PCA <- as.data.frame(ir.pca$x)
#' 
#' PC1 <- PCA[,1]
#' PC2 <- PCA[,2]
#' PC3 <- PCA[,3]
#' 
#' fit <- lm(Petal.Width ~ PC1 + PC2 + PC3, data = PCA)
#' diagnostic <- diagnosticData(fit)
#' ResidualsFitted(diagnostic, "Petal Width")
ResidualsFitted <- function(diagnostic, dependentVariableName) {
  if (missing("dependentVariableName")) {
    dependentVariableName <- "Dependent Variable"
  }
  
  ggplot(diagnostic, aes(fitted, resid)) +
    geom_hline(yintercept = 0, colour = "grey50", size = 0.5, linetype="dashed") +
    geom_point(aes(colour = diagnostic$Dependent.Variable), na.rm = TRUE) + 
    scale_color_gradientn(name = dependentVariableName,
                          colours = c("darkred", "yellow", "darkgreen")) + #set the pallete
    geom_smooth(method = "auto", size = 0.7, se = F, colour = "#299E98") +
    xlab("Fitted Values") +
    ylab("Residuals") +
    theme(legend.position = "bottom" #legend at the bottom
    )#end theme
}

#' Standarized Residuals v/s Fitted Values (Plot)
#'
#' Generate a plot of fitted values v/s Standarized Residuals of a regression model.
#' 
#' @param diagnostic an object of class data frame with differents error types 
#' calculated for make the graph.
#' @param dependentVariableName is an optional parameter. It's an string that
#' contains de name of your dependent variable of your regression model.
#' @seealso diagnosticData, ResidualsFitted, NormalQQ, StResidualsLeverange
#' @examples
#' iris.x <- iris[,1:3]
#' Petal.Width <- iris[,4]
#' ir.pca <- prcomp(iris.x, center = TRUE, scale. = TRUE)
#' PCA <- as.data.frame(ir.pca$x)
#' 
#' PC1 <- PCA[,1]
#' PC2 <- PCA[,2]
#' PC3 <- PCA[,3]
#' 
#' fit <- lm(Petal.Width ~ PC1 + PC2 + PC3, data = PCA)
#' diagnostic <- diagnosticData(fit)
#' StResidualsFitted(diagnostic, "Petal Width")
StResidualsFitted <- function(diagnostic, dependentVariableName) {
  if (missing("dependentVariableName")) {
    dependentVariableName <- "Dependent Variable"
  }
  
  ggplot(diagnostic, aes(fitted, sqrt.abs.stz.r)) +
    geom_point(aes(colour = diagnostic$Dependent.Variable), na.rm = TRUE) + 
    scale_color_gradientn(name = dependentVariableName,
                          colours = c("darkred", "yellow", "darkgreen")) + #set the pallete
    geom_smooth(method = "auto", size = 0.7, se = F, colour = "#299E98") +
    xlab("Fitted Values") +
    ylab(expression(sqrt("|Standarized Residuals|"))) +
    theme(legend.position = "bottom" #legend at the bottom
    )#end theme
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
#' @note Part of this code, it's from 
#' http://librestats.com/2012/06/11/autoplot-graphical-methods-with-ggplot2/
#' @seealso diagnosticData, ResidualsFitted, StResidualsFitted, StResidualsLeverange
#' @examples
#' iris.x <- iris[,1:3]
#' Petal.Width <- iris[,4]
#' ir.pca <- prcomp(iris.x, center = TRUE, scale. = TRUE)
#' PCA <- as.data.frame(ir.pca$x)
#' 
#' PC1 <- PCA[,1]
#' PC2 <- PCA[,2]
#' PC3 <- PCA[,3]
#' 
#' fit <- lm(Petal.Width ~ PC1 + PC2 + PC3, data = PCA)
#' diagnostic <- diagnosticData(fit)
#' NormalQQ(diagnostic, "Petal Width")
NormalQQ <- function(diagnostic, dependentVariableName) {
  
  if (missing("dependentVariableName")) {
    dependentVariableName <- "Dependent Variable"
  }
  
  a <- quantile(diagnostic$stz.r, c(0.25, 0.75))
  b <- qnorm(c(0.25, 0.75))
  slope <- diff(a)/diff(b)
  int <- a[1] - slope * b[1]
  
  ggplot(diagnostic, aes(sample = diagnostic$stz.r)) +
    stat_qq() +
    geom_abline(slope = slope, intercept = int, colour = "#299E98", linetype="dashed") +
    geom_abline(slope = 0, intercept = -2, colour = "#299E98", linetype="dashed") +
    geom_abline(slope = 0, intercept = 2, colour = "#299E98", linetype="dashed") +
    scale_x_continuous("Theoretical Quantiles") +
    scale_y_continuous("Standardized Residuals")
}

#' Leverange v/s Standarized Residuals (Plot)
#'
#' Generate a plot of Standarized Residuals v/s Leverange of a regression model.
#' 
#' @param diagnostic an object of class data frame with differents error types 
#' calculated for make the graph.
#' @param dependentVariableName is an optional parameter. It's an string that
#' contains de name of your dependent variable of your regression model.
#' @seealso diagnosticData, ResidualsFitted, StResidualsFitted, NormalQQ
#' @examples
#' iris.x <- iris[,1:3]
#' Petal.Width <- iris[,4]
#' ir.pca <- prcomp(iris.x, center = TRUE, scale. = TRUE)
#' PCA <- as.data.frame(ir.pca$x)
#' 
#' PC1 <- PCA[,1]
#' PC2 <- PCA[,2]
#' PC3 <- PCA[,3]
#' 
#' fit <- lm(Petal.Width ~ PC1 + PC2 + PC3, data = PCA)
#' diagnostic <- diagnosticData(fit)
#' StResidualsLeverange(diagnostic, "Petal Width")
StResidualsLeverange <- function(diagnostic, dependentVariableName) {
  if (missing("dependentVariableName")) {
    dependentVariableName <- "Dependent Variable"
  }
  
  ggplot(diagnostic, aes(leverage, stz.r)) +
    geom_hline(yintercept = 0, colour = "grey50", size = 0.5, linetype="dashed") +
    geom_point(aes(colour = diagnostic$Dependent.Variable), na.rm = TRUE) + 
    scale_color_gradientn(name = dependentVariableName, colours = c("darkred", "yellow", "darkgreen")) + #set the pallete
    geom_smooth(method = "auto", size = 0.7, se = F, colour = "#299E98") +
    xlab("Leverange") +
    ylab("Standarized Residuals") +
    theme(legend.position = "bottom" #legend at the bottom
    )#end theme
}