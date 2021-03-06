% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/Plots.R
\name{elbowPlot}
\alias{elbowPlot}
\title{Elbow Plot for PCA (Plot)}
\usage{
elbowPlot(data.pca)
}
\arguments{
\item{data.pca}{a list with class "prcomp" containing all principal components
calculated.}
}
\description{
Generate a plot of 10 first variances of Principal Components. This is useful to
determinate which are the most important components.
}
\examples{
#Example 1
iris.x <- iris[,1:4] # These are the independent variables
# We know that there are no missing values in the data set

# performing prcomp
ir.pca <- prcomp(iris.x, center = TRUE, scale. = TRUE)

# Generating elbow plot to detect the most important principal components
elbowPlot(ir.pca)


#Example 2
# Getting a clean data set (without missing values)
cars <- read.csv("https://dl.dropboxusercontent.com/u/12599702/autosclean.csv", sep = ";", dec = ",")
cars.x <- cars[,1:16] # These are the independent variables

# Performing prcomp
cars.pca <- prcomp(cars.x, center = TRUE, scale. = TRUE)

# Generating elbow plot to detect the most important principal components
elbowPlot(cars.pca)
}
\seealso{
CalculateVariance, plotPC
}

