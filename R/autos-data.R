#' Automobile data set
#'
#' Data from Machine Learning Repository, UCI. This data set just containg a 
#' numerical columns of original data set obtained from the repository. The last 
#' column (price) is the dependent variable, and the first sixteen columns are the
#' independents variables. This data set, is useful for regression problems.
#' 
#' @details
#' Attribute Information:     
#'     Attribute:                Attribute Range:
#' ------------------    --------------------------------------
#' 1. wheel-base:          continuous from 86,6 120,9
#' 2. length:              continuous from 141,1 to 208,1
#' 3. width:               continuous from 60,3 to 72,3
#' 4. height:              continuous from 47,8 to 59,8
#' 5. curb-weight:         continuous from 1488 to 4066
#' 6. engine-type:         1 (dohc), 2 (dohcv), 3 (l), 4 (ohc),
#'                         5 (ohcf), 6 (ohcv), 7 (rotor)
#' 7. num-of-cylinders:    8, 5, 4, 6, 3, 12, 2
#' 8. engine-size:         continuous from 61 to 326
#' 9. fuel-system:         1 (1bbl), 2 (2bbl), 3 (4bbl), 4 (idi),
#'                         5 (mfi), 6 (mpfi), 7 (spdi), 8 (spfi)
#' 10. bore:               continuous from 2,54 to 3,94
#' 11. stroke:             continuous from 2,07 to 4,17
#' 12. compression-ratio:  continuous from 7 to 23
#' 13. horsepower:         continuous from 48 to 288
#' 14. peak-rpm:           continuous from 4150 to 6600
#' 15. city-mpg:           continuous from 13 to 49
#' 16. highway-mpg:        continuous from 16 to 54
#' 17. price:              continuous from 5118 to 45400
#'
#' @docType data
#'
#' @usage data(autos)
#'
#' @keywords datasets
#'
#' @source \href{https://archive.ics.uci.edu/ml/datasets/Automobile}
#'
#' @examples
#' data(autos)
#' 
"autos"