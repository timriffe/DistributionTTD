
#' @title calculate the Nth moment of remaining years of life as a function of age
#' 
#' @description The Nth moment of the conditional distribution of remaining years of life, \eqn{\eta _n(y|a)} is defined as:
#' \deqn{\eta _n(y|a) =  \int_{y=0}^\infty (y-e(a))^n f(y|a) \text{d}y}
#' ax is used as an argument to account for infants. If missing, assumed as .5. Ages assumed single.
#' 
#' @param dx a vector of single age lifetable deaths (dx). Any radix.
#' @param n which moment?
#' @param ax Chiang's lifetable ax. 
#' 
#' @return vector of age specific nth moments.
#' 
#' @importFrom compiler cmpfun
#' 
#' @export 
#' 

momentN <- compiler::cmpfun(function(dx, n, ax = rep(.5, length(dx))){
            fya <- da2fya(dx)
            ex  <- rowSums((col(fya) - (1 - ax)) * fya)
            rowSums(((col(fya) - .5) - ex)^n * fya)
        })

# -----------------------------------------------------------------
#' @title calculate the conditional skewness of remaining years of life.
#' 
#' @description Ratio of the 3rd to 2nd moments.
#' 
#' @param dx a vector of single age lifetable deaths (dx). Any radix.
#' @param ax Chiang's lifetable ax
#' 
#' @return vector of the age-specific skewness of the conditional distribution of remaining years of life
#' 
#' @export
getSkewst <- function(dx, ax){
    # formula from http://en.wikipedia.org/wiki/Skewness
    momentN(dx, n = 3, ax = ax) / (momentN(dx, n = 2, ax = ax) ^ (3/2)) 
}

# -----------------------------------------------------------------
#'
#' @title calculate the conditional kurtosis of remaining years of life.
#' 
#' @description Ratio of the 4th to 2nd moments.
#' 
#' @param dx a vector of single age lifetable deaths (dx). Any radix.
#' @param ax Chiang's lifetable ax. 
#' 
#' @return vector of the age-specific kurtosis of the conditional distribution of remaining years of life
#' 
#' @export
getKurtst <- function(dx, ax){
    momentN(dx, n = 4, ax = ax) / (momentN(dx, n = 2, ax = ax) ^ 2) 
}