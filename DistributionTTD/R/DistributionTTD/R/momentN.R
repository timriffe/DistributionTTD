#'
#' @title calculate the Nth moment of remaining years of life as a function of age
#' 
#' @description The Nth moment of the conditional distribution of remaining years of life, \eqn{\eta _n(y|a)} is defined as:
#' \deqn{\eta _n(y|a) =  \int_{y=0}^\infty (y-e(a))^n f(y|a) \text{d}y}
#' ax is used as an argument to account for infants. If missing, assumed as .5. Ages assumed single.
#' 
#' @param da a vector of single age lifetable deaths (dx). Any radix.
#' @param n which moment?
#' @param ax Chiang's lifetable ax. 
#' 
#' @return vector of age specific nth moments.
#' 
#' @importFrom compiler cmpfun
#' 
#' @export 
#' 

momentN <- cmpfun(function(dx, n, ax = rep(.5, length(dx))){
            fya <- da2fya(dx)
            ex  <- rowSums((col(fya) - (1 - ax)) * fya)
            rowSums(((col(fya) - .5) - ex)^n * fya)
        })

getSkewst <- function(dx, ax){
    # formula from http://en.wikipedia.org/wiki/Skewness
    momentN(dx, n = 3, ax = ax) / (momentN(dx, n = 2, ax = ax) ^ (3/2)) 
}
getKurtst <- function(dx, ax){
    momentN(dx, n = 4, ax = ax) / (momentN(dx, n = 2, ax = ax) ^ 2) 
}