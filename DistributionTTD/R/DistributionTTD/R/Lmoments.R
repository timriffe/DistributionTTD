# functions originally written (and figured out) by Adam Lenart
# adapted to be functions of age, partially documented and packaged 
# by Tim Riffe, July 29, 2014

# -----------------------------------------------------
#' @title Adam's function getB0b, reworked as a function of age
#' 
#' @description Awaiting description from Adam. This amounts to a dx-weighted average remaining years of life, approximately equal to ex.
#' 
#' @param dx vector the lifetable deaths distribution
#' @param age vector of (single) completed ages
#' @param radix for scaling. Should be reasonably large, not 1 ...
#' 
#' @return age-specific vector of ...
#' 
#' @importFrom compiler cmpfun
#' 
#' @export
getB0b_ta <- cmpfun(function(dx, age, radix = 1e5){
            fya     <- da2fya(dx, FALSE) * radix
            age     <- matrix(age[col(fya)], nrow = nrow(fya), ncol = ncol(fya))
            rowSums(age * fya) / rowSums(fya)
        } )

# -----------------------------------------------------
#' @title Adam's function getB1b, reworked as a function of age
#' 
#' @description Awaiting description from Adam.
#' 
#' @param dx vector the lifetable deaths distribution
#' @param age vector of (single) completed ages
#' @param radix for scaling. Should be reasonably large, not 1 ...
#' 
#' @return vector of age-specific ...
#' 
#' @importFrom compiler cmpfun
#' 
#' @export
getB1b_ta <- cmpfun(function(dx, age, radix = 1e5){
            fya     <- da2fya(dx, FALSE) * radix
            age     <- matrix(age[col(fya)], nrow = nrow(fya), ncol = ncol(fya))
            W       <- cbind(0, t(apply(fya, 1, cumsum))[,-ncol(fya)])
            L       <- age * fya * (W + (1 / 2) * (fya - 1))
            1 / (radix * (radix - 1)) * rowSums(L)
        })

# -----------------------------------------------------
#' @title Adam's function getB2b, reworked as a function of age
#' 
#' @description Awaiting description from Adam.
#' 
#' @param dx vector the lifetable deaths distribution
#' @param age vector of (single) completed ages
#' @param radix for scaling. Should be reasonably large, not 1 ...
#' 
#' @return vector of age specific ...
#' 
#' @importFrom compiler cmpfun
#' 
#' @export
getB2b_ta <- cmpfun(function(dx, age, radix = 1e5){
            fya     <- da2fya(dx, FALSE) * radix
            age     <- matrix(age[col(fya)], nrow = nrow(fya), ncol = ncol(fya))
            W       <- cbind(0, t(apply(fya, 1, cumsum))[, -ncol(fya)])
            L       <- rowSums(fya * age * (W ^ 2 + W * (fya - 2) + (fya - 1) * (fya - 2) / 3))
            1 / (radix * (radix - 1) * (radix - 2)) * L
        })

# -----------------------------------------------------
#' @title Adam's function getL2b, reworked as a function of age
#'  
#' @description Awaiting further description from Adam. calculates age-specific 2nd L-moment.
#' 
#' @param dx vector the lifetable deaths distribution
#' @param age vector of (single) completed ages
#' @param radix for scaling. Should be reasonably large, not 1 ...
#' 
#' @return vector of age specific 2nd L-moments
#' 
#' @export
getL2b_ta <- function(dx, age, radix = 1e5){
    2 * getB1b_ta(dx = dx, age = age, radix = radix) - 
            getB0b_ta(dx = dx, age = age, radix = radix)
}

# -----------------------------------------------------
#' @title Adam's function getL3, reworked as a function of age
#'
#' @description Calculates age-specific 3rd L-Moment. Awaiting further description from Adam.
#' 
#' @param dx vector the lifetable deaths distribution
#' @param age vector of (single) completed ages
#' @param radix for scaling. Should be reasonably large, not 1 ...
#' 
#' @return vector of age specific 3rd L-moments
#' 
#' @export
getL3b_ta <- function(dx, age, radix = 1e5){
    6 * getB2b_ta(dx = dx, age = age, radix = radix) - 
            6 * getB1b_ta(dx = dx, age = age, radix = radix) +
            getB0b_ta(dx = dx, age = age, radix = radix)
}

# -----------------------------------------------------
#' @title Adam's function getLSkew, reworked as a function of age
#'
#' @description Ratio of third to second L-moments gives the skewness. This function calculates the L-skewness of the distribution of remaining years of life as a function of age.
#' 
#' @param dx vector the lifetable deaths distribution
#' @param age vector of (single) completed ages
#' @param radix for scaling. Should be reasonably large, not 1 ...
#' 
#' @return vector of age specific L-skewness
#' 
#' @export
getLSkew_ta <- function(dx, age, radix = 1e5){
    getL3b_ta(dx, age, radix = radix) / getL2b_ta(dx, age, radix = radix)  
} 

# -----------------------------------------------------
# 4th moment tbd. general moment function?
#getL4b_ta <- function(dx){
#    
#}
#getKurt_ta <- function(dx, age, radix = 1e5){
#    getL4b_ta(dx = dx, age = age, radix = radix) / getL2b_ta(dx = dx, age = age, radix = radix)  
#} 

# -----------------------------------------------------
#' @title Adam's function getLCV, reworked as a function of age
#' 
#' @description Calculates coefficient of L-variation, identical to Gini coefficient. Ratio of second to first L-moments.
#'
#' @param dx vector the lifetable deaths distribution
#' @param age vector of (single) completed ages
#' @param radix for scaling. Should be reasonably large, not 1 ...
#' 
#' @return vector of age specific L-CV
#' 
#' @export
getLCV_ta <- function(dx, age, radix = 1e5){
    getL2b_ta(dx, age = age, radix = radix) / getB0b_ta(dx, age = age, radix = radix)
}