#'
#' @title calculate the conditional median remaining years of life
#' 
#' @description This function uses a monotonic spline function fit to the cdf to approximate the median remaining years of life for each age. Final values may be iffy, you may wish to truncate after, say, omega - 5.
#' 
#' @param dx a vector of single age lifetable deaths, any radix 
#' 
#' @return numeric vector of median remaining lifespans. 
#' 
#' @export
#' 
# monotonic spline - interpolated median remaining years of life.
getMedian <- function(dx){
    fya     <- da2fya(dx)
    # needs age at clean breaks, like lx
    CDF     <- cbind(0, t(apply(fya, 1, cumsum)))
    Age     <- 1:ncol(CDF) - 1
    # monotonic avoids negatives. Last value zero for ease, but not comparable with e_\omega
    medAges <- apply(CDF, 1, function(cdf, Age){
                if (length(unique(cdf)) > 2){
                    return(splinefun(Age~cdf, method = "monoH.FC")(.5))
                } else {
                    # default value, didn't think much on this
                    return(0.5)
                }
            }, Age = Age)
    medAges[medAges < 0] <- 0 
    medAges 
}

#'
#' @title calculate quantiles of the conditional distribution of remaining years of life
#' 
#' @description This function uses a monotonic spline function fit to the cdf to approximate quantiles from the remaining years of life for each age. Final values may be iffy, you may wish to truncate after, say, omega - 5.
#' 
#' @param dx a vector of single age lifetable deaths, any radix 
#' @param probs a vector of quantiles
#' 
#' @return a vector or matrix of quantiles from the distribution of remaining lifespans.
#' 
#' @export
#' 

# monotonic spline - interpolated median remaining years of life.
getQuantile <- function(dx, probs = c(.1,.25,.5,.75,.9)){
    fya     <- da2fya(dx)
    # needs age at clean breaks, like lx
    CDF     <- cbind(0, t(apply(fya, 1, cumsum)))
    Age     <- 1:ncol(CDF) - 1
    # monotonic avoids negatives. Last value zero for ease, but not comparable with e_\omega
    Quantiles <- sapply(probs, function(p, CDF, Age){
                medAges <- apply(CDF, 1, function(cdf, Age, p){
                            if (length(unique(cdf)) > 2){
                                return(splinefun(Age~cdf, method = "monoH.FC")(p))
                            } else {
                                # default value, didn't think much on this
                                return(0.5)
                            }
                        }, Age = Age, p = p)     
            }, CDF = CDF, Age = Age)
    colnames(Quantiles) <- probs
    Quantiles[Quantiles < 0] <- 0 
    Quantiles 
}

#'
#' @title calculate the conditional modal remaining years of life
#' 
#' @description This function uses a spline function fit to the conditional remaining lifetime function to approximate the modal remaining years of life for each age. This function might not be robust. Note, results can seem like a function of discrete segments because a mode can be the same for many ages, getting 1 year closer each age. It's less aesthetically interesting than the median remaining lifespan, but still potentially interesting. After the primary old age mode, results get choppy due to noise.
#' 
#' @param dx a vector of single age lifetable deaths, any radix 
#' 
#' @return numeric vector of modal remaining lifespans. 
#' 
#' @export
#' 
getMode <- function(dx){
    fya <- cbind(da2fya(dx),0)
    Age <- 1:ncol(fya) - 1
    
    # spline minimizer function. Deriv = 0 in neighborhood of integer max
    minMode <- function(y, myspline){
        myspline(y, deriv = 1)^2
    }
   
    # apply over rows of fy.
    apply(fya,1, function(fy,y){
                myspline           <- splinefun(fy ~ y)
                bounds             <- which.max(fy) + c(-2,2) 
                bounds[bounds < 0] <- 0            
                optimize(myspline, lower = bounds[1], upper = bounds[2])$minimum
            }, y = 0:111)
}



