#' 
#' @title replace NAs and NaNs with 0
#' 
#' @param M a vector or matrix
#' 
#' @return M, with NAs and NaNs imputed with zeros.
#' 
#' @importFrom compiler cmpfun
#' 
#' @export
#' 
Mna0 <- compiler::cmpfun(function(M){
            M[is.na(M)]  <- 0
            M[is.nan(M)] <- 0
            M
        })
#' 
#' @title replace Inf elements with 0
#' 
#' @param M a vector or matrix
#' 
#' @return M, with Inf imputed with zeros.
#' 
#' @importFrom compiler cmpfun
#' 
#' @export
#' 
Minf0 <- compiler::cmpfun(function(M){
            M[is.infinite(M)]  <- 0
            M
        })
#' 
#' @title replace Inf elements with NA
#' 
#' @param M a vector or matrix
#' 
#' @return M, with Inf imputed with NAs.
#' 
#' @importFrom compiler cmpfun
#' 
#' @export
#' 
MinfNA <- compiler::cmpfun(function(M){
            M[is.infinite(M)]  <- NA
            M
        })

#'
#' @title produce a matrix containing the age-specific distribution of remaining lifespans
#' 
#' @description Given a single-age vector of lifetable deaths (any radix), we convert to a matrix with chronological age in rows and thanatological age in columns. Each row represents the distribution of remaining lifespans for the given chronological age, and is scaled to sum to 1.
#' 
#' @details if \code{stagger} is \code{TRUE}, age 0 is not treated specially at this time, even though most of \eqn{d_0} is concentrated in the first day and weeks.
#' 
#' @param da a vector of single age lifetable deaths (dx). 
#' @param stagger logical. Should the da vector be element-wise averaged to account for deaths being spread through the year?
#' 
#' @return a matrix of the conditional distribution of remaining lifetime: chronological age in rows and remaining years in columns.
#' 
#' @export
#' 
da2fya <- function(da, stagger = FALSE){
    N       <- length(da)
    ay      <- 1:N - 1
    
    da      <- Mna0(da)   # remove NAs if any       
    da      <- c(da, da * 0) / sum(da) # pad out with 0s
    fya     <- matrix(da[col(matrix(nrow = N, 
                                    ncol = N)) + ay], 
            nrow = N, 
            ncol = N, 
            dimnames = list(Ex = ay, 
                    Age = ay)
    )
    if (stagger){
        fya <- (fya + cbind(fya[, 2:ncol(fya)], 0)) / 2
    }
    fya <- Minf0(Mna0(fya / rowSums(fya)))
    fya
}
