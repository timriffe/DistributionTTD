#' 
#' @title dx2lx a simple conversion function
#' 
#' @description Functionalized for the sake of modularity. No NA checking done.
#' 
#' @param dx the lifetable dx column, any radix ok
#' @param radix, the destination radix, l(0)
#' 
#' @return lx a survival function, with l(0) set to radix
#' 
#' @export
#' 

dx2lx <- function(dx, radix = 1){
    radix * (rev(cumsum(rev(dx))) / sum(dx))
}


