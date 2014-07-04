Mna0 <- compiler::cmpfun(function(M){
            M[is.na(M)]  <- 0
            M[is.nan(M)] <- 0
            M
        })

Minf0 <- compiler::cmpfun(function(M){
            M[is.infinite(M)]  <- 0
            M
        })
MinfNA <- compiler::cmpfun(function(M){
            M[is.infinite(M)]  <- NA
            M
        })


#'
#' @title AKm02q0 derive m0 from q0
#' 
#' @description Derive m0 from q0 according to the relevant segment of the Andreeve-Kingkade formula. This is elegant because it's an analytic solution, but ugly because, man, look at it. Carl Boe got this from Maple I think. This formula is only necessary because AK start with q0 whereas the HMD starts with m0, so we needed to adapt.
#' 
#' @param m0 a value of m0, per the period lifetable derivation
#' @param constant the intercept of the relevant Andreev-Kingkade segment
#' @param slope the slope of the relevant Andreev-Kingkade segment
#' 
#' @value q0 the estimate of q0 according to the identity between a0, m0, q0
#' 
#' @author Tim Riffe \email{triffe@@demog.berkeley.edu}
#' 
#' @export
#' 

AKm02q0 <- function(m0,constant,slope){
    -1 / slope / m0 * (-m0 +  (m0 * constant) - 0.1e1 + sqrt(((m0 ^ 2) - 2 * constant * (m0 ^ 2) + 2 * m0 + (constant ^ 2) * (m0 ^ 2) - 2 *  (m0 * constant) + 1 - 4 * slope * (m0 ^ 2)))) / 2
}

#' @title \code{AKm02a0} estimates a0 using the Andreev-Kinkade rule of thumb.
#'
#' @description
#' \code{AKm02a0} is an auxiliary function used by version 6 of the four HMD lifetable functions, \code{ltper_AxN()}, \code{ltcoh_AxN()}, \code{ltperBoth_AxN()}, \code{ltcohBoth_AxN()}. This function calls \code{AKm02q0()} to help get the work done, since the HMD needed to adapt the Andreev-Kingkade formulas to work with the period lifetable flow.
#'
#' @param m0 a value or vector of values of m0, the death rate for age 0 infants.
#' @param sex either "m" or "f"
#' 
#' @return a0, the estimated average age at death of those dying in the first year of life, either a single value or a vector of a_0 values.
#' 
#' @author Tim Riffe \email{triffe@@demog.berkeley.edu}
#' 
#' @export

AKm02a0 <- function(m0,sex="m"){
    sex <- rep(sex,length(m0))
    ifelse(sex == "m",
            ifelse(m0 < 0.02306737, 0.1493 - 2.0367 * AKm02q0(m0, 0.1493, -2.0367),
                    ifelse(m0 < 0.0830706, 0.0244 + 3.4994 * AKm02q0(m0, 0.0244, 3.4994), .2991)),
            ifelse(m0 < 0.01725977, 0.1490 - 2.0867 * AKm02q0(m0, 0.1490, -2.0867),
                    ifelse(m0 < 0.06919348, 0.0438 + 4.1075 * AKm02q0(m0, 0.0438, 4.1075), 0.3141))
    )
}

# need mu2dx, refine for a0.
mx2dxHMD <- compiler::cmpfun(function(mx, sex = "m"){
            mx                  <- Mna0(as.numeric(mx))
            
            # mean proportion of interval passed at death
            ax                  <- mx * 0 + .5                      # ax = .5, pg 38 MPv5
            
            ax[1]               <- AKm02a0(mx[1], sex)
            
            qx                  <- mx / (1 + (1 - ax) * mx)          # Eq 60 MPv5 (identity)
# ---------------------------------------------------------------------------------
# set open age qx to 1
            i.openage           <- length(mx) # removed argument OPENAGE
            qx[i.openage]       <- 1
            ax[i.openage]       <- 1 / mx[i.openage]                   
# ---------------------------------------------------------------------------------
# define remaining lifetable columns:
            px                  <- 1 - qx                                                                                 # Eq 64 MPv5
            px[is.nan(px)]      <- 0 # skips BEL NAs, as these are distinct from NaNs
# lx needs to be done columnwise over px, argument 2 refers to the margin.
            lx                  <- c(1, cumprod(px[1:(i.openage-1)]))
            # NA should only be possible if there was a death with no Exp below age 80- impossible, but just to be sure
            # lx[is.na(lx)]   <- 0 # removed for BEL testing        
            dx                  <- lx * qx                                                                                # Eq 66 MPv5
            Mna0(dx)
        })

mx2exHMD <- compiler::cmpfun(function(mx, sex = "m"){
            mx                  <- Mna0(as.numeric(mx))
            
            # mean proportion of interval passed at death
            ax                  <- mx * 0 + .5                      # ax = .5, pg 38 MPv5
            
            ax[1]               <- AKm02a0(mx[1], sex)
            
            qx                  <- mx / (1 + (1 - ax) * mx)          # Eq 60 MPv5 (identity)
# ---------------------------------------------------------------------------------
# set open age qx to 1
            i.openage           <- length(mx) # removed argument OPENAGE
            qx[i.openage]       <- 1
            ax[i.openage]       <- 1 / mx[i.openage-1]                   
# ---------------------------------------------------------------------------------
# define remaining lifetable columns:
            px                  <- 1 - qx                                                                                 # Eq 64 MPv5
            px[is.nan(px)]      <- 0 # skips BEL NAs, as these are distinct from NaNs
# lx needs to be done columnwise over px, argument 2 refers to the margin.
            lx                  <- c(1, cumprod(px[1:(i.openage-1)]))
            # NA should only be possible if there was a death with no Exp below age 80- impossible, but just to be sure
            # lx[is.na(lx)]   <- 0 # removed for BEL testing        
            dx                  <- lx * qx                                                                                # Eq 66 MPv5
            
            Lx                  <- lx - (1 - ax) * dx                                                         # Eq 67 MPv5
            Lx[i.openage]     <- lx[i.openage] * ax[i.openage]
# we need to do operations on Lx, but taking its NAs to mean 0
# Lx[is.na(Lx)]    <- 0 # removed for BEL testing
# Tx needs to be done columnwise over Lx, argument 2 refers to the column margin.
            Tx                      <- c(rev(cumsum(rev(Lx[1:(i.openage-1)]))),0) + Lx[i.openage] # Eq 68 MPv5
            ex                      <- Tx / lx 
            # ad hoc return
            ex
        })

mx2lxHMD <- compiler::cmpfun(function(mx, sex = "m"){
            mx                  <- Mna0(as.numeric(mx))
            
            # mean proportion of interval passed at death
            ax                  <- mx * 0 + .5                      # ax = .5, pg 38 MPv5
            
            ax[1]               <- AKm02a0(mx[1], sex)
            
            qx                  <- mx / (1 + (1 - ax) * mx)          # Eq 60 MPv5 (identity)
# ---------------------------------------------------------------------------------
# set open age qx to 1
            i.openage           <- length(mx) # removed argument OPENAGE
            qx[i.openage]       <- 1
            ax[i.openage]       <- 1 / mx[i.openage-1]                   
# ---------------------------------------------------------------------------------
# define remaining lifetable columns:
            px                  <- 1 - qx                                                                                 # Eq 64 MPv5
            px[is.nan(px)]      <- 0 # skips BEL NAs, as these are distinct from NaNs
# lx needs to be done columnwise over px, argument 2 refers to the margin.
            lx                  <- c(1, cumprod(px[1:(i.openage-1)]))
            # NA should only be possible if there was a death with no Exp below age 80- impossible, but just to be sure
            # lx[is.na(lx)]   <- 0 # removed for BEL testing        
            lx
        })

dx2lx <- function(dx){
    rev(cumsum(rev(dx))) / sum(dx)
}


