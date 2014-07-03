

mx2vx <- compiler::cmpfun(function(mx, sex = "m"){
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
            Lx[i.openage]       <- lx[i.openage] * ax[i.openage]
# we need to do operations on Lx, but taking its NAs to mean 0
# Lx[is.na(Lx)]    <- 0 # removed for BEL testing
# Tx needs to be done columnwise over Lx, argument 2 refers to the column margin.
            Tx                      <- c(rev(cumsum(rev(Lx[1:(i.openage-1)]))),0) + Lx[i.openage] # Eq 68 MPv5
            ex                      <- Tx / lx 
            # ad hoc return
            getvx(lx,ex)
            
        })

