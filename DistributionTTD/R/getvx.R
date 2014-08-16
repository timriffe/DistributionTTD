# not used. Subsumed in package functions.

# get variance in time until death, where mean time until death is simply ex
# both arguments full lifetable vectors.
getvx <- compiler::cmpfun(function(lx, ex){
            lx     <- lx / lx[1]
            N      <- length(lx)
            x      <- 1:N - 1
            X      <- rep(x,N)
            dim(X) <- c(N,N)
            Y      <- t(X)-x
            fx     <- c(-diff(lx), lx[N])
            rowSums(
                    ((Y - ex)^2) * upper.tri(Y,TRUE) * 
                            t(outer(fx,lx,"/") * lower.tri(Y,TRUE))
            )
        })
