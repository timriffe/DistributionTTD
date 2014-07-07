# Hi Adam, here is your script back, just the 'b' functions,
# no 'section' function. Now age-conditioned. See further comments below
setwd("/home/triffe/git/DistributionTTD/DistributionTTD")

# silly utils:
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
# this returns a matrix where each row is the dx vector shifted up one age
# and rescaled to sum to 1.
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

# in following are you 'b' functions, loop-free and age-conditioned.
# i.e. a vector is returned. If age is 0:110, then the first element of the
# vector is identical to your 'b' functions if radix = 1e5 (assuming the HMD
# dx indeed sums to 1e5, which it sometimes does not due to rounding)

# other minor change, age is specified mid-interval, or is that wrong?
# radix is given as an argument because it affects results. Can the implementation
# be made robust to this? Seems that the 'c' functions won't care about scale, as long
# as the radix is not 1? We should be able to calculate things with radix-1 lifetables, right?

# OK getting on with it: everything is the same as yours, but with '_ta' appended to the name
getB0b_ta <- function(dx, radix = 1e5, n = length(dx), age = (1:n) - .5){
    fya     <- da2fya(dx, FALSE) * radix
    age     <- matrix(age[col(fya)], nrow = nrow(fya), ncol = ncol(fya))
    rowSums(age * fya) / rowSums(fya)
} 

getB1b_ta <- function(dx, radix = 1e5, n = length(dx), age = (1:n) - .5){
    fya     <- da2fya(dx, FALSE) * radix
    age     <- matrix(age[col(fya)], nrow = nrow(fya), ncol = ncol(fya))
    W       <- cbind(0, t(apply(fya, 1, cumsum))[,-n])
    L       <- age * fya * (W + (1 / 2) * (fya - 1))
    1 / (radix * (radix - 1)) * rowSums(L)
 }

getB2b_ta <- function(dx, radix = 1e5, n = length(dx), age = (1:n) - .5){
    fya     <- da2fya(dx, FALSE) * radix
    age     <- matrix(age[col(fya)], nrow = nrow(fya), ncol = ncol(fya))
    W       <- cbind(0, t(apply(fya, 1, cumsum))[, -n])
    L       <- rowSums(fya * age * (W ^ 2 + W * (fya - 2) + (fya - 1) * (fya - 2) / 3))
    1 / (radix * (radix - 1) * (radix - 2)) * L
}

getL2b_ta <- function(dx){
    2 * getB1b_ta(dx = dx) - 
            getB0b_ta(dx = dx)
}

getL3b_ta <- function(dx){
    6 * getB2b_ta(dx = dx) - 
            6 * getB1b_ta(dx = dx) +
            getB0b_ta(dx = dx)
}

getSkew_ta <- function(dx){
    getL3b_ta(dx) / getL2b_ta(dx)  
} 

# 4th moment tbd. general moment function?
#getL4b_ta <- function(dx){
#    
#}
#getKurt_ta <- function(dx){
#    getL4b_ta(dx) / getL2b_ta(dx)  
#} 


getCV_ta <- function(dx){
    getL2b_ta(dx) / getB0b_ta(dx)
}

# -----------------------------------------------------------------
# how does this compare with general moments?
#\eta _n(y|a) =&  \int_{y=0}^\infty (y-e(a))^n f(y|a) \dd y \tc
momentN <- function(dx,n){
    fya <- da2fya(dx)
    ex  <- rowSums((col(fya) - .5) * fya)
    rowSums(((col(fya) - .5) - ex)^n * fya)
}

getSkewst <- function(dx){
    # formula from http://en.wikipedia.org/wiki/Skewness
    momentN(dx,3) / (momentN(dx,2) ^ (3/2)) 
}
getKurtst <- function(dx){
    momentN(dx,4) / (momentN(dx,2) ^ 2) 
}

getMedian <- function(dx,n = length(dx),age = (1:n-1)){
    fya     <- da2fya(dx)
    # needs age at clean breaks, like lx
    CDF     <- t(apply(fya,1,cumsum))
    # monotonic avoids negatives. Last value zero for ease, but not comparable with e_\omega
    med <- c(apply(CDF[-n,],1,function(cdf,age){
                splinefun(age~cdf,method="monoH.FC")(.5) 
            }, age = age),0)
    med
}



plot(test$Age, getMedian(test$dx),type = 'l')
lines(test$Age, test$ex,col="blue")

# neither skew measure benchmarks symmetry
plot(test$Age, getMedian(test$dx) - test$ex,type = 'l')
lines(test$Age, test$Sskew,col="blue")
lines(test$Age, test$Lskew,col="green")
abline(h=0)

#library(devtools)
#install_github("DemogBerkeley", subdir = "DemogBerkeley", username = "UCBdemography")
library(DemogBerkeley)
Countries <- getHMDcountries() # returns vector of HMD country codes

# us <- "adam's username"
# pw <- "adam's password"
# makes long data frame of HMD lifetables, takes several minutes to run
LT <- do.call(rbind,lapply(Countries, function(XYZ,us,pw){
            # downloads just like yours, but fixes some column classes.
            LTM      <- readHMDweb(XYZ,"mltper_1x1",username=us,password=pw)   
            LTF      <- readHMDweb(XYZ,"fltper_1x1",username=us,password=pw) 
            LTM$Sex  <- "m"
            LTF$Sex  <- "f"
            LT       <- rbind(LTM, LTF)
            LT$CNTRY <- XYZ
            LT
        },us=us,pw=pw))
# order columns (no worries, Age is integer)
LT <- LT[with(LT, order(CNTRY,Sex,Year,Age)),]
# convert to data.table
library(data.table)
LT   <- data.table(LT)

# This is just quick because it avoids df copying somehow..
LT[, Lskew := getSkew_ta(dx), by = list(CNTRY, Sex, Year)]
LT[, LCV   := getCV_ta(dx),   by = list(CNTRY, Sex, Year)]
LT[, Lmad  := getB0b_ta(dx),  by = list(CNTRY, Sex, Year)]
LT[, L2    := getL2b_ta(dx),  by = list(CNTRY, Sex, Year)]
LT[, L3    := getL3b_ta(dx),  by = list(CNTRY, Sex, Year)]
# standard skew and kurtosis (from stanardized moments)
LT[, Sskew := getSkewst(dx),  by = list(CNTRY, Sex, Year)]
LT[, Skurt := getKurtst(dx),  by = list(CNTRY, Sex, Year)]
# need to make getMedian() more robust to lifetables that close out early.
#LT[, eMed := getMedian(dx),  by = list(CNTRY, Sex, Year)]
# you can use it just like a df as well, no worries.
save(LT, file = "Data/LTM.Rdata")

i <- with(LT, Year == 2010 & Sex == "m" & CNTRY == "USA")
test <- LT[i,]
# ex differences in young ages.
plot(test$ex, test$Lmad)
# compare skew, not same
plot(test$Sskew,test$Lskew,main = "seem to agree about crossover age")
abline(h=0);abline(v=0)

# again compare, seems that Standard skew is similar but scaled up
plot(test$Age, test$Sskew, type = 'l')
lines(test$Age, test$Lskew, col = "blue")
abline(v=test$ex[1])
# does Skew = 0 at the age where median remaining = mean remaining?
# not necessarily, since it doesn't measure symmetry per se.

# except it's not a simple scalar
plot(test$Age, test$Sskew / test$Lskew)

# look at standard kurtosis (still need formula for 4th L moment??
plot(test$Age, test$Skurt, type='l')
abline(v=test$ex[1])

# LexisMap() comes from LexisUtils, on github:
# devtools::install_github("LexisUtils", subdir = "LexisUtils", username = "timriffe")
#LexisUtils::LexisMap(
#        reshape2::acast(LT[LT$CNTRY == "SWE" & LT$Sex == "f", ], Age~Year, value.var = "skew"),
#        log=FALSE)
#LexisUtils::LexisMap(
#        reshape2::acast(LT[LT$CNTRY == "SWE" & LT$Sex == "f", ], Age~Year, value.var = "CV"),
#        log=FALSE)
print(object.size(LT),units="Mb") # 121.5 Mb
