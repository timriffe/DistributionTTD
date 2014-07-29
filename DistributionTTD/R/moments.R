# Hi Adam, here is your script back, just the 'b' functions,
# no 'section' function. Now age-conditioned. See further comments below
setwd("/home/triffe/git/DistributionTTD/DistributionTTD")

# -----------------------------------------------------------------



# monotonic spline - interpolated median remaining years of life.
getMedian <- function(dx){
    fya     <- da2fya(dx)
    # needs age at clean breaks, like lx
    CDF     <- cbind(0,t(apply(fya,1,cumsum)))
    Age     <- 1:ncol(CDF) - 1
    # monotonic avoids negatives. Last value zero for ease, but not comparable with e_\omega
    medAges <- apply(CDF,1,function(cdf,Age){
                        if (length(unique(cdf)) > 2){
                            return(splinefun(Age~cdf, method = "monoH.FC")(.5))
                        } else {
                            # defaul value, didn't think much on this
                            return(0.5)
                        }
                    }, Age = Age)
    medAges[medAges < 0] <- 0 
    medAges 
}
# interpolated Modal remaining years of life
getMode <- function(dx){
    fya <- c(0,da2fya(dx),0)
    Age <- 1:ncol(fya) - 1
    
}

#library(devtools)
#install_github("DemogBerkeley", subdir = "DemogBerkeley", username = "UCBdemography")

LT <- local(get(load("Data/HMDltper.Rdata")))

# These calcs might take a while.
#LT[, Lskew := getSkew_ta(dx,age = Age + ax), by = list(CNTRY, Sex, Year)]
#LT[, LCV   := getCV_ta(dx,age = Age + ax),   by = list(CNTRY, Sex, Year)]
#LT[, Lmad  := getB0b_ta(dx, age = Age + ax), by = list(CNTRY, Sex, Year)]
#LT[, L2    := getL2b_ta(dx, age = Age + ax), by = list(CNTRY, Sex, Year)]
#LT[, L3    := getL3b_ta(dx, age = Age + ax), by = list(CNTRY, Sex, Year)]
## standard skew and kurtosis (from stanardized moments)
#LT[, Sskew := getSkewst(dx,ax),  by = list(CNTRY, Sex, Year)]
#LT[, Skurt := getKurtst(dx,ax),  by = list(CNTRY, Sex, Year)]
#LT[, eMed := getMedian(dx),  by = list(CNTRY, Sex, Year)]


i <- with(LT, Year == 2010 & Sex == "m" & CNTRY == "USA")
test <- LT[i,]
getSkew_ta(test$dx,age = test$Age + test$ax)
#plot(test$Age, getMedian(test$dx),type = 'l')
#lines(test$Age, test$ex,col="blue")
#
## neither skew measure benchmarks symmetry
#plot(test$Age, getMedian(test$dx) - test$ex,type = 'l')
#lines(test$Age, test$Sskew,col="blue")
#lines(test$Age, test$Lskew,col="green")
#abline(h=0)


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
