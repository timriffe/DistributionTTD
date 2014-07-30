# Hi Adam, here is your script back, just the 'b' functions,
# no 'section' function. Now age-conditioned. See further comments below
setwd("/home/triffe/git/DistributionTTD/DistributionTTD")

# -----------------------------------------------------------------
# install from github /or/ do devtools::load_all("R/DistributionTTD", TRUE)
#library(devtools)
#install_github("DistributionTTD", 
#        subdir = "DistributionTTD/R/DistributionTTD", username = "timriffe")
library(DistributionTTD)
# what functions do we have?
ls("package:DistributionTTD")
# silly utilities: "Minf0", "MinfNA", "Mna0"
# L-moment-related: "getB0b_ta", "getB1b_ta", "getB2b_ta", "getL2b_ta", 
#                   "getL3b_ta", "getLCV_ta", "getLSkew_ta"
# General distribution-related: "da2fya", "momentN", "getMedian", 
#                   "getMode", "getQuantile", "getSkewst", "getKurtst" 

# waiting to be written: getCVst(), getGinest()
# ------------------------------------------------------
library(data.table)
LT <- local(get(load("Data/HMDltper.Rdata")))

# These calcs might take a while.
LT[, Lskew := getLSkew_ta(dx,age = Age + ax), by = list(CNTRY, Sex, Year)]
LT[, LCV   := getLCV_ta(dx,age = Age + ax),   by = list(CNTRY, Sex, Year)]
LT[, Lmad  := getB0b_ta(dx, age = Age + ax), by = list(CNTRY, Sex, Year)]
LT[, L2    := getL2b_ta(dx, age = Age + ax), by = list(CNTRY, Sex, Year)]
LT[, L3    := getL3b_ta(dx, age = Age + ax), by = list(CNTRY, Sex, Year)]
## standard skew and kurtosis (from stanardized moments)
LT[, Sskew := getSkewst(dx,ax),    by = list(CNTRY, Sex, Year)]
# LT[, CV := getCVst(dx,ax),  by = list(CNTRY, Sex, Year)] # not written yet
LT[, Skurt := getKurtst(dx,ax),    by = list(CNTRY, Sex, Year)]
# last ones take a while because of MANY splines being fit...
LT[, q25   := getQuantile(dx,.25), by = list(CNTRY, Sex, Year)]
LT[, q50   := getQuantile(dx,.5),  by = list(CNTRY, Sex, Year)] # er, median
LT[, q75   := getQuantile(dx,.75), by = list(CNTRY, Sex, Year)]
LT[, Mode  := getMode(dx),         by = list(CNTRY, Sex, Year)]

save(LT, file = "Data/HMDresults.Rdata")
head(LT)
str(LT)
print(object.size(LT),units="Mb") # 121.5 Mb
#i <- with(LT, Year == 2010 & Sex == "m" & CNTRY == "USA")
#test <- LT[i,]
#dx <- test$dx
#getQuantile(test$dx)
#
#getSkew_ta(test$dx,age = test$Age + test$ax)
#plot(test$Age, getMedian(test$dx),type = 'l')
#lines(test$Age, test$ex,col="blue")
#
## neither skew measure benchmarks symmetry
#plot(test$Age, getMedian(test$dx) - test$ex,type = 'l')
#lines(test$Age, test$Sskew,col="blue")
#lines(test$Age, test$Lskew,col="green")
#abline(h=0)


## ex differences in young ages.
#plot(test$ex, test$Lmad)
## compare skew, not same
#plot(test$Sskew,test$Lskew,main = "seem to agree about crossover age")
#abline(h=0);abline(v=0)
#
## again compare, seems that Standard skew is similar but scaled up
#plot(test$Age, test$Sskew, type = 'l')
#lines(test$Age, test$Lskew, col = "blue")
#abline(v=test$ex[1])
## does Skew = 0 at the age where median remaining = mean remaining?
## not necessarily, since it doesn't measure symmetry per se.
#
## except it's not a simple scalar
#plot(test$Age, test$Sskew / test$Lskew)
#
## look at standard kurtosis (still need formula for 4th L moment??
#plot(test$Age, test$Skurt, type='l')
#abline(v=test$ex[1])
#
## LexisMap() comes from LexisUtils, on github:
## devtools::install_github("LexisUtils", subdir = "LexisUtils", username = "timriffe")
##LexisUtils::LexisMap(
##        reshape2::acast(LT[LT$CNTRY == "SWE" & LT$Sex == "f", ], Age~Year, value.var = "skew"),
##        log=FALSE)
##LexisUtils::LexisMap(
##        reshape2::acast(LT[LT$CNTRY == "SWE" & LT$Sex == "f", ], Age~Year, value.var = "CV"),
##        log=FALSE)
#
