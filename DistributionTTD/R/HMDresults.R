# Hi Adam, here is your script back, just the 'b' functions,
# no 'section' function. Now age-conditioned. See further comments below
setwd("/home/triffe/git/DistributionTTD/DistributionTTD")

# -----------------------------------------------------------------
# install from github /or/ do devtools::load_all("R/DistributionTTD", TRUE)
#library(devtools)
install_github("DistributionTTD", 
        subdir = "DistributionTTD/R/DistributionTTD", username = "timriffe")
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

# for when new variables added:
#LT <- local(get(load("Data/HMDresults.Rdata")))

# These calcs might take a while.
LT[, Lskew := getLSkew_ta(dx, age = Age + ax), by = list(CNTRY, Sex, Year)]
LT[, LCV   := getLCV_ta(dx, age = Age + ax),   by = list(CNTRY, Sex, Year)]
LT[, Lmad  := getB0b_ta(dx, age = Age + ax),   by = list(CNTRY, Sex, Year)]
LT[, L2    := getL2b_ta(dx, age = Age + ax),   by = list(CNTRY, Sex, Year)]
LT[, L3    := getL3b_ta(dx, age = Age + ax),   by = list(CNTRY, Sex, Year)]
## standard skew and kurtosis (from stanardized moments)
LT[, Sskew := getSkewst(dx,ax),    by = list(CNTRY, Sex, Year)]
LT[, Var   := momentN(dx,2,ax),    by = list(CNTRY, Sex, Year)]
LT[, ex2   := getex(dx,ax),        by = list(CNTRY, Sex, Year)]
LT$CV <- sqrt(LT$Var) / LT$ex2 # CV quicker to calculate this way
LT[, Skurt := getKurtst(dx,ax),    by = list(CNTRY, Sex, Year)]
# last ones take a while because of MANY splines being fit...
LT[, q25   := getQuantile(dx,.25), by = list(CNTRY, Sex, Year)]
LT[, q50   := getQuantile(dx,.5),  by = list(CNTRY, Sex, Year)] # er, median
LT[, q75   := getQuantile(dx,.75), by = list(CNTRY, Sex, Year)]
LT[, Mode  := getMode(dx),         by = list(CNTRY, Sex, Year)]

save(LT, file = "Data/HMDresults.Rdata")
head(LT)
str(LT)
print(object.size(LT),units="Mb") # 173.1 Mb
