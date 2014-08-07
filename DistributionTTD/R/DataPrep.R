# compiles long-format all-cause complete lifetables from HMD.
# should only run once at beginning and again once at end 
# (for final data version in paper, to be saved with metadata)

setwd("/home/triffe/git/DistributionTTD/DistributionTTD")

#library(devtools)
#install_github("DemogBerkeley", subdir = "DemogBerkeley", username = "UCBdemography")
library(DemogBerkeley)
Countries <- getHMDcountries() # returns vector of HMD country codes

# us <- "HMD username" # change to yours: you need to be registered at www.mortality.org
# pw <- "HMD password" # change to yours
# makes long data frame of HMD lifetables, takes several minutes to run
LT <- do.call(rbind,lapply(Countries, function(XYZ, us, pw){
                    # downloads just like yours, but fixes some column classes.
                    LTM      <- readHMDweb(XYZ, "mltper_1x1", username = us, password = pw)   
                    LTF      <- readHMDweb(XYZ, "fltper_1x1", username = us, password = pw) 
                    LTM$Sex  <- "m"
                    LTF$Sex  <- "f"
                    LT       <- rbind(LTM, LTF)
                    LT$CNTRY <- XYZ
                    LT
                }, us = us, pw = pw))
# order columns (no worries, Age is integer)
LT   <- LT[with(LT, order(CNTRY, Sex, Year, Age)),]
# convert to data.table
library(data.table)
LT   <- data.table(LT)

# for 'Date Accessed' entry in HMD reference. 
# Re-Run this script for the final version to regenerate clean results on latest data.
attr(LT,"timestamp") <- Sys.Date()
# save out for repeated use
save(LT, file = "Data/HMDltper.Rdata")