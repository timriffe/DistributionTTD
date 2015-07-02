# compiles long-format all-cause complete lifetables from HMD.
# should only run once at beginning and again once at end 
# (for final data version in paper, to be saved with metadata)
# for Tim, this will choke
if (system("hostname",intern=TRUE) %in% c("triffe-N80Vm", "tim-ThinkPad-L440")){
	# if I'm on the laptop
	setwd("/home/tim/git/DistributionTTD/DistributionTTD")
} else {
	if (system("hostname",intern=TRUE) == "PC-403478"){
		# on MPIDR PC
		setwd("U://git//DistributionTTD//DistributionTTD")
	} else {
		# in that case I'm on Berkeley system, and other people in the dept can run this too
		setwd(paste0("/data/commons/",system("whoami",intern=TRUE),"/git/DistributionTTD/DistributionTTD"))
	}
}

#library(devtools)
#install_github("timriffe/TR1/TR1/HMDHFDplus")
library(HMDHFDplus)
library(data.table)
Countries <- getHMDcountries() # returns vector of HMD country codes

# us <- "HMD username" # change to yours: you need to be registered at www.mortality.org
# pw <- "HMD password" # change to yours
# makes long data frame of HMD lifetables, takes several minutes to run
LT <- do.call(rbind,lapply(Countries, function(XYZ, us, pw){
                    # downloads just like yours, but fixes some column classes.
					cat(XYZ,"\n")
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

LT   <- data.table(LT)

# for 'Date Accessed' entry in HMD reference. 
# Re-Run this script for the final version to regenerate clean results on latest data.
attr(LT,"timestamp") <- Sys.Date()
# save out for repeated use
save(LT, file = "Data/HMDltper.Rdata")


############################################################################
# make cohort lifetables: need to base it on cMx, import scripts as necessary

#
#LT <- do.call(rbind,lapply(Countries, function(XYZ, us, pw){
#      # downloads just like yours, but fixes some column classes.
#      cat(XYZ,"\n")
#      LTM      <- readHMDweb(XYZ, "mltper_1x1", username = us, password = pw)   
#      LTF      <- readHMDweb(XYZ, "fltper_1x1", username = us, password = pw) 
#      LTM$Sex  <- "m"
#      LTF$Sex  <- "f"
#      LT       <- rbind(LTM, LTF)
#      LT$CNTRY <- XYZ
#      LT
#    }, us = us, pw = pw))
## order columns (no worries, Age is integer)
#LT   <- LT[with(LT, order(CNTRY, Sex, Year, Age)),]
## convert to data.table
#
#LT   <- data.table(LT)
#
## for 'Date Accessed' entry in HMD reference. 
## Re-Run this script for the final version to regenerate clean results on latest data.
#attr(LT,"timestamp") <- Sys.Date()
## save out for repeated use
#save(LT, file = "Data/HMDltper.Rdata")






