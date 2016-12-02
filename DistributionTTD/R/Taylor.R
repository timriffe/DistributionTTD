
# TR: I basically only get straight lines in log-log coords for mx, but not for 
# most other lifetable measures, such as var, quantiles, ex, etc

# Author: tim
###############################################################################

# this script uses the HMDresults object to search for common patterns to the various defined measures.
if (system("hostname",intern=TRUE) %in% c("triffe-N80Vm", "tim-ThinkPad-L440")){
	# if I'm on the laptop
	setwd("/home/tim/git/DistributionTTD/DistributionTTD")
} else {
	# in that case I'm on Berkeley system, and other people in the dept can run this too
	setwd(paste0("/data/commons/",system("whoami",intern=TRUE),"/git/DistributionTTD/DistributionTTD"))
}
library(data.table)
library(reshape2)
library(LexisUtils)
library(RColorBrewer)
HMD <- local(get(load("Data/HMDresults.Rdata")))

head(HMD)
grays <- gray(seq(0,.7,length=111))
cexs <- seq(.5,2,length=111)
ind <- HMD$Age == 60
plot(HMD$ex2[ind], HMD$Var[ind], pch = 16, col = "#00000050", cex = 1, log='xy')

# start 1950:

# take variance and expectancy of the variance, by age, just to check this.

HMD <- HMD[HMD$Year >= 1950, ]

library(data.table)
HMD <- data.table(HMD)
Var <- HMD[,list(v=var(mx),e=mean(mx)),by=list(Year,Age)]
v <- acast(Var, Age~Year, value.var = "v")
e <- acast(Var, Age~Year, value.var = "e")
head(HMD)
Var <- HMD[,list(v=var(mx),e=mean(mx)),by=list(CNTRY,Age)]
v <- acast(Var, Age~CNTRY, value.var = "v")
e <- acast(Var, Age~CNTRY, value.var = "e")
matplot(e,v,log='xy', type = 'l')

matplot(t(e),t(v),log='xy', type = 'l')

codes <- get_codes()
head(codes)
#library(devtools)
#install_github("expersso/WHO")
library(WHO)
codes <- get_codes()
head(codes)