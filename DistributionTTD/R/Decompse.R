# install.packages("devtools")
#library(devtools)
#install_github("DecompHoriuchi","timriffe",subdir="DecompHoriuchi")
library(DecompHoriuchi)
library(LexisUtils)
library(DemogBerkeley)

if (system("hostname",intern=TRUE)=="triffe-N80Vm"){
  # if I'm on the laptop
  setwd("/home/tim/git/DistributionTTD/DistributionTTD")
} else {
  # in that case I'm on Berkeley system, and other people in the dept can run this too
  setwd(paste0("/hdir/0/",system("whoami",intern=TRUE),"/git/DistributionTTD/DistributionTTD"))
}

source("R/Functions.R")
source("R/getvx.R")
source("R/mx2vx.R")
vxdecomp <- function(mx,x=50){
    sqrt(mx2vx(mx)[x+1])
}
# end preamble

# get data
flt <- readHMDweb("SWE","fltper_1x1")
mlt <- readHMDweb("SWE","mltper_1x1")

# compare two years
mxm1 <- mlt$mx[mlt$Year==1800]
mxm2 <- mlt$mx[mlt$Year==2010]

mxf1 <- flt$mx[flt$Year==1800]
mxf2 <- flt$mx[flt$Year==2010]
fvx <- DecompContinuousOrig(vxdecomp,mxf1,mxf2,N=50,x=50)

v51.1 <- mx2vx(mxf1)[51]
v51.2 <- mx2vx(mxf2)[51]

exf1 <- mx2exHMD(mxf1)
exf2 <- mx2exHMD(mxf2)
vxf1 <- mx2vx(mxf1)
vxf2 <- mx2vx(mxf2)


plot(0:110, mxf1, type='l',log="y")
par(mfrow=c(1,2))
plot(0:110, exf1,ylim=c(0,100),col="red",type="n",xlab="age",ylab="remaining years", main = "SWE females 1800")
polygon(c(0:110,110:0),c(exf1+sqrt(vxf1),rev(exf1-sqrt(vxf1))),col="#55000050")
lines(0:110,exf1,col="red")

plot(0:110, exf2, ylim=c(0,100),col="red",type="n",xlab="age",ylab="remaining years", main = "SWE females 1900")
polygon(c(0:110,110:0),c(exf2+sqrt(vxf2),rev(exf2-sqrt(vxf2))),col="#00555550")
lines(0:110,exf2,col="blue")

pdf("sd(y|x).pdf",height=9,width=9)
par(mfrow=c(2,2),mai=c(.5,.5,.5,.5))
plot(0:110, exf2, ylim=c(0,100),col="red",type="n",xlab="age",ylab="remaining years", main = "SWE females 1800 and 2010")
polygon(c(0:110,110:0),c(exf1+sqrt(vxf1),rev(exf1-sqrt(vxf1))),col="#55000050")
lines(0:110,exf1,col="red")
polygon(c(0:110,110:0),c(exf2+sqrt(vxf2),rev(exf2-sqrt(vxf2))),col="#00555550")
lines(0:110,exf2,col="blue")
abline(v=50)
x<- 0:110
plot(0:110, exf2+x, ylim=c(0,115),col="red",type="n",xlab="age",ylab="lifespan (x+ex or x+y)", main = "SWE females 1800 and 2010")
polygon(c(0:110,110:0),c(exf1+sqrt(vxf1)+x,rev(x+exf1-sqrt(vxf1))),col="#55000050")
polygon(c(0:110,110:0),c(exf2+x+sqrt(vxf2),rev(exf2+x-sqrt(vxf2))),col="#00555550")
lines(0:110,exf2+x,col="blue")
lines(0:110,exf1+x,col="red")
abline(v=50)

dxf1 <- mx2dxHMD(mxf1)
dxf2 <- mx2dxHMD(mxf2)
plot(50:110,dxf1[51:111]/sum(dxf1[51:111]),type='l', col = "red",ylim=c(0,.055),
        main="SWE females 1800 and 2010\nf(y|50), e50 and sd(y|50)",xlab="50+y",ylab="density")
lines(50:110,dxf2[51:111]/sum(dxf2[51:111]), col = "blue")
abline(v=exf1[51]+50,col="red")
abline(v=exf2[51]+50,col="blue")
rect(exf1[51]+50-2*sqrt(exf1[51]),-.5,exf1[51]+50+2*sqrt(exf1[51]),.5,col="#55550030")
rect(exf2[51]+50-2*sqrt(exf2[51]),-.5,exf2[51]+50+2*sqrt(exf2[51]),.5,col="#00555530")

plot(50:110,fvx[51:111],type="n",xlim=c(50,111),
        main="SWE females 1800 vs 2010\nage decomposition of difference in sd(y|50)",xlab="50+y",ylab="contribution to difference in sd(y|50)")
polygon(x= c(50,rep(51:110,each=2),111,rev(c(50,rep(51:110,each=2),111))),
        c(rep(fvx[51:111],each=2),rep(0,length(fvx[51:111])*2)),
        col="#555555")
dev.off()

getwd()