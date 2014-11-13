



# this script uses the HMDresults object to search for common patterns to the various defined measures.
if (system("hostname",intern=TRUE)=="triffe-N80Vm"){
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


id <- with(HMD, CNTRY == "SWE" & Year == 2010 & Sex == "f")

mx <- HMD$mx[id]
dx <- HMD$dx[id] / sum(HMD$dx[id])
lx <- HMD$lx[id] / 1e5
Lx <- HMD$Lx[id] / 1e5
ex <- HMD$ex[id]
disparity1 <- function(dx,ex){
  sum(dx*ex)
}
disparity2 <- function(mx){
  sum(cumsum(mx)*exp(-cumsum(mx)))
}
disparity3 <- function(lx){
  sum(lx*log(1/lx))
}

entropy1 <- function(dx,ex){
  disparity1(dx,ex) / ex[1]
}
entropy2 <- function(mx,ex){
  disparity2(mx) / ex[1]
}
entropy3 <- function(lx,ex){
  disparity3(lx) / ex[1]
}
disparity1(dx,ex)
disparity2(mx)
disparity3(lx)
entropy1(dx,ex)
entropy2(mx,ex)
entropy3(lx,ex)

variance <- function(dx,ex,x=(1:length(dx)-1)){
  sum(dx*(ex[1]-x)^2)
}
variance(dx,ex)

COD <- local(get(load("/data/commons/triffe/git/YearsLost/YearsLost/Data/COD.Rdata")))


head(COD[[1]][["Mxmc"]])
names(COD[[1]])

CAN <- read.csv("/data/commons/triffe/git/YearsLost/YearsLost/Data/COD5x1/CAN_5x1_chapters.csv")

r8 <- runif(8)
props <- cumsum(r8)/sum(cumsum(r8))

library(sp)
areas <- SpatialPolygons(list(
    Polygons(list(Polygon(cbind(x=c(0,2,2,1,0,0),y=c(0,0,2,2,1,0)), hole=FALSE)),"1"),
    Polygons(list(Polygon(cbind(x=c(2,4,4,3,3,2,2),y=c(0,0,2,2,1,1,0)),hole=FALSE)),"2"),
    Polygons(list(Polygon(cbind(x=c(4,5,5,4,4),y=c(0,0,3,2,0)),hole=FALSE)),"3"),
    Polygons(list(Polygon(cbind(x=c(0,1,2,2,0,0),y=c(1,2,2,3,3,1)),hole=FALSE)),"4"),
    Polygons(list(Polygon(cbind(x=c(2,3,3,4,4,3,3,2,2),y=c(1,1,2,2,3,3,4,4,1)),hole=FALSE)),"5"),
    Polygons(list(Polygon(cbind(x=c(0,2,2,1,0,0),y=c(3,3,4,5,5,3)),hole=FALSE)),"6"),
    Polygons(list(Polygon(cbind(x=c(1,2,3,4,1),y=c(5,4,4,5,5)),hole=FALSE)),"7"),
    Polygons(list(Polygon(cbind(x=c(3,4,4,5,5,4,3,3),y=c(3,3,2,3,5,5,4,3)),hole=FALSE)),"8")
  ))
areas <- SpatialPolygonsDataFrame(areas, data = data.frame(ID=paste0(1:8),
    row.names=paste0(1:8),
    stringsAsFactors=FALSE)
)
slotNames(areas)
areas@polygons[[1]]@area


