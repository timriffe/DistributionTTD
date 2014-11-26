



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
variance(dx,ex) / ex[1]
ex[1] / variance(dx,ex) 
library(devtools)
load_all("/data/commons/triffe/git/DistributionTTD/DistributionTTD/R/DistributionTTD")

fya <- da2fya(dx)

varya <- function(dx){
  y   <- 1:length(dx)-.5
  fya <- da2fya(dx)
  ex  <- colSums(t(fya) * y)
  rowSums(outer(ex,y,"-")^2 * fya)
}
dx2ex <- function(dx){
  y   <- 1:length(dx)-.5
  fya <- da2fya(dx)
  ex  <- colSums(t(fya) * y)
}
CVa <- function(dx,ex){
  
}
plot(lx)
lines(dx)
plot(dx2ex(dx))
plot(varya(dx), main = "variance")
plot(sqrt(varya(dx)), main = "standard deviation")
plot(sqrt(varya(dx))/dx2ex(dx), main = "coefficient of variation")
plot(dx2ex(dx)/sqrt(varya(dx)), main = "signal to noise" )

#
#props <- cumsum(runif(8))
#props <- props / sum(props)
#
#sum(round(100*props))
#radius <- sqrt(max(props)/pi)
#
#
#plot(NULL,type='l',xlim=c(0,2*pi),ylim=c(0,1), xaxs="i", yaxs = "i")
#abline(v=seq(pi/4,2*pi,by=pi/4), col = gray(.7))
#rads <- seq(0,pi/4,length=100)
#lines(rads,.5/cos(rads))
#
#rads <- seq(pi/4,3*pi/4,length=100)
#lines(rads,.5/sin(rads))
#
#rads <- seq(3*pi/4,5*pi/4,length=100)
#lines(rads,-.5/cos(rads))
#
#rads <- seq(5*pi/4,7*pi/4,length=100)
#lines(rads,-.5/sin(rads))
#
#rads <- seq(7*pi/4,2*pi,length=100)
#lines(rads,.5/cos(rads))
#
#abline(h=radius)

COD <- local(get(load("/data/commons/triffe/git/YearsLost/YearsLost/Data/COD.Rdata")))
MatF <- COD[["USA"]][["Dxfc"]] / rowSums(COD[["USA"]][["Dxfc"]])







# code for piecing out Tim's plots'

rands    <- sort(runif(np),decreasing=TRUE)
p        <- cumsum(rands)/sum(cumsum(rands))


get_phis <- function(p,n=5000){
  np <- length(p)
# code for piecing out Tim's plots'
  r0        <- (p[1]/pi)^0.5 ##CORRECTED!
  prem      <- p[2:8]
  
  phi0      <- 0.25*pi/n*c(0:(n-1))
  y         <- 0.5*tan(phi0) #'y' piece of triangle for a given angle
  phi_skip  <- 0.25*pi/n*c(1:n)
  y_skip    <- 0.5*tan(phi_skip)
  y_rev     <- rev(y_skip)
  y_m       <- c(y,y_rev)
  y_all     <- c(y_m,y_m,y_m,y_m)
  dPhi      <- 2*pi/n/8  #infenitesimal of the angle
#phi       <- dPhi*c(0:((n*8)-1))
  phi       <- cumsum(c(0,rep(dPhi,n*8)))
  R         <- (0.5^2 + y_all^2)^0.5  #Radius from center to edge of square
  dA        <- 0.5*(R^2-r0^2)*dPhi #the infinesimal of the area
  
  phi_set   <- phi[1]  #Initiate the set of angles
  jset      <- 1
  j0        <- 1  #The first angle will be angle=0 radians (phi[1])
  
  for (i in c(1:(np-1))) {
    # Our indexing starts from the last angle computed (j0)
    # Figure out the cumulative sum of all the area elements
    # from that angle to 2pi
    a       <- cumsum(dA[j0:((n*8)+1)])
    
    # pad the front end (angles less than phi[j0]) of the area vector with zeros
    atot    <- c(rep(0,(j0-1)),a)
    
    #find the index, j, where the cumulative area matches the proportion, p[i], at hand
    j       <- which.min(abs(atot-prem[i]))
    
    #add the angle, phi[j], to the master set
    phi_set <- c(phi_set,phi[j])
    jset    <- c(jset,j)
    
    #the next iteration will compute angles from phi[j]
    j0 <- j+1
  }
#phi_set <- c(phi_set,2*pi)
  list(phi_set=phi_set,r0=r0)
}

get_phis(p)




addphis <- c(1, 3, 5, 7) * pi/4
myphis  <- sort(c(phi_set, addphis))
corneri <- myphis %in% addphis

hyp <- ifelse(myphis >= 0 & myphis < pi/4, .5/cos(myphis),
  ifelse(myphis >= pi/4 & myphis < 3*pi/4, .5/sin(myphis),
    ifelse(myphis >= 3*pi/4 & myphis < 5*pi/4, -.5/cos(myphis),
      ifelse(myphis >= 5*pi/4 & myphis < 7*pi/4, -.5/sin(myphis),.5/cos(myphis)))))
xs <- hyp * cos(myphis) * .5
ys <- hyp * sin(myphis) * .5

cols <- RColorBrewer::brewer.pal(8,"Accent")
plot(NULL, xlim = c(-.5, .5), ylim = c(-.5, .5), asp=1)
for (i in 1:(np-1)){
  inds <- which(myphis==phi_set[i])[1]:which(myphis==phi_set[i+1])[1]
  polygon(c(0,xs[inds],0),c(0,ys[inds],0),border=NA,col=cols[i+1])
}

polygon(r0*cos(seq(0,2*pi,length=300)),r0*sin(seq(0,2*pi,length=300)),border=NA,col=cols[1])


