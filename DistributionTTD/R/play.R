# code not used in paper


JPN <- local(get(load("~/HMDWORK/JPN/Rbin/bltper_1x1.Rdata")))
SWE <- local(get(load("~/HMDWORK/SWE/Rbin/bltper_1x1.Rdata")))

ex <- JPN$ex[JPN$Year == 2000]

x <- 0:110 + JPN$ax[JPN$Year == 2000]
fx <- JPN$dx[JPN$Year == 2000]

fx <- fx / sum(fx)
plot(fx)

# life expectancy lost due to death
edagger <- sum(fx*ex)
a <- 0:110

# variance in age at death
sigsq <- sum((a-ex[1])^2 * fx)
sum(ex^2 * fx)

ea <- (ex + c(ex[2:length(ex)],0))/2
sum((x-ex[1])^2 * fx)
sum(ea^2 * fx)

lx <- JPN$lx[JPN$Year == 2000]
lx <- lx/lx[1]

# Goldstein 2009:
sum(ex*lx)
sum(x*lx)
sum(ea^2 * fx)
2 * sum(a*lx)-ex[1]^2

# OK
mux <- JPN$mx[JPN$Year == 2000]
plot(diff(ex))
lines(ex * mux - 1)

#
Lx <- JPN$Lx[JPN$Year == 2000] / 1e5
sum(ea^2 * fx)
ex[1] ^ 2 + 2 * sum(ea * c(diff(ex),0) *Lx)

# OK
2 * sum(x*Lx)
sum(x^2*fx)

#
sum(ea^2*fx)
sum(x^2*fx) - ex[1]^2

# ergo:
# variance in age at death:
sum((a-ex[1])^2 * fx)
# average squared remaining life expectancy at death
sum(ea^2*fx)


# now for the more complex one:
sum(outer(ex,ex,"-")^2*outer(fx,fx,"*"))/2
sum((a-ex[1])^2 * fx) - sum(fx*ex)^2 # sigsq - edaggersq
# or
sum((x-ex[1])^2 * fx) - sum(fx*ex)^2

sum(outer(a,a,"-")^2*outer(fx,fx,"*"))/2

#
sum(fx*ex)^2
sum(outer(a,a,"-")^2*outer(fx,fx,"*"))/2-sum(outer(ea,ea,"-")^2*outer(fx,fx,"*"))/2


# ---------------------------------------------------------------
# further playing:
x     <- 0:110

vx <- getvx(lx,ex)
vx[1]
plot(x,ex,type= 'l',ylim=c(0,100))
polygon(c(x,rev(x)),c(ex-2*sqrt(vx),rev(ex+2*sqrt(vx))),col="#55555550")
polygon(c(x,rev(x)),c(ex-sqrt(vx),rev(ex+sqrt(vx))),col="#55555550")

plot(x[-length(x)],diff(sqrt(vx)))
abline(h=0)

library(data.table)
JPN <- data.table(JPN)
JPN[,vx := getvx(lx,ex),by=c("Year")]
JPN <- as.data.frame(JPN)

library(reshape2)
library(DemogBerkeley)
JPN$Age <- age2int(JPN$Age )
JPN$sigx <- sqrt(JPN$vx)
SX <- acast(JPN, Age~Year, value.var = "sigx")

library(LexisUtils)
LexisMap(SX, log= FALSE)


SWE <- data.table(SWE)
SWE[,vx := getvx(lx,ex),by=c("Year")]
SWE <- as.data.frame(SWE)
SWE$Age <- age2int(SWE$Age )
SWE$sigx <- sqrt(SWE$vx)
SX <- acast(SWE, Age~Year, value.var = "sigx")
LexisMap(SX, log= FALSE)
mx <- acast(SWE, Age~Year, value.var = "mx")
dev.new()
LexisMap(mx)

# ---------------------------------------------
HMD <- local(get(load("/home/tim/git/DistributionTTD/DistributionTTD/Data/HMDltper.Rdata")))

lx <- HMD$lx[HMD$CNTRY == "SWE" & HMD$Year == 2000 & HMD$Sex == "f"]/1e5

myspline <- function(lx,y){
	lxy <- lx/lx[1]
	yy <- 1:length(lxy)-1
	splinefun(lxy~yy,method="monoH.FC")(y)
}

maxA <- function(y,lx){
	myspline(lx=lx,y=y)*y
}

#xA <- optimize(maxA,c(0,110),lx=lx,maximum=TRUE)$max

getxA <- function(lx){
	optimize(maxA,c(0,length(lx)-1),lx=lx,maximum=TRUE)$max
}


plot(0:110,lx,type='l')
abline(v=getxA(lx))
abline(h=myspline(lx,getxA(lx)))

getA <- function(lx){
	xA <- getxA(lx)
	lxA <- myspline(lx,xA)
	A <- xA * lxA
	A / sum(lx)
}
getA(lx)

lines(0:110, 100*c(-diff(c(-diff(c(lx,0)),0))))


plot(0:110,lx*0:110,type='l',main="yup, looks like there's an age pattern")
abline(v=getxA(lx))
sapply(0:110,function(x,lx){
			lxx <- lx[(x+1):length(lx)]
			lines(x:(110),lxx* (0:(110-x)))
		},lx=lx)


lx
plot(0:100,sapply(1:101,function(xx,lx){
			lxx <- lx[(xx+1):length(lx)]
			getxA(lxx)
		},lx=lx),type='l',asp=1,ylab="xA(x)")
abline(h=0)
TEST <- sapply(1:101,function(xx,lx){
			lxx <- lx[(xx+1):length(lx)]
			getxA(lxx)
		},lx=lx)
getxAP <- function(lx){
	if (any(lx==0)){
		lx <- c(lx[lx>0],0)
	}
	xAy <- sapply(1:(length(lx)-2),function(xx,lx){
				lxx <- lx[(xx+1):length(lx)]
				getxA(lxx)
			},lx=lx)
	heights <- myspline(lx,xAy+(1:length(xAy)-1)) / lx[1:length(xAy)]
	
	c(xAy * heights,rep(0,111-length(xAy)))
}


library(reshape2)
LX <- acast(HMD[HMD$CNTRY == "SWE" & HMD$Sex == "f", ], Age~Year, value.var = "lx")/1e5
EX <- acast(HMD[HMD$CNTRY == "SWE" & HMD$Sex == "f", ], Age~Year, value.var = "ex")
# stalls
library(parallel)
AXP <- do.call(cbind,mclapply(as.data.frame(LX),getxAP,mc.cores=4))
dimnames(AXP) <- dimnames(LX)

library(LexisUtils)
prop <- AXP/EX
prop[prop > 1] <- 1
LexisMap(prop[1:95,],log=FALSE)
head(AXY)
range(AXY/EX)
plot(LX[,"1900"])


######################################################3
# entropy?
# min, when everyone dies at same age:
# max when uniform?

# edagger is null when all die at same time
# max when death are uniform.
dx <- -diff(c(lx,0))
ex <-rev(cumsum(rev(lx))) / lx 
plot(dx * ex)
sum(dx*ex)



