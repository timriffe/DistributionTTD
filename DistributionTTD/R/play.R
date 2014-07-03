
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
