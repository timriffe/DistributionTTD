
# this script uses the HMDresults object to search for common patterns to the various defined measures.
if (system("hostname",intern=TRUE)=="triffe-N80Vm"){
  # if I'm on the laptop
  setwd("/home/tim/git/DistributionTTD/DistributionTTD")
} else {
  # in that case I'm on Berkeley system, and other people in the dept can run this too
  setwd(paste0("/hdir/0/",system("whoami",intern=TRUE),"/git/DistributionTTD/DistributionTTD"))
}
library(data.table)
library(reshape2)
HMD <- local(get(load("Data/HMDresults.Rdata")))


Vars <- c("Lskew","LCV","Lmad","Sskew","Skurt","q25","q50","q75","Mode","CV","Var","ex","ex2")

#Fdx <- acast(HMD[HMD$Sex == "f", ], Age~Year+CNTRY, value.var = "dx")
#Fex <- acast(HMD[HMD$Sex == "f", ], Age~Year+CNTRY, value.var = "ex")
#matplot(0:110, Fdx, type = 'l', lty = 1, col = "#00000005")
#matplot(0:110, Fex, type = 'l', lty = 1, col = "#00000005")
Fmatrices[[1]]
Fmatrices <- lapply(Vars, function(v, dat){
            acast(dat[dat$Sex == "f", ], Age~Year+CNTRY, value.var = v)
        },dat=HMD)
Mmatrices <- lapply(Vars, function(v, dat){
            acast(dat[dat$Sex == "m", ], Age~Year+CNTRY, value.var = v)
        },dat=HMD)

#matplot(0:100, Fmatrices[[1]][1:101,], type = 'l', lty = 1, col = "#00000005",main = "Females, Lskew(x)")
#matplot(0:100, Fmatrices[[2]][1:101,], type = 'l', lty = 1, col = "#00000005",main = "Females, LCV(x)")
#matplot(0:100, Fmatrices[[3]][1:101,], type = 'l', lty = 1, col = "#00000005",main = "Females, Lmad(x)")
#matplot(0:100, Fmatrices[[4]][1:101,], type = 'l', lty = 1, col = "#00000005",main = "Females, Sskew(x)")
#matplot(0:100, Fmatrices[[5]][1:101,], type = 'l', lty = 1, col = "#00000005",main = "Females, Skurt(x)")
#matplot(0:100, Fmatrices[[6]][1:101,], type = 'l', lty = 1, col = "#00000005",main = "Females, q25(x)")
#matplot(0:100, Fmatrices[[7]][1:101,], type = 'l', lty = 1, col = "#00000005",main = "Females, q50(x)")
#matplot(0:100, Fmatrices[[8]][1:101,], type = 'l', lty = 1, col = "#00000005",main = "Females, q75(x)")
#matplot(0:100, Fmatrices[[9]][1:101,], type = 'l', lty = 1, col = "#00000005",main = "Females, Mode(x)") # Mode Boring!


# Just compare SWE in 1900, 1950, 2000
ind <- with(HMD, CNTRY == "SWE" & Year %in% c(1900,1950,2000) & Sex == "f")

FSWE <- lapply(Vars, function(v, dat){
            acast(dat[dat$Sex == "f", ], Age~Year, value.var = v)
        },dat=HMD[ind,])
names(FSWE) <- Vars
plotVariable <- function(XXX, Var){
    matplot(0:100, XXX[[Var]][1:101,], type = 'l', lty = 1,
            main = paste0("Females, ",Var,"(x)"), col = gray(c(.2,.4,.6)),
            lwd=c(1,2,3), xlab = "Age", ylab = Var)
    vals        <- rowSums(XXX[[Var]][1:101,], na.rm = TRUE)
    leftval     <- sum(vals[1:5],na.rm=TRUE)
    rightval    <- sum(vals[96:101],na.rm=TRUE)
    leftOrRight <- ifelse(leftval > rightval,"bottomleft","bottomright")
    legend(leftOrRight,lty=1,col=gray(c(.2,.4,.6)),lwd=c(1,2,3),legend=c(1900,1950,2000), bty = "n") 
}
#plotVariable(FSWE,"q50")
plotVariable(FSWE,"ex2")
par(mfrow=c(1,2))
plotVariable(FSWE,"LCV")
plotVariable(FSWE,"CV")
graphics.off()
#

#matplot(0:100, FSWE[["q50"]][1:101,]+0:100, type = 'l', lty = 1,
#        main = paste0("Females, q50(x)"), col = gray(c(.2,.4,.6)),
#        lwd=c(1,2,3), xlab = "Age", ylab = "q50")
#legend("bottomright",lty=1,col=gray(c(.2,.4,.6)),lwd=c(1,2,3),legend=c(1900,1950,2000), bty = "n") 
#


pdf("Figures/SwedenFemalesIQR1900_2000.pdf", width = 4, height = 4)
cols <- RColorBrewer::brewer.pal(4,"Paired")
par(mai=c(.6,.6,.2,.2), xaxs = "i", yaxs = "i",xpd = TRUE)
plot(0:100, FSWE[["q50"]][1:101,3]+0:100, type = 'n', lty = 1,
        main = paste0(""),
        lwd=c(1,2,3), xlab = "", ylab = "",ylim=c(25,105),
        axes = FALSE,
        panel.first = list(
                rect(0,25,100,105,border = NA,col=gray(.95)),
                segments(0,seq(30,100,by=10),100,seq(30,100,by=10),col = "white",lwd=.5),
                segments(seq(10,90,by=10),25,seq(10,90,by=10),105,col="white",lwd=.5),
                text(0,seq(30,100,by=10),seq(30,100,by=10),cex=.7,pos=2),
                text(seq(0,100,by=10),25,seq(0,100,by=10),cex=.7,pos=1)))
polygon(c(0:100,100:0),c(FSWE[["q75"]][1:101,3]+0:100,rev(FSWE[["q25"]][1:101,3]+0:100)),
        border = cols[2], col = paste0(cols[1],30))
polygon(c(0:100,100:0),c(FSWE[["q75"]][1:101,1]+0:100,rev(FSWE[["q25"]][1:101,1]+0:100)),
        border = cols[4], col = paste0(cols[3],30))
text(30,c(62,74,82),c(".25","median",".75"),col=cols[4],cex=.7)
text(40,c(78.5,86,92),c(".25","median",".75"),col=cols[2],cex=.7)
lines(0:100, FSWE[["q50"]][1:101,3]+0:100,col=cols[2])
lines(0:100, FSWE[["q50"]][1:101,1]+0:100,col=cols[4])
text(20,c(87,69),c("2000","1900"),col=cols[c(2,4)])
text(50,15, "a")
text(-10,65, "a + y",srt=90)
#rect(0,25,100,105,border = "white")
dev.off()

pdf("Figures/SwedenFemalesVar1900_2000.pdf", width = 4, height = 4)
cols <- RColorBrewer::brewer.pal(4,"Paired")
par(mai=c(.6,.6,.2,.2), xaxs = "i", yaxs = "i",xpd = TRUE)
plot(0:100, FSWE[["Var"]][1:101,3]+0:100, type = 'n', lty = 1,
  main = paste0(""),
  lwd=c(1,2,3), xlab = "", ylab = "",ylim=c(25,105),
  axes = FALSE,
  panel.first = list(
    rect(0,25,100,105,border = NA,col=gray(.95)),
    segments(0,seq(30,100,by=10),100,seq(30,100,by=10),col = "white",lwd=.5),
    segments(seq(10,90,by=10),25,seq(10,90,by=10),105,col="white",lwd=.5),
    text(0,seq(30,100,by=10),seq(30,100,by=10),cex=.7,pos=2),
    text(seq(0,100,by=10),25,seq(0,100,by=10),cex=.7,pos=1)))

polygon(c(0:100,100:0),c(0:100+FSWE[["ex"]][1:101,3]+sqrt(FSWE[["Var"]][1:101,3]),
    rev(0:100+FSWE[["ex"]][1:101,3]-sqrt(FSWE[["Var"]][1:101,3]))),
  border = cols[2], col = paste0(cols[1],30))
polygon(c(0:100,100:0),c(0:100+FSWE[["ex"]][1:101,1]+sqrt(FSWE[["Var"]][1:101,1]),
    rev(0:100+FSWE[["ex"]][1:101,1]-sqrt(FSWE[["Var"]][1:101,1]))),
  border = cols[4], col = paste0(cols[3],30))
lines(0:100, FSWE[["ex"]][1:101,3]+0:100,col=cols[2])
lines(0:100, FSWE[["ex"]][1:101,1]+0:100,col=cols[4])
text(30,86,expression(a+e(a)+sigma("y|a")),col=cols[4],cex=.7,pos=4)
text(30,68,expression(a+e(a)),col=cols[4],cex=.7,pos=4)
text(30,50,expression(a+e(a)-sigma("y|a")),col=cols[4],cex=.7,pos=4)

text(10,96,expression(a+e(a)+sigma("y|a")),col=cols[2],cex=.7,pos=4)
text(10,80,expression(a+e(a)),col=cols[2],cex=.7,pos=4)
text(10,72,expression(a+e(a)-sigma("y|a")),col=cols[2],cex=.7,pos=4)


text(20,c(89,60),c("2000","1900"),col=cols[c(2,4)])
text(50,15, "a")
text(-10,65, "a + y",srt=90)
dev.off()


# Figure for Sweden Skewness
pdf("Figures/SwedenFemalesSkew1900_2000.pdf", width = 4, height = 4)
cols <- RColorBrewer::brewer.pal(6,"Paired")
par(mai=c(.6,.6,.2,.2), xaxs = "i", yaxs = "i",xpd = TRUE)
plot(0:105, FSWE[["Sskew"]][1:106,3], type = 'n', lty = 1,
  main = paste0(""),
  lwd=c(1,2,3), 
  xlab = "", 
  ylab = "",
  ylim=c(-2,1.7),
  axes = FALSE,
  panel.first = list(
    rect(0,-2,105,1.7,border = NA,col=gray(.95)),
    segments(0,seq(-2,1.5,by=.5),105,seq(-2,1.5,by=.5),col = "white",lwd=.5),
    segments(seq(10,90,by=10),-2,seq(10,90,by=10),1.7,col="white",lwd=.5),
    text(0,seq(-2,1.5,by=.5),seq(-2,1.5,by=.5),cex=.7,pos=2),
    text(seq(0,100,by=10),-2,seq(0,100,by=10),cex=.7,pos=1)))
lines(0:105, FSWE[["Sskew"]][1:106,3], col = cols[2])
lines(0:105, FSWE[["Sskew"]][1:106,1], col = cols[4])
lines(0:105, FSWE[["Sskew"]][1:106,2], col = cols[6])
text(50,-2.4, "a")
text(-13,0, "Skewness",srt=90)
text(20,-1.4,"2000",col=cols[2],srt=30,cex=.8)
text(20,-1.1,"1950",col=cols[6],srt=30,cex=.8)
text(20,-.8,"1900",col=cols[4],srt=10,cex=.8)

segments(50,0,80,0)
text(50,0,"Tails of f(y|a) balanced",pos=2,cex=.8)
text(40,-.5,"Left skew",pos=2,cex=.8)
text(70,.5,"Right skew",pos=2,cex=.8)
dev.off()

# Figure for Sweden Kurtosis
pdf("Figures/SwedenFemalesKurt1900_2000.pdf", width = 4, height = 4)
cols <- RColorBrewer::brewer.pal(6,"Paired")
par(mai=c(.6,.6,.2,.2), xaxs = "i", yaxs = "i",xpd = TRUE)
plot(0:105, FSWE[["Skurt"]][1:106,3], type = 'n', lty = 1,
  main = paste0(""),
  lwd=c(1,2,3), 
  xlab = "", 
  ylab = "",
  ylim=c(1,10),
  axes = FALSE,
  panel.first = list(
    rect(0,1,105,10,border = NA,col=gray(.95)),
    segments(0,seq(1,10,by=1),105,seq(1,10,by=1),col = "white",lwd=.5),
    segments(seq(10,90,by=10),1,seq(10,90,by=10),10,col="white",lwd=.5),
    text(0,seq(1,10,by=1),seq(1,10,by=1),cex=.7,pos=2),
    text(seq(0,100,by=10),1,seq(0,100,by=10),cex=.7,pos=1)))
lines(0:105, FSWE[["Skurt"]][1:106,3], col = cols[2])
lines(0:105, FSWE[["Skurt"]][1:106,1], col = cols[4])
lines(0:105, FSWE[["Skurt"]][1:106,2], col = cols[6])
text(50,0, "a")
text(-10,6, "Kurtosis",srt=90)
text(40,4.3,"2000",col=cols[2],srt=-35,cex=.8)
text(40,3.6,"1950",col=cols[6],srt=-35,cex=.8)
text(30,3,"1900",col=cols[4],cex=.8)

text(0,8,"Variance driven by very\npremature deaths",pos=4,cex=.8)
text(105,6,"Variance driven by\nvery long survivors",pos=2,cex=.8)
dev.off()
# some kind of linear relationship between LCV and CV
# from age 0 until 85 or so. Not sure if it's constant.
# late age curl in LCV could be due to radix, but haven't tested it.
# age 0 difference due to ax use in CV, but midpoint in LCV
plot(FSWE[["CV"]][1:101,2], FSWE[["LCV"]][1:101,2], type = 'l')
lm(FSWE[["LCV"]][2:86,3] ~ FSWE[["CV"]][2:86,3])

range(FSWE[["Skurt"]][1:106,],na.rm=TRUE)
names(FSWE)

# Figure for Sweden Kurtosis
pdf("Figures/SwedenFemalesCV1900_2000.pdf", width = 4, height = 4)
cols <- RColorBrewer::brewer.pal(6,"Paired")
par(mai=c(.6,.6,.2,.2), xaxs = "i", yaxs = "i",xpd = TRUE)
plot(0:105, FSWE[["CV"]][1:106,3], type = 'n', lty = 1,
  main = paste0(""),
  lwd=c(1,2,3), 
  xlab = "", 
  ylab = "",
  ylim=c(0,1),
  axes = FALSE,
  panel.first = list(
    rect(0,0,105,1,border = NA,col=gray(.95)),
    segments(0,seq(0,1,by=.2),105,seq(0,1,by=.2),col = "white",lwd=.5),
    segments(seq(10,90,by=10),0,seq(10,90,by=10),1,col="white",lwd=.5),
    text(0,seq(0,1,by=.2),seq(0,1,by=.2),cex=.7,pos=2),
    text(seq(0,100,by=10),0,seq(0,100,by=10),cex=.7,pos=1)))
lines(0:105, FSWE[["CV"]][1:106,3], col = cols[2])
lines(0:105, FSWE[["CV"]][1:106,1], col = cols[4])
lines(0:105, FSWE[["CV"]][1:106,2], col = cols[6])
text(50,0, "a")
text(-10,6, "Kurtosis",srt=90)
text(40,4.3,"2000",col=cols[2],srt=-35,cex=.8)
text(40,3.6,"1950",col=cols[6],srt=-35,cex=.8)
text(30,3,"1900",col=cols[4],cex=.8)

#text(0,8,"Variance driven by very\npremature deaths",pos=4,cex=.8)
#text(105,6,"Variance driven by\nvery long survivors",pos=2,cex=.8)
dev.off()

colnames(HMD)
# surfaces?
Sskew <- acast(HMD[HMD$CNTRY == "SWE" & Sex == "f", ],Age~Year, value.var = "Sskew")
Var   <- acast(HMD[HMD$CNTRY == "SWE" & Sex == "f", ],Age~Year, value.var = "Var")
Kurt  <- acast(HMD[HMD$CNTRY == "SWE" & Sex == "f", ],Age~Year, value.var = "Skurt")
ex  <- acast(HMD[HMD$CNTRY == "SWE" & Sex == "f", ],Age~Year, value.var = "ex")
library(LexisUtils)
library(RColorBrewer)
display.brewer.all()
colramp <- colorRampPalette(brewer.pal(9, "BuGn"), space = "Lab")


pdf("Figures/Surf/exSWEf.pdf",height=5,width=11)
par(mai=c(.5,.5,.2,1))
LexisMap(ex,log=FALSE,contour=TRUE,colramp=colramp,nbreaks=11,LexRef=FALSE)
dev.off()

pdf("Figures/Surf/SDSWEf.pdf",height=5,width=11)
par(mai=c(.5,.5,.2,1))
LexisMap(sqrt(Var),log=FALSE,contour=TRUE,colramp=colramp,nbreaks=15,LexRef=FALSE)
dev.off()

pdf("Figures/Surf/SskewSWEf.pdf",height=5,width=11)
par(mai=c(.5,.5,.2,1))
LexisMap(Sskew,log=FALSE,contour=TRUE,colramp=colramp,nbreaks=11,LexRef=FALSE)
dev.off()

pdf("Figures/Surf/KurtSWEf.pdf",height=5,width=11)
par(mai=c(.5,.5,.2,1))
LexisMap(Kurt,log=FALSE,contour=TRUE,colramp=colramp,nbreaks=13,LexRef=FALSE)
dev.off()
