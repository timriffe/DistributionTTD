# defunct. Median remaining years of life can be calculated from getQuantile() in package

# I think the age at which RLE = MRYL is the cut-off between an early and late death:


# MRYL added in data prep!, already column
#getMedianRYL <- function(lx){
#    # distribution
#    dx   <- c(-diff(lx),0)
#    # repeated in a matrix
#    DX   <- replicate(length(dx),dx)
#    # zero out earlier ages as we move up in age
#    DX[upper.tri(DX)] <- 0
#    # standardize for each age (column) to sum to 1
#    EDX  <- t(t(DX) / colSums(DX))
#    # the cumulative sum give the proportion, we want to know when it hits .5
#    EDXC <- apply(EDX,2,cumsum)
#    # approximates an exact median remaing lifespan for each age
#    MRYL <- apply(EDXC,2,function(llx){
#                if (all(is.na(llx))){
#                    return(NA)
#                }
#                splinefun(0:110~llx)(.5)
#            }) - 0:110
#    # in old ages you can get junk...
#    MRYL[is.na(MRYL) | MRYL < 0] <- 0
#    # return the goods to compare with ex.
#    MRYL
#}
# already done! just load the file!
#Data[,MRYL := getMedianRYL(lx),by=list(Code,Year,Sex)]
#save(Data,file = "/home/triffe/git/PAA2014poster/PAA2014Poster/Data/HMDAll.Rdata")

Data <- local(get(load("/home/triffe/git/PAA2014poster/PAA2014Poster/Data/HMDAll.Rdata")))

Data$Gap <- Data$MRYL - Data$ex

ex <- with(Data,ex[Code == "USA" & Year == 2010 & Sex == "m"])
lx <- with(Data,lx[Code == "USA" & Year == 2010 & Sex == "m"])
dx <- c(-diff(lx),0)


minfun <- function(MagicAge, ex, MRYL){
    (splinefun(ex ~ 0:110)(MagicAge) - splinefun(MRYL ~ 0:110)(MagicAge))^2
}
Cross <- function(ex, MRYL){
    optimize(minfun,c(0,110),ex=ex,MRYL=MRYL)$minimum
}
Cross(Data$ex[1:111],Data$MRYL[1:111])

CrossOver <- Data[, 
        Cross(ex,MRYL), 
        by = list(Code,Year,Sex)]
head(CrossOver)
library(reshape2)
FemCross <- acast(CrossOver[CrossOver$Sex == "f", ], Year ~ Code, value.var = "V1")
MalCross <- acast(CrossOver[CrossOver$Sex == "m", ], Year ~ Code, value.var = "V1")

par(mfrow=c(1,2))
matplot(1751:2011, FemCross, type = 'l', ylim = c(30,70), xlim = c(1970,2011), lty = 1, col = "#00000050")
lines(1751:2011, FemCross[,"JPN"],col = "red")
lines(1751:2011, FemCross[,"USA"],col = "blue")
matplot(1751:2011, MalCross, type = 'l', ylim = c(30,70), xlim = c(1970,2011), lty = 1, col = "#00000050")
lines(1751:2011, MalCross[,"JPN"],col = "red")
lines(1751:2011, MalCross[,"USA"],col = "blue")

colnames(MalCross)[order(MalCross["2009", ])]

dx <- with(Data,dx[Code == "USA" & Sex == "m" & Year == 2010])
dx <- dx / sum(dx)
MRYL <- with(Data,MRYL[Code == "USA" & Sex == "m" & Year == 2010])

dx[51:111]
MRYL[51]
ex[51]
# 63% of those that survive to 50 will outlive the remining life expectancy at 50...
sum(dx[(51+27):111]) / (sum(dx[(51+27):111]) + sum(dx[51:(51+26)]))

MRYLm <- with(Data, MRYL[Code == "USA" & Sex == "m" & Year == 2010])
exm   <- with(Data, ex[Code == "USA" & Sex == "m" & Year == 2010])
lxm   <- with(Data, lx[Code == "USA" & Sex == "m" & Year == 2010])
MRYLf <- with(Data, MRYL[Code == "USA" & Sex == "f" & Year == 2010])
exf   <- with(Data, ex[Code == "USA" & Sex == "f" & Year == 2010])
lxf   <- with(Data, lx[Code == "USA" & Sex == "f" & Year == 2010])


age <- 0:110
graphics.off()
plot(age, MRYLm - exm, type = 'l', col = "blue", ylab = "Gap (years)", xlab = "Age",
        main = "Median RYL - Mean RYL")
lines(age, MRYLf - exf, col = "red")
abline(h=0)
segments(FemCross["2010","USA"],0,FemCross["2010","USA"]+3,.25)
segments(MalCross["2010","USA"],0,MalCross["2010","USA"]-3,-.25)
points(FemCross["2010","USA"],0,col = "red", pch = 19)
points(MalCross["2010","USA"],0, col = "blue", pch = 19)
text(FemCross["2010","USA"]+2,.25,round(FemCross["2010","USA"],2),pos=4)
text(MalCross["2010","USA"]-2,-.25,round(MalCross["2010","USA"],2),pos=2)

# ------------------------------------------
# new plot: % that outlive ex by age:
# ------------------------------------------


percentoutlivex <- function(lx,ex){
     Age <- 1:length(lx) - 1
     approx(Age, lx,  Age + ex)$y / lx
}


POm <- percentoutlivex(lxm,exm)
POf <- percentoutlivex(lxf,exf)

Mcross <- approx(POm,0:110,.5)$y
Fcross <- approx(POf,0:110,.5)$y

approx(POm,0:110,.5)
# why is this crossover different than the age where MRYL = ex ?
ylim <- c(.35, .65)
xlim <- c(0, 110)
graphics.off()
pdf("/home/triffe/git/PAA2014poster/PAA2014Poster/Figures/percentOutliveexUS2010.pdf",height=8,width=12)
par(mai = c(1, 1, 1, 1), xaxs = "i", yaxs = "i")
plot(0:110, POm, type = 'l',col = "blue", lwd = 2, xlim = xlim, ylim = ylim, 
        ylab = "Percent outliving expectancy", xlab = "Age", axes = FALSE, cex.lab = 1.8,
        panel.first = list(
                rect(xlim[1],ylim[1],xlim[2],ylim[2], border = NA, col = gray(.95)),
                segments(xlim[1],seq(ylim[1],ylim[2],by=.025),xlim[2],seq(ylim[1],ylim[2],by=.025),col="white",lwd=.5),
                segments(xlim[1],seq(ylim[1],ylim[2],by=.05),xlim[2],seq(ylim[1],ylim[2],by=.05),col="white"),
                segments(seq(xlim[1],xlim[2],by=10),ylim[1],seq(xlim[1],xlim[2],by=10),ylim[2],col="white"),
                text(xlim[1],seq(ylim[1],ylim[2],by=.05),paste0(100*seq(ylim[1],ylim[2],by=.05),"%"),cex=1.8,pos=2,xpd=TRUE),
                text(seq(xlim[1],xlim[2],by=10),ylim[1],seq(xlim[1],xlim[2],by=10),cex=1.8,pos=1,xpd=TRUE)
))
lines(0:110, POf, col = "red", lwd = 2)
abline(h = .5)
points(Mcross, .5, col = "blue", pch = 19)
points(Fcross, .5, col = "red", pch = 19)
text(Mcross, .49, paste("Male crossover, age",round(Mcross,1)), pch = 19,pos=2,cex=1.8)
text(Fcross, .51, paste("Female crossover, age",round(Fcross,1)),pch = 19,pos=4,cex=1.8)
dev.off()
