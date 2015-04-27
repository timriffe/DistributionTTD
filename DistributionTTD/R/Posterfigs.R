
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


Vars <- c("Lskew","LCV","Lmad","Sskew","Skurt","q25","q50","q75","Mode","CV","Var","ex","ex2")

#Fdx <- acast(HMD[HMD$Sex == "f", ], Age~Year+CNTRY, value.var = "dx")
#Fex <- acast(HMD[HMD$Sex == "f", ], Age~Year+CNTRY, value.var = "ex")
#matplot(0:110, Fdx, type = 'l', lty = 1, col = "#00000005")
#matplot(0:110, Fex, type = 'l', lty = 1, col = "#00000005")

#Fmatrices <- lapply(Vars, function(v, dat){
#            acast(dat[dat$Sex == "f", ], Age~Year+CNTRY, value.var = v)
#        },dat=HMD)
#Mmatrices <- lapply(Vars, function(v, dat){
#            acast(dat[dat$Sex == "m", ], Age~Year+CNTRY, value.var = v)
#        },dat=HMD)

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
#plotVariable <- function(XXX, Var){
#    matplot(0:100, XXX[[Var]][1:101,], type = 'l', lty = 1,
#            main = paste0("Females, ",Var,"(x)"), col = gray(c(.2,.4,.6)),
#            lwd=c(1,2,3), xlab = "Age", ylab = Var)
#    vals        <- rowSums(XXX[[Var]][1:101,], na.rm = TRUE)
#    leftval     <- sum(vals[1:5],na.rm=TRUE)
#    rightval    <- sum(vals[96:101],na.rm=TRUE)
#    leftOrRight <- ifelse(leftval > rightval,"bottomleft","bottomright")
#    legend(leftOrRight,lty=1,col=gray(c(.2,.4,.6)),lwd=c(1,2,3),legend=c(1900,1950,2000), bty = "n") 
#}
##plotVariable(FSWE,"q50")
#plotVariable(FSWE,"ex2")
#par(mfrow=c(1,2))
#plotVariable(FSWE,"LCV")
#plotVariable(FSWE,"CV")
#graphics.off()
#

#matplot(0:100, FSWE[["q50"]][1:101,]+0:100, type = 'l', lty = 1,
#        main = paste0("Females, q50(x)"), col = gray(c(.2,.4,.6)),
#        lwd=c(1,2,3), xlab = "Age", ylab = "q50")
#legend("bottomright",lty=1,col=gray(c(.2,.4,.6)),lwd=c(1,2,3),legend=c(1900,1950,2000), bty = "n") 
#



#pdf("Figures/SwedenFemalesVar1900_2000.pdf", width = 4, height = 4)
#cols <- RColorBrewer::brewer.pal(4,"Paired")
#par(mai=c(.6,.6,.2,.2), xaxs = "i", yaxs = "i",xpd = TRUE)
#plot(0:100, FSWE[["Var"]][1:101,3]+0:100, type = 'n', lty = 1,
#		main = paste0(""),
#		lwd=c(1,2,3), xlab = "", ylab = "",ylim=c(25,105),
#		axes = FALSE,
#		panel.first = list(
#				rect(0,25,100,105,border = NA,col=gray(.95)),
#				segments(0,seq(30,100,by=10),100,seq(30,100,by=10),col = "white",lwd=.5),
#				segments(seq(10,90,by=10),25,seq(10,90,by=10),105,col="white",lwd=.5),
#				text(0,seq(30,100,by=10),seq(30,100,by=10),cex=.7,pos=2),
#				text(seq(0,100,by=10),25,seq(0,100,by=10),cex=.7,pos=1)))
#
#polygon(c(0:100,100:0),c(0:100+FSWE[["ex"]][1:101,3]+sqrt(FSWE[["Var"]][1:101,3]),
#				rev(0:100+FSWE[["ex"]][1:101,3]-sqrt(FSWE[["Var"]][1:101,3]))),
#		border = cols[2], col = paste0(cols[1],30))
#polygon(c(0:100,100:0),c(0:100+FSWE[["ex"]][1:101,1]+sqrt(FSWE[["Var"]][1:101,1]),
#				rev(0:100+FSWE[["ex"]][1:101,1]-sqrt(FSWE[["Var"]][1:101,1]))),
#		border = cols[4], col = paste0(cols[3],30))
#lines(0:100, FSWE[["ex"]][1:101,3]+0:100,col=cols[2])
#lines(0:100, FSWE[["ex"]][1:101,1]+0:100,col=cols[4])
#text(30,86,expression(a+e(a)+sigma("y|a")),col=cols[4],cex=.7,pos=4)
#text(30,68,expression(a+e(a)),col=cols[4],cex=.7,pos=4)
#text(30,50,expression(a+e(a)-sigma("y|a")),col=cols[4],cex=.7,pos=4)
#
#text(10,96,expression(a+e(a)+sigma("y|a")),col=cols[2],cex=.7,pos=4)
#text(10,80,expression(a+e(a)),col=cols[2],cex=.7,pos=4)
#text(10,72,expression(a+e(a)-sigma("y|a")),col=cols[2],cex=.7,pos=4)
#
#
#text(20,c(89,60),c("2000","1900"),col=cols[c(2,4)])
#text(50,15, "a")
#text(-10,65, "a + y",srt=90)
#dev.off()

cols <- gray(c(.6,.3,0))
# Figure for Sweden standard deviation
pdf("Figures/SwedenFemalesSD1900_2000_Poster.pdf", width = 4, height = 4)
par(mai=c(.6,.6,.2,.2), xaxs = "i", yaxs = "i",xpd = TRUE)
plot(0:110, FSWE[["Var"]][1:111,3], type = 'n', lty = 1,
		main = paste0(""),
		lwd=c(1,2,3), 
		xlab = "", 
		ylab = "",
		ylim=c(0,35),
		xlim=c(0,110),
		axes = FALSE,
		panel.first = list(
				rect(0,0,110,35,border = NA,col=gray(.95)),
				segments(0,seq(0,35,by=5),110,seq(0,35,by=5),col = "white",lwd=.5),
				segments(seq(10,100,by=10),0,seq(10,100,by=10),35,col="white",lwd=.5),
				text(0,seq(0,35,by=5),seq(0,35,by=5),cex=.7,pos=2),
				text(seq(0,100,by=10),0,seq(0,100,by=10),cex=.7,pos=1)))
head(FSWE[["Var"]])
lines(0:110, sqrt(FSWE[["Var"]][1:111,"2000"]), col = cols[3],lwd=1)
lines(0:103, sqrt(FSWE[["Var"]][1:104,"1950"]), col = cols[2],lwd=2)
lines(0:105, sqrt(FSWE[["Var"]][1:106,"1900"]), col = cols[1],lwd=3)
text(20,11,"2000",col=cols[3],cex=.8)
text(20,13,"1950",col=cols[2],cex=.8)
text(20,19,"1900",col=cols[1],cex=.8)
dev.off()



# Figure for Sweden Skewness
pdf("Figures/SwedenFemalesSkew1900_2000_Poster.pdf", width = 4, height = 4)
par(mai=c(.6,.6,.2,.2), xaxs = "i", yaxs = "i",xpd = TRUE)
plot(0:105, FSWE[["Sskew"]][1:106,3], type = 'n', lty = 1,
		main = paste0(""),
		lwd=c(1,2,3), 
		xlab = "", 
		ylab = "",
		xlim=c(0,110),
		ylim=c(-2,1.7),
		axes = FALSE,
		panel.first = list(
				rect(0,-2,110,1.7,border = NA,col=gray(.95)),
				segments(0,seq(-2,1.5,by=.5),110,seq(-2,1.5,by=.5),col = "white",lwd=.5),
				segments(seq(10,100,by=10),-2,seq(10,100,by=10),1.7,col="white",lwd=.5),
				text(0,seq(-2,1.5,by=.5),seq(-2,1.5,by=.5),cex=.7,pos=2),
				text(seq(0,100,by=10),-2,seq(0,100,by=10),cex=.7,pos=1)))
lines(0:105, FSWE[["Sskew"]][1:106,"2000"], col = cols[3],lwd=1)
lines(0:105, FSWE[["Sskew"]][1:106,"1950"], col = cols[2],lwd=2)
lines(0:105, FSWE[["Sskew"]][1:106,"1900"], col = cols[1],lwd=3)
text(20,-1.4,"2000",col=cols[3],srt=30,cex=.8)
text(20,-1.1,"1950",col=cols[2],srt=30,cex=.8)
text(20,-.8,"1900",col=cols[1],srt=10,cex=.8)

segments(50,0,80,0)
text(50,0,"Tails of f(y|a) balanced",pos=2,cex=.8)
text(40,-.5,"Left skew",pos=2,cex=.8)
text(70,.5,"Right skew",pos=2,cex=.8)
dev.off()

# Figure for Sweden Kurtosis
pdf("Figures/SwedenFemalesKurt1900_2000_Poster.pdf", width = 4, height = 4)
par(mai=c(.6,.6,.2,.2), xaxs = "i", yaxs = "i",xpd = TRUE)
plot(0:105, FSWE[["Skurt"]][1:106,3], type = 'n', lty = 1,
		main = paste0(""),
		lwd=c(1,2,3), 
		xlab = "", 
		ylab = "",
		ylim=c(1-3,10-3),
		xlim=c(0,110),
		axes = FALSE,
		panel.first = list(
				rect(0,1-3,110,10-3,border = NA,col=gray(.95)),
				segments(0,seq(1,10,by=1)-3,110,seq(1,10,by=1)-3,col = "white",lwd=.5),
				segments(seq(10,100,by=10),1-3,seq(10,100,by=10),10-3,col="white",lwd=.5),
				text(0,seq(1,10,by=1)-3,seq(1,10,by=1)-3,cex=.7,pos=2),
				text(seq(0,100,by=10),1-3,seq(0,100,by=10),cex=.7,pos=1)))
lines(0:105, FSWE[["Skurt"]][1:106,"2000"], col = cols[3])
lines(0:105, FSWE[["Skurt"]][1:106,"1950"], col = cols[2])
lines(0:105, FSWE[["Skurt"]][1:106,"1900"], col = cols[1])
text(40,4.3-3,"2000",col=cols[3],srt=-35,cex=.8)
text(40,3.6-3,"1950",col=cols[2],srt=-35,cex=.8)
text(30,3-3,"1900",col=cols[1],cex=.8)

text(0,8-3,"Variance driven by very\npremature deaths",pos=4,cex=.8)
text(105,6-3,"Variance driven by\nvery long survivors",pos=2,cex=.8)
dev.off()
# some kind of linear relationship between LCV and CV
# from age 0 until 85 or so. Not sure if it's constant.
# late age curl in LCV could be due to radix, but haven't tested it.
# age 0 difference due to ax use in CV, but midpoint in LCV


# Figure for Sweden CV
pdf("Figures/SwedenFemalesCV1900_2000_Poster.pdf", width = 4, height = 4)
par(mai=c(.6,.6,.2,.2), xaxs = "i", yaxs = "i",xpd = TRUE)
plot(0:105, FSWE[["CV"]][1:106,3], type = 'n', lty = 1,
		main = paste0(""),
		lwd=c(1,2,3), 
		xlab = "", 
		ylab = "",
		ylim=c(0,1),
		xlim=c(0,110),
		axes = FALSE,
		panel.first = list(
				rect(0,0,110,1,border = NA,col=gray(.95)),
				segments(0,seq(0,1,by=.2),110,seq(0,1,by=.2),col = "white",lwd=.5),
				segments(seq(10,100,by=10),0,seq(10,100,by=10),1,col="white",lwd=.5),
				text(0,seq(0,1,by=.2),seq(0,1,by=.2),cex=.7,pos=2),
				text(seq(0,100,by=10),0,seq(0,100,by=10),cex=.7,pos=1)))
lines(0:105, FSWE[["CV"]][1:106,"2000"], col = cols[3],lwd=1)
lines(0:105, FSWE[["CV"]][1:106,"1950"], col = cols[2],lwd=2)
lines(0:105, FSWE[["CV"]][1:106,"1900"], col = cols[1],lwd=3)
text(40,.21,"2000",col=cols[3],cex=.8)
text(40,.35,"1950",col=cols[2],cex=.8)
text(40,.42,"1900",col=cols[1],cex=.8)

dev.off()

