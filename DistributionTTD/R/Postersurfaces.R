
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


Sskew <- acast(HMD[HMD$CNTRY == "SWE" & Sex == "f", ],Age~Year, value.var = "Sskew")
Var   <- acast(HMD[HMD$CNTRY == "SWE" & Sex == "f", ],Age~Year, value.var = "Var")
Kurt  <- acast(HMD[HMD$CNTRY == "SWE" & Sex == "f", ],Age~Year, value.var = "Skurt")
ex    <- acast(HMD[HMD$CNTRY == "SWE" & Sex == "f", ],Age~Year, value.var = "ex")
CV    <- acast(HMD[HMD$CNTRY == "SWE" & Sex == "f", ],Age~Year, value.var = "CV")

# I would typically use LexisMap() for plotting surfaces, but this wasn't
# useful for creating the poster. Image functions in R use raster cells, such
# that once you're in inkscape you get tons of vertices and it becomes a memory 
# hog. We therefore merge cells into polygons within like-colored regions of the
# plot. Solution provided on SO here:
# http://stackoverflow.com/questions/29837704/how-to-save-levelplot-with-like-cells-merged/29840590#29840590
# and it took a bit of modification to get back to my original product. In principle
# it could be an exact replica, differing only in the interior vertices.

MatrixLevels <- function(X,nbreaks){
	zlim <- range(pretty(X))
	X[X < zlim[1]]        <- zlim[1]
	X[X > zlim[2]]        <- zlim[2]
	pretty(X, nbreaks) # still only gives a rough number
}

Matrix2Poly <- function(X,nbreaks){
	require(raster)
	breaks <- MatrixLevels(X,nbreaks)
	Ages   <- as.integer(rownames(X))
	Years  <- as.integer(colnames(X))
	r     <- raster(X[nrow(X):1,],
			   xmn = min(Years), xmx = max(Years)+1, 
			   ymn = min(Ages), ymx = max(Ages)+1)
	z     <- cut(r, breaks)# plot(p)
	p     <- rasterToPolygons(z, dissolve=TRUE)
	Lines <- contourLines(Years,Ages,t(X),levels=breaks)
	list(p=p,contours = Lines,N=length(breaks)-1) # note you need to grab list elements for plotting
}

LexisPoly <- function(X,nbreaks){
	Ages   <- as.integer(rownames(X))
	Years  <- as.integer(colnames(X))
	Agest  <- Ages[Ages %% 10 == 0]
	Yearst <- Years[Years %% 10 == 0]
	xmx    <- max(Years)+1 ; xmn <- min(Years)
	ymx    <- max(Ages)+1;ymn <- min(Ages)
	Xp     <- Matrix2Poly(X,11)
	N      <- Xp$N
	# start plotting
	par(mai=c(.5,.5,.2,1.5),xaxs="i",yaxs="i")
	plot(Xp$p,asp=1,border=NA,col=colramp(N))
	NULLS  <- lapply(Xp$contour,lines)
	rect(xmn,ymn,xmx,ymx)
	
	# yearsticks:
	segments(Yearst,ymn,Yearst,ymn-3,xpd=TRUE)
	segments(Yearst,ymx,Yearst,ymx+3,xpd=TRUE)
    # ages ticks:
	segments(xmn,Agest,xmn-3,Agest,xpd=TRUE)
	segments(xmx,Agest,xmx+3,Agest,xpd=TRUE)
	# tick labels
	text(xmn-2,Agest,Agest,pos=2,cex=.7,xpd=TRUE)
	text(Yearst,ymn-2,Yearst,pos=1,cex=.7,xpd=TRUE)
	
	# legend... will need to ensure an inch margin on the right...
	Colors      <- colramp(N)
	xr    <- max(Years)+1       # right plt absolute
	# now we can universalize, since xrr - xr = 1 inch :-)
	xw    <- diff(par("usr")[1:2]) / par("pin")[2]
	# bar width
	bw    <- xw * 1/4
	xl    <- xr + xw * 2/7
	yat   <- seq(min(Ages), max(Ages)+1, length = N+1)
	
	labs <- MatrixLevels(X,N+1)[-(N+1)]
	
	rect(xl, yat[1:N], xl+bw, yat[2:(N+1)], 
			col = Colors, border = gray(.3), lwd = .5, xpd = TRUE)
	text(xl+bw, yat[1:N], labs, cex = .7, pos = 4, xpd = TRUE)
}

# figure out how to rev colors via arg

#colramp <- colorRampPalette(brewer.pal(9, "BuGn"), space = "Lab")
#
#pdf("Figures/Surf/exSWEfposter.pdf",height=5,width=11)
#LexisPoly(ex,11)
#dev.off()

colramp <- colorRampPalette(brewer.pal(9, "PuRd"), space = "Lab")

pdf("Figures/Surf/SDSWEfposter.pdf",height=5,width=11)
LexisPoly(sqrt(Var),15)
dev.off()

colramp <- colorRampPalette(brewer.pal(9, "OrRd"), space = "Lab")
pdf("Figures/Surf/SskewSWEfposter.pdf",height=5,width=11)
LexisPoly(Sskew,11)
dev.off()

colramp <- colorRampPalette(brewer.pal(9, "YlGn"), space = "Lab")

pdf("Figures/Surf/KurtSWEfposter.pdf",height=5,width=11)
LexisPoly(Kurt,11)
dev.off()

colramp <- colorRampPalette(brewer.pal(9, "GnBu"), space = "Lab")

pdf("Figures/Surf/CVSWEfposter.pdf",height=5,width=11)
LexisPoly(CV,15)
dev.off()

display.brewer.all()