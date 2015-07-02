
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
	ticklabs    <- ticks       <- pretty(X, 10) # still only gives a rough number
	approx(zlim, n = nbreaks)$y
}

Matrix2Poly <- function(X,nbreaks){
	require(raster)
	breaks <- MatrixLevels(X,nbreaks)
	Ages   <- as.integer(rownames(X))
	Years  <- as.integer(colnames(X))
	r     <- raster(X[nrow(X):1,],
			   xmn = min(Years), xmx = max(Years)+1, 
			   ymn = min(Ages), ymx = max(Ages)+1)
	z     <- cut(r, breaks)
	p     <- rasterToPolygons(z, dissolve=TRUE)
	Lines <- contourLines(Years,Ages,t(X),levels=breaks)
	list(p=p,contours = Lines) # note you need to grab list elements for plotting
}

LexisPoly <- function(X,nbreaks){
	Ages   <- as.integer(rownames(X))
	Years  <- as.integer(colnames(X))
	Agest  <- Ages[Ages %% 10 == 0]
	Yearst <- Years[Years %% 10 == 0]
	xmx    <- max(Years)+1 ; xmn <- min(Years)
	ymx    <- max(Ages)+1;ymn <- min(Ages)
	Xp     <- Matrix2Poly(X,11)
	# start plotting
	par(mai=c(.5,.5,.2,1.5),xaxs="i",yaxs="i")
	plot(Xp$p,asp=1,border=NA,col=colramp(nbreaks-1))
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
	Colors      <- colramp(nbreaks-1)
	xr    <- max(Years)+1       # right plt absolute
	# now we can universalize, since xrr - xr = 1 inch :-)
	xw    <- diff(par("usr")[1:2]) / par("pin")[2]
	# bar width
	bw    <- xw * 1/4
	xl    <- xr + xw * 2/7
	yat   <- seq(min(Ages), max(Ages)+1, length = nbreaks)
	
	labs <- MatrixLevels(X,nbreaks)[-nbreaks]
	
	rect(xl, yat[1:(nbreaks-1)], xl+bw, yat[2:nbreaks], 
			col = Colors, border = gray(.3), lwd = .5, xpd = TRUE)
	text(xl+bw, yat[1:(nbreaks-1)], labs, cex = .7, pos = 4, xpd = TRUE)
}

# figure out how to rev colors via arg

