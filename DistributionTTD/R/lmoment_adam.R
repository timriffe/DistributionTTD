# original functions by Adam Lenart for L-Moments.
# subsumed in package functions at this time after modification by Tim Riffe.
# new equivalents bear the same names, but with '_ta' appended, for the most part

# ----------------------------------------------- #
#                 Functions                       #
# ----------------------------------------------- #

getLT <- function (country,username,password,sex) {
  print(paste("Downloading",country))
  whichFile <- switch(sex,"Female"="fltper_1x1.txt","Male"="mltper_1x1.txt")
  path <- paste("http://www.mortality.org/hmd/", country, "/STATS/",
                whichFile, sep = "")
  userpwd <- paste(username, ":", password, sep = "")
  txt <- getURL(path, userpwd = userpwd)
  con <- textConnection(txt)
  dat <- read.table(con, skip = 2, header = TRUE, na.strings = ".")
  close(con)
  datCnt <- cbind(dat,country)
  return(datCnt)
}

# ---------------------------------------------- #
#     Functions to calculate L-moments           #
# ---------------------------------------------- #

getB0b <- function(age,w) weighted.mean(x=age,w=w)

getB1b <- function(age,w) {
    L <- list()
  #  k <- 1
    n <- length(age)
    W <- c(0,cumsum(w))
 for(i in 1:n) {
    L[[i]] <- age[i]*w[i]*(W[i]+1/2*(w[i]-1))
   #            age[i]*(W[i]+(0:(w[i]-1)))
 }

   1/(sum(w)*(sum(w)-1))*sum(unlist(L))
}


getB1bSection <- function(age,w,sec) {
    L <- list()
  #  k <- 1
    n <- length(age)
    W <- c(0,cumsum(w))
 for(i in 1:n) {
    L[[i]] <- age[i]*w[i]*(W[i]+1/2*(w[i]-1))
   #            age[i]*(W[i]+(0:(w[i]-1)))
 }
   Ls <- unlist(L)

   1/(sum(w[1:sec])*(sum(w[1:sec])-1))*sum(Ls[1:sex])
}


getB1c <- function(age,w) {
    L <- list()
    #  k <- 1
    n <- length(age)
    W <- c(0,cumsum(w))
    for(i in 1:n) {
        L[[i]] <- age[i]*w[i]*(W[i]+1/2*(w[i]))
    #            age[i]*(W[i]+(0:(w[i]-1)))
    }

    1/(sum(w)^(2))*sum(unlist(L))
}


 getB2b <- function(age,w) {
     L <- list()
     n <- length(age)
     W <- c(0,cumsum(w))

  for(i in 1:n) {
     wd <- w[i]
#     sum((wd)*age[i]*(W[i]^2+W[i]*1/2*(wd-1+wd-3)+(wd-1)*(wd-2)/3))
    L[[i]] <-   sum((wd)*age[i]*(W[i]^2+W[i]*(wd-2)+(wd-1)*(wd-2)/3))


  }
    1/(sum(w)*(sum(w)-1)*(sum(w)-2))*sum(unlist(L))
 }

 getB2c <- function(age,w) {
     L <- list()
     n <- length(age)
     W <- c(0,cumsum(w))

  for(i in 1:n) {
     wd <- w[i]
#     sum((wd)*age[i]*(W[i]^2+W[i]*1/2*(wd-1+wd-3)+(wd-1)*(wd-2)/3))
    L[[i]] <-   sum((wd)*age[i]*(W[i]^2+W[i]*(wd)+(wd)^(2)/3))


  }
    1/(sum(w)^(3))*sum(unlist(L))
 }



getL2b <- function(age,w)  2*getB1b(age=age,w=w)-getB0b(age=age,w=w)
getL2c <- function(age,w)  2*getB1c(age=age,w=w)-getB0b(age=age,w=w)

getL3b <- function(age,w)
    6*getB2b(age=age,w=w)-6*getB1b(age=age,w=w)+getB0b(age=age,w=w)
getL3c <- function(age,w)
    6*getB2c(age=age,w=w)-6*getB1c(age=age,w=w)+getB0b(age=age,w=w)
getL2section <- function(age,w,section) 2*getB1bSection(age=age,w=w,sec=section)-getB0b(age=age,w=w)


getSkew <- function(age,w) getL3b(age=age,w=w)/getL2b(age=age,w=w)
getSkewc <- function(age,w) getL3c(age=age,w=w)/getL2c(age=age,w=w)
getCV <- function(age,w) getL2b(age=age,w=w)/getB0b(age=age,w=w)


# ---------------------------------------------- #
#                    Run                         #
# ---------------------------------------------- #

sex <- c("Female","Male")
# HMD country codes
countries <-
  c("AUS","AUT","BLR","BEL","BGR","CAN","CHL","CZE","DNK","EST",
    "FIN","FRATNP","DEUTNP","HUN","ISL","IRL","ISR","ITA","JPN",
  "LVA","LTU","LUX","NLD","NZL_NM","NOR","POL","PRT","RUS","SVK",
  "SVN","ESP","SWE","CHE","TWN","GBRTENW","USA","UKR")
     dL <- list()
        k <- 1
for(l in sex) {
    for(i in countries) {
        dat <- getLT(country=i,username,password,sex=l)
        years <- unique(dat$Year)
        for(j in years) {
            d2 <- subset(dat,Year==j)
            skew <- getSkew(age=0:110,w=d2$dx)
            cv <- getCV(age=0:110,w=d2$dx)
            mad <- getB0b(age=0:110,w=d2$dx)
            L2 <- getL2b(age=0:110,w=d2$dx)
            L3 <- getL3b(age=0:110,w=d2$dx)
            dL[[k]] <-
                data.frame(skew=skew,cv=cv,mad=mad,L2=L2,L3=L3,sex=l,country=i,Year=j,e0=d2$ex[1],
                           eastern=ifelse(any(c("BLR","BGR","CZE","EST","HUN","LVA","LTU","POL","RUS","SVK","SVN","UKR")==i),yes=1,no=0))
            k <- k+1
        }
    }
}


dats <- plyr:::rbind.fill(dL)
