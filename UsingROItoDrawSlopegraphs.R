## Get the data
tufte2 <- read.csv("https://raw.githubusercontent.com/ZRVc/Tufte-Style-Slopegraphs-in-D3-/42095cdaf56e02762fbb785fea86c9d1b0fadd3d/TufteGovernment.csv")

## The package "ROI" needs to be installed.
## install.packages("ROI")
## install.packages("alabama")
## install.packages("numDeriv")
## install.packages("ROI.plugin.alabama")

library(ROI)
library(ROI.plugin.alabama)

x <- as.numeric(c(unlist(tufte2[,2]),(unlist(tufte2[,3]))))

x1 <- x[1:(length(x)/2)]
x2 <- x[((length(x)/2)+1):length(x)]

## space sets the linespacing.  It is the minimum distance between labels.
## drop controls the slope.

space <- 18
drop <- 24

## This will set the slope so that a one percentage point decrease in GDP share
## corresponds to a 24 point drop on the page.  If there's a tie, it adds 1 to  
## one of them, so that (for this data at least) the minimum distance between 
## two points is 1.  This code will need to be adjusted for other data sets.

y1 <- drop*(max(x) - x1)
y2 <- (y1 - drop*(x2-x1))

for(i in 1:(length(y1)-1)) {
  if(y1[i] == y1[(i+1)]) {
    y1[(i+1)] <- y1[(i+1)]+1
    y2[(i+1)] <- y2[(i+1)]+1
  }
}

## This will be the starting point in our search for an optimum solution.  I multiply 
## everything by 18 so that the minimum distance is now 18 (which is the linespacing
## I'm after).  This code will need to be adjusted to use an arbitrary minimum.

start1 <- c(space*y1,space*y2)

###################################################
## In this part, the functions used for optimization are being declared.

## This is the function I'm trying to optimize.

fn <- function(y){
  z <- c(252.0, 321.6, 333.6, 400.8, 441.6, 477.6, 532.8, 533.8, 540.0, 648.0,
         650.4, 734.4, 741.6, 837.6, 880.8, 0.0, 38.4, 124.8, 441.6, 336.0, 348.0, 340.8, 519.4, 460.8, 520.8,
         597.6, 643.2, 580.8, 727.2, 739.2)
  return(sum((z - y)^2))
}

gr <- function(y){
  z <- c(252.0, 321.6, 333.6, 400.8, 441.6, 477.6, 532.8, 533.8, 540.0, 648.0,
         650.4, 734.4, 741.6, 837.6, 880.8, 0.0, 38.4, 124.8, 441.6, 336.0, 348.0, 340.8, 519.4, 460.8, 520.8,
         597.6, 643.2, 580.8, 727.2, 739.2)
  return(2*y-2*z)
}


ineq0 <- function(y) {
  x <- c(46.9, 44.0, 43.5, 40.7, 39.0, 37.5, 35.2, 35.2, 34.9, 30.4, 30.3, 26.8,
          26.5, 22.5, 20.7, 57.4, 55.8, 52.2, 39.0, 43.4, 42.9, 43.2, 35.8, 38.2, 35.7, 32.5, 30.6,
          33.2, 27.1, 26.6)

  u <- rep(0,30)

  for(i in 1:(length(y)/2-1)) {
    w <- rep(0,30)
    w[i] <- 1/(x[i]-x[(i+15)])
    w[(i+1)] <- -1/(x[(i+1)]-x[(i+16)])
    w[(i+15)] <- -1/(x[i]-x[(i+15)])
    w[(i+16)] <- 1/(x[(i+1)]-x[(i+16)])
        
    u <- rbind(u,w)
  }
  return(unname(u[2:dim(u)[1],]))
}

ineq1 <- function(y) {

  y1 <- y[1:(length(y)/2)]
  y2 <- y[(length(y)/2+1):length(y)]

  u <- rep(0,30)
  
  for(i in 1:(length(y1)-1)) {
    for(j in (i+1):(length(y1))) {
      w <- rep(0,30)
      w[i] <- -1
      w[j] <- 1
      u <- rbind(u,w)
    }
  }
  
  for(i in (1:length(y2))) {
    y2index <- which(y2 > y2[i])
    for(j in y2index) {
      w <- rep(0,30)
      w[(i+15)] <- -1
      w[(j+15)] <- 1
      u <- rbind(u,w)
    }
  }
  return(unname(u[2:dim(u)[1],]))
}

ineq2 <- function(y) {
  x1 <- c(46.9, 44.0, 43.5, 40.7, 39.0, 37.5, 35.2, 35.2, 34.9, 30.4, 30.3, 26.8,
          26.5, 22.5, 20.7)
  x2 <- c(57.4, 55.8, 52.2, 39.0, 43.4, 42.9, 43.2, 35.8, 38.2, 35.7, 32.5, 30.6,
          33.2, 27.1, 26.6)
  
  y1 <- y[1:(length(y)/2)]
  
  u <- rep(0,30)
  
  for(i in 1:(length(y)/2)) {
    if(x1[i] > min(x1) && x1[i] < max(x1)){
      upper <- min(y1[which(x1 < x1[i])])
      lower <- max(y1[which(x1 > x1[i])])
      y1low <- which(y1 == lower)
      y1high <- which(y1 == upper)
      for(k in y1low){
        for(m in y1high){
          if(x1[i] - x1[m] > x1[k] - x1[i]) {
            w <- rep(0,15)
            w[m] <- 1
            w[k] <- 1
            w[i] <- -2
            w1 <- c(w,rep(0,15))
            u <- rbind(u,w1)
          }
          if(x1[i] - x1[m] < x1[k] - x1[i]) {
            w <- rep(0,15)
            w[m] <- -1
            w[k] <- -1
            w[i] <- 2
            w1 <- c(w,rep(0,15))
            u <- rbind(u,w1)
          }
        }
      }
    }
  }
  return(unname(u[2:dim(u)[1],]))
}


ineq3 <- function(y) {
  x1 <- c(46.9, 44.0, 43.5, 40.7, 39.0, 37.5, 35.2, 35.2, 34.9, 30.4, 30.3, 26.8,
          26.5, 22.5, 20.7)
  x2 <- c(57.4, 55.8, 52.2, 39.0, 43.4, 42.9, 43.2, 35.8, 38.2, 35.7, 32.5, 30.6,
          33.2, 27.1, 26.6)
  
  y2 <- y[(length(y)/2+1):length(y)]
  
  u <- rep(0,30)
  
  for(i in 1:(length(y)/2)) {
    if(x2[i] > min(x2) && x2[i] < max(x2)){
      upper <- min(y2[which(x2 < x2[i])])
      lower <- max(y2[which(x2 > x2[i])])
      y2low <- which(y2 == lower)
      y2high <- which(y2 == upper)
      for(k in y2low){
        for(m in y2high){
          if(x2[i] - x2[m] > x2[k] - x2[i]) {
            w <- rep(0,15)
            w[m] <- 1
            w[k] <- 1
            w[i] <- -2
            w1 <- c(rep(0,15),w)
            u <- rbind(u,w1)
          }
          if(x2[i] - x2[m] < x2[k] - x2[i]) {
            w <- rep(0,15)
            w[m] <- -1
            w[k] <- -1
            w[i] <- 2
            w1 <- c(rep(0,15),w)
            u <- rbind(u,w1)
          }
        }
      }
    }
  }
  return(unname(u[2:dim(u)[1],]))
}


ineq <- rbind(ineq0(start1),ineq0(start1),ineq1(start1),ineq3(start1))

lc <- L_constraint(L=ineq, dir=c(rep(">=",14),rep("<=",14),rep(">=",210),rep(">=",13)),
             rhs=c(rep(-0.01,14),rep(0.01,14),rep(18,210),rep(0,13)))

fo <-  F_objective(F=fn,n=30,G=gr)

prob <- OP(fo,lc)
sol <- ROI_solve(prob,solver="alabama",start=start1)

## The points
round(solution(sol)-min(solution(sol)),1)

## Check the slopes
y1Solve <- round(solution(sol)-min(solution(sol)),1)[1:(length(solution(sol))/2)]
y2Solve <- round(solution(sol)-min(solution(sol)),1)[(length(solution(sol))/2+1):length(solution(sol))]

(y2Solve-y1Solve)/(x2-x1)
