# Get the data
tufte2 <- read.csv("https://raw.githubusercontent.com/ZRVc/Tufte-Style-Slopegraphs-in-D3-/master/TufteGovernment.csv")

## "space" sets the line spacing.  It is the minimum distance between labels.
## "drop" controls the slope.

space <- 16
drop <- 24

## Set the constraints:  
constr <- c(1,2,4)

## Set the tolerance for the slopes: 
tol <- 0.0001

## The package "ROI" needs to be installed.
# install.packages("ROI")
# install.packages("alabama")
# install.packages("numDeriv")
# install.packages("ROI.plugin.alabama")

library(ROI)
library(ROI.plugin.alabama)
library("alabama")

x <- as.numeric(c(unlist(tufte2[,2]),(unlist(tufte2[,3]))))

x1.1 <- x[1:(length(x)/2)]
x2.1 <- x[((length(x)/2)+1):length(x)]

x1.1 <- x1.1
x2.1 <- x2.1

## This will set the slope so that a one unit decrease in x
## corresponds to a "drop" pixel drop on the page.
y1 <- drop*(max(x) - x1.1)
y2 <- (y1 - drop*(x2.1-x1.1))

y <- c(y1,y2)
z <- y

###################################################
## In this part, the functions used for optimization are being declared.

## This is the function I'm trying to optimize.
fn <- function(v,z=z){
  return(sum((z-v)^2))
}

gr <- function(v,z=z){
  return(2*v-2*z)
}

fn2 <- function(v){
  w <- c(rep(532.8,15),rep(340.8,15))
  return(sum(v^2-2*v*w+w^2))
}



gr2 <- function(v){
  w <- c(rep(532.8,15),rep(340.8,15))
  return(2*v-2*w)
}
## Wrapper functions to work with ROI
fn1 <- function(y) {
  return(fn(y,z))
}

gr1 <- function(y) {
  return(gr(y,z))
}


fn3 <- function(y) {
  return(-1*fn2(y))
}

gr3 <- function(y) {
  return(-1*gr2(y))
}

## First set of constraints: ensure that the slopes are drawn on the same scale.
ineq0 <- function(y,x1=x1.1,x2=x2.1) {
  
  index1 <- 0
  for(i in 1:length(x1)){
    if(x1[i]!=x2[i]){
      index1 <- c(index1, i)
    }
  }
  index1 <- index1[2:length(index1)]
  index2 <- subset((1:length(x1)),!((1:length(x1)) %in% index1))
  
  u <- rep(0,length(y))
  
  for(i in 1:(length(index1)-1)) {
    w1 <- rep(0,length(y))
    w1[index1[i]] <- 1/(x1[index1[i]]-x2[index1[i]])
    w1[index1[(i+1)]] <- -1/(x1[index1[(i+1)]]-x2[index1[(i+1)]])
    w1[(index1[i]+(length(y)/2))] <- -1/(x1[index1[i]]-x2[index1[i]])
    w1[(index1[i]+(length(y)/2+1))] <- 1/(x1[index1[(i+1)]]-x2[index1[(i+1)]])
    
    u <- rbind(u,w1)
  }
  
  if(length(index2) > 0){
    for(i in 1:length(index2)) {
      w2 <- rep(0,length(y))
      w2[index2[i]] <- 1
      w2[(index2[i]+(length(y)/2))] <- -1
      
      u <- rbind(u,w2)
    }
  }    
  
  return(unname(u[2:dim(u)[1],]))
}

## Second set of constraints: ensure that the numbers are displayed in order with the 
## correct line spacing.
ineq1 <- function(y) {
  
  y1 <- y[1:(length(y)/2)]
  y2 <- y[(length(y)/2+1):length(y)]
  
  u <- rep(0,length(y))
  
  for(i in 1:(length(y1)-1)) {
    for(j in (i+1):(length(y1))) {
      w <- rep(0,length(y))
      w[i] <- -1
      w[j] <- 1
      u <- rbind(u,w)
    }
  }
  
  for(i in (1:length(y2))) {
    y2index <- which(y2 > y2[i])
    for(j in y2index) {
      w <- rep(0,length(y))
      w[(i+(length(y)/2))] <- -1
      w[(j+(length(y)/2))] <- 1
      u <- rbind(u,w)
    }
  }
  return(unname(u[2:dim(u)[1],]))
}

## Third set of constraints:  Except for the endpoints, every number is between two
## other numbers.  The in-between number's positioning should be closer to whichever 
## of the two numbers is closer.  I.e., if 37.5 is between 39.0 and 35.2, it should
## be positioned closer to 39.0, because 39.0 - 37.5 < 37.5 - 35.2.  This constraint 
## is for the first column.
ineq2 <- function(y,x1=x1.1,x2=x2.1) {
  
  y1 <- y[1:(length(y)/2)]
  
  u <- rep(0,length(y))
  
  for(i in 1:(length(y)/2)) {
    if(y1[i] > min(y1) && y1[i] < max(y1)){
      upper <- min(y1[subset(which(x1 <= x1[i]), which(x1 <= x1[i])!=i)])
      lower <- max(y1[subset(which(x1 >= x1[i]), which(x1 >= x1[i])!=i)])
      y1low <- which(y1 == lower)
      y1high <- which(y1 == upper)
      
      if(x1[i] - x1[y1high] > x1[y1low] - x1[i]) {
        w <- rep(0,(length(y)/2))
        w[y1high] <- 1
        w[y1low] <- 1
        w[i] <- -2
        w1 <- c(w,rep(0,(length(y)/2)))
        u <- rbind(u,w1)
      }
      if(x1[i] - x1[y1high] < x1[y1low] - x1[i]) {
        w <- rep(0,(length(y)/2))
        w[y1high] <- -1
        w[y1low] <- -1
        w[i] <- 2
        w1 <- c(w,rep(0,(length(y)/2)))
        u <- rbind(u,w1)
      }
    }
  }
  return(unname(u[2:dim(u)[1],]))
}

## Fourth set of constraints:  This is the same as the third constraint, but it's for
## the second column.
ineq3 <- function(y,x1=x1.1,x2=x2.1) {
  
  y2 <- y[(length(y)/2+1):length(y)]
  
  u <- rep(0,length(y))
  
  for(i in 1:(length(y)/2)) {
    if(y2[i] > min(y2) && y2[i] < max(y2)){
      upper <- min(y2[subset(which(x2 <= x2[i]), which(x2 <= x2[i])!=i)])
      lower <- max(y2[subset(which(x2 >= x2[i]), which(x2 >= x2[i])!=i)])
      y2low <- which(y2 == lower)
      y2high <- which(y2 == upper)
      
      if(x2[i] - x2[y2high] > x2[y2low] - x2[i]) {
        w <- rep(0,(length(y)/2))
        w[y2high] <- 1
        w[y2low] <- 1
        w[i] <- -2
        w1 <- c(rep(0,(length(y)/2)),w)
        u <- rbind(u,w1)
      }
      if(x2[i] - x2[y2high] < x2[y2low] - x2[i]) {
        w <- rep(0,(length(y)/2))
        w[y2high] <- -1
        w[y2low] <- -1
        w[i] <- 2
        w1 <- c(rep(0,(length(y)/2)),w)
        u <- rbind(u,w1)
      }
    }
  }
  return(unname(u[2:dim(u)[1],]))
}

## Constraints for first wave of optimization:
ineq4 <- function(y) {
  
  y1 <- y[1:(length(y)/2)]
  y2 <- y[(length(y)/2+1):length(y)]
  
  u <- rep(0,length(y))
  
  for(i in 1:length(y1)) {
    w <- rep(0, length(y))
    w[i] <- 1
    u <- rbind(u,w)
  }
  
  for(i in (1:length(y2))) {
    w <- rep(0, length(y))
    w[(i+(length(y)/2))] <- 1
    u <- rbind(u,w)
  }
  return(unname(u[2:dim(u)[1],]))
}

inequalitymaker1 <- function(y){
  
  iq <- rep(0,length(y))
  
  iq <- rbind(iq,ineq0(y),ineq0(y))
  iq <- rbind(iq,ineq4(y),ineq4(y))
  iq <- rbind(iq,ineq2(y))
  iq <- rbind(iq,ineq3(y))
  return(iq[2:length(iq[,1]),])
  
}

ineqbegin <- inequalitymaker1(y)

yboundu <- y+0.1
yboundu[7] <- yboundu[7]-0.1+0.0001
yboundu[22] <- yboundu[22]-0.1+0.0001
yboundl <- y-0.1
yboundl[7] <- yboundl[7]+0.1-0.0001
yboundl[22] <- yboundl[22]+0.1-0.0001
yboundl[8] <- yboundl[7]+0.1
yboundu
yboundl

constraintmaker1 <- function(y,tol1=tol,ineq99=ineqbegin,yboundu1=yboundu,yboundl1=yboundl){
  
  dir1 <- 0
  rhs1 <- 0
  
  boundsl0 <- length(ineq0(y)[,1])
  boundsl1 <- length(ineq4(y)[,1])
  boundsl2 <- length(ineq2(y)[,1])
  boundsl3 <- length(ineq3(y)[,1])
  
  dir1 <- c(dir1,rep(">=",boundsl0),rep("<=",boundsl0))
  rhs1 <- c(rhs1,rep(-tol,boundsl0),rep(tol,boundsl0))
  
  dir1 <- c(dir1,rep(">=",boundsl1),rep("<=",boundsl1))
  rhs1 <- c(rhs1,yboundl1,yboundu1)
  
  dir1 <- c(dir1,rep(">=",boundsl2))
  rhs1 <- c(rhs1,rep(0,boundsl2))
  
  dir1 <- c(dir1,rep(">=",boundsl3))
  rhs1 <- c(rhs1,rep(0,boundsl3))
  
  dir1 <- dir1[2:length(dir1)]
  rhs1 <- rhs1[2:length(rhs1)]
  
  return(L_constraint(L=ineq99, dir=dir1, rhs=rhs1))
}
ineq1(points0)%*%points0
##################################  SOLVING1  #########################################
fo1 <-  F_objective(F=fn2,n=30,G=gr2)

lc1 <- constraintmaker1(y)

prob1 <- OP(fo1,lc1)
sol1 <- ROI_solve(prob1,solver="alabama",start=y)

points0 <- solution(sol1)


#############################SHRINKING IT BACK
inequalitymaker2 <- function(y){
  
  iq <- rep(0,length(y))
  
  iq <- rbind(iq,ineq0(y),ineq0(y))
  iq <- rbind(iq,ineq1(y))
  iq <- rbind(iq,ineq2(y))
  iq <- rbind(iq,ineq3(y))
  return(iq[2:length(iq[,1]),])
  
}

ineqnext <- inequalitymaker2(points0)



constraintmaker2 <- function(y,tol1=tol,ineq99=ineqnext){
  
  dir1 <- 0
  rhs1 <- 0
  
  boundsl0 <- length(ineq0(y)[,1])
  boundsl1 <- length(ineq1(y)[,1])
  boundsl2 <- length(ineq2(y)[,1])
  boundsl3 <- length(ineq3(y)[,1])
  
  dir1 <- c(dir1,rep(">=",boundsl0),rep("<=",boundsl0))
  rhs1 <- c(rhs1,rep(-tol,boundsl0),rep(tol,boundsl0))
  
  dir1 <- c(dir1,rep(">=",boundsl1))
  rhs1 <- c(rhs1,rep(0.05,boundsl1))
  
  dir1 <- c(dir1,rep(">=",boundsl2))
  rhs1 <- c(rhs1,rep(0,boundsl2))
  
  dir1 <- c(dir1,rep(">=",boundsl3))
  rhs1 <- c(rhs1,rep(0,boundsl3))
  
  dir1 <- dir1[2:length(dir1)]
  rhs1 <- rhs1[2:length(rhs1)]
  
  return(L_constraint(L=ineq99, dir=dir1, rhs=rhs1))
}


fo2 <-  F_objective(F=fn1,n=30,G=gr1)
lc2 <- constraintmaker2(points0)

prob2 <- OP(fo2,lc2)
sol2 <- ROI_solve(prob2,solver="alabama",start=points0)

points1 <- solution(sol2)



