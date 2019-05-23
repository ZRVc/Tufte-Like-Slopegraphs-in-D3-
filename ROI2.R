# Get the data
tufte2 <- read.csv("https://raw.githubusercontent.com/ZRVc/Tufte-Style-Slopegraphs-in-D3-/master/TufteGovernment.csv")

## space sets the linespacing.  It is the minimum distance between labels.
## drop controls the slope.

space <- 16
drop <- 24

## set the constraints:  
constr <- c(1,2,4)

## set the tolerance for the constraints 
tol <- 0.0001

## The package "ROI" needs to be installed.
# install.packages("ROI")
# install.packages("alabama")
# install.packages("numDeriv")
# install.packages("ROI.plugin.alabama")

library(ROI)
library(ROI.plugin.alabama)

x <- as.numeric(c(unlist(tufte2[,2]),(unlist(tufte2[,3]))))

x1.1 <- x[1:(length(x)/2)]
x2.1 <- x[((length(x)/2)+1):length(x)]

## This will set the slope so that a one percentage point decrease in GDP share
## corresponds to a "drop" pixel drop on the page.
y1 <- drop*(max(x) - x1.1)
y2 <- (y1 - drop*(x2.1-x1.1))

y <- c(y1,y2)

## difference finder
differ <- function(y) {
  
  y1 <- y[1:(length(y)/2)]
  y2 <- y[(length(y)/2+1):length(y)]
  
  diff1 <- Inf
  dist10 <- 0
  diff2 <- Inf
  dist20 <- 0
  
  for(i in 1:(length(y1)-1)) {
    for(j in (i+1):(length(y1))) {
      if(abs(y1[i]-y1[(j)]) < diff1) {
        if(abs(y1[i]-y1[(j)]) < 0.0001) {
          dist10 <- 1
        } else {
          diff1 <- abs(y1[i]-y1[(j)])
        }
      }
    }
  }
  
  for(i in 1:(length(y2)-1)) {
    for(j in (i+1):(length(y2))) {
      if(abs(y2[i]-y2[(j)]) < diff2) {
        if(abs(y2[i]-y2[(j)]) < 0.0001) {
          dist20 <- 1
        } else {
          diff2 <- abs(y2[i]-y2[(j)])
        }
      }
    }
  }
  
  diff <- min(c(diff1,diff2))
  return(diff)
}

## adjuster function
adjuster <- function(y,x1=x1.1,x2=x2.1,diff=mindiff) {
  
  y1 <- y[1:(length(y)/2)]
  y2 <- y[(length(y)/2+1):length(y)]
  
  index1 <- which(duplicated(x1))
  index2 <- which(duplicated(x2))
  
  y1.1 <- subset(y1,duplicated(x1))
  y1.2 <- subset(y1,duplicated(x2))
  y1.1tab <- table(y1.1)
  crashpts1 <- as.numeric(names(y1.1tab))
  
  y2.1 <- subset(y2,duplicated(x1)) 
  y2.2 <- subset(y2,duplicated(x2)) 
  y2.2tab <- table(y2.2)
  crashpts2 <- as.numeric(names(y2.2tab))
  
  
  n <- max(c(unname(y1.1tab),unname(y2.2tab)))
  
  
  adj <- diff/(n+1)
  
  if(length(index1)>0) {
    adj0 <- 0
    for(i in 1:length(y1.1tab)) {
      for(j in 1:(unname(y1.1tab)[i])) {
        adj0 <- c(adj0,j)
      }
    }
    adj0 <- adj0[2:length(adj0)]
    
    y1.1 <- y1.1+adj0*adj
    y2.1 <- y2.1+adj0*adj
    y1[index1] <- y1.1
    y2[index1] <- y2.1
  }
  
  
  if(length(index2)>0) {
    adj0 <- 0
    for(i in 1:length(y2.2tab)) {
      for(j in 1:(unname(y2.2tab)[i])) {
        adj0 <- c(adj0,j)
      }
    }
    adj0 <- adj0[2:length(adj0)]
    
    y2.2 <- y2.2+adj0*adj
    y1.2 <- y1.2+adj0*adj
    y1[index2] <- y1.2
    y2[index2] <- y2.2
  }
  return(c(y1,y2))
}


mindiff <- differ(y)

space2 <- 1.1*space/(mindiff)

z <- adjuster(y)
## This will be the starting point in our search for an optimum solution.  I multiply 
## everything by "space" so that the minimum distance is now the linespacing I'm
## after.
start1 <- c(space2*z[1:(length(z)/2)],space2*z[(length(z)/2+1):length(z)])

###################################################
## In this part, the functions used for optimization are being declared.

## This is the function I'm trying to optimize.
fn1 <- function(v,z=z){
  return(sum((z-v)^2))
}

gr1 <- function(v,z=z){
  return(2*v-2*z)
}

## Wrapper functions to work with ROI
fn <- function(y) {
  return(fn1(y,z))
}

gr <- function(y) {
  return(gr1(y,z))
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
## correct linespacing.
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
## is for the first column.  It isn't used in the solution because I'm trying to 
## keep the size the same as Tufte's.
ineq2 <- function(y,x1=x1.1,x2=x2.1) {
  
  y1 <- y[1:(length(y)/2)]
  
  u <- rep(0,length(y))
  
  for(i in 1:(length(y)/2)) {
    if(y1[i] > min(y1) && y1[i] < max(y1)){
      upper <- min(y1[subset(which(x1 <= x1[i]), which(x1 <= x1[i])!=i)])
      lower <- max(y1[subset(which(x1 >= x1[i]), which(x1 >= x1[i])!=i)])
      y1low <- which(y1 == lower)
      y1high <- which(y1 == upper)
      for(k in y1low){
        for(m in y1high){
          if(x1[i] - x1[m] > x1[k] - x1[i]) {
            w <- rep(0,(length(y)/2))
            w[m] <- 1
            w[k] <- 1
            w[i] <- -2
            w1 <- c(w,rep(0,(length(y)/2)))
            u <- rbind(u,w1)
          }
          if(x1[i] - x1[m] < x1[k] - x1[i]) {
            w <- rep(0,(length(y)/2))
            w[m] <- -1
            w[k] <- -1
            w[i] <- 2
            w1 <- c(w,rep(0,(length(y)/2)))
            u <- rbind(u,w1)
          }
        }
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
      for(k in y2low){
        for(m in y2high){
          if(x2[i] - x2[m] > x2[k] - x2[i]) {
            w <- rep(0,(length(y)/2))
            w[m] <- 1
            w[k] <- 1
            w[i] <- -2
            w1 <- c(rep(0,(length(y)/2)),w)
            u <- rbind(u,w1)
          }
          if(x2[i] - x2[m] < x2[k] - x2[i]) {
            w <- rep(0,(length(y)/2))
            w[m] <- -1
            w[k] <- -1
            w[i] <- 2
            w1 <- c(rep(0,(length(y)/2)),w)
            u <- rbind(u,w1)
          }
        }
      }
    }
  }
  return(unname(u[2:dim(u)[1],]))
}

inequalitymaker <- function(y,const=constr){
  
  iq <- rep(0,length(y))
  
  if(1 %in% const){
    iq <- rbind(iq,ineq0(y),ineq0(y))
  } 
  if(2 %in% const) {
    iq <- rbind(iq,ineq1(y))
  } 
  if(3 %in% const) {
    iq <- rbind(iq,ineq2(y))
  } 
  if(4 %in% const) {
    iq <- rbind(iq,ineq3(y))
  }
  return(iq[2:length(iq[,1]),])
  
}


ineq <- inequalitymaker(start1)

constraintmaker <- function(y,tol1=tol,ineq99=ineq,space1=space,const=constr){
  
  dir1 <- 0
  rhs1 <- 0
  
  boundsl0 <- length(ineq0(y)[,1])
  boundsl1 <- length(ineq1(y)[,1])
  boundsl2 <- length(ineq2(y)[,1])
  boundsl3 <- length(ineq3(y)[,1])
  
  if(1 %in% const) {
    dir1 <- c(dir1,rep(">=",boundsl0),rep("<=",boundsl0))
    rhs1 <- c(rhs1,rep(-tol,boundsl0),rep(tol,boundsl0))
  }
  if(2 %in% const) {
    dir1 <- c(dir1,rep(">=",boundsl1))
    rhs1 <- c(rhs1,rep(space1,boundsl1))
  }
  if(3 %in% const) {
    dir1 <- c(dir1,rep(">=",boundsl2))
    rhs1 <- c(rhs1,rep(0,boundsl2))
  }
  if(4 %in% const) {
    dir1 <- c(dir1,rep(">=",boundsl3))
    rhs1 <- c(rhs1,rep(0,boundsl3))
  }
  dir1 <- dir1[2:length(dir1)]
  rhs1 <- rhs1[2:length(rhs1)]
  
  return(L_constraint(L=ineq99, dir=dir1, rhs=rhs1))
}
##################################  SOLVING  #########################################
fo <-  F_objective(F=fn,n=length(start1),G=gr)
lc <- constraintmaker(start1)

prob <- OP(fo,lc)
sol <- ROI_solve(prob,solver="alabama",start=start1)

## The points (formatted for easy pasting into D3)
points0 <- round(solution(sol)-min(solution(sol)),2)

points1 <- points0[1]
for(i in 2:(length(points0)/2)) {
  points1 <- paste(points1,points0[i],sep = ", ")
}


points2 <- points0[(length(points0)/2+1)]
for(i in (length(points0)/2+2):length(points0)) {
  points2 <- paste(points2,points0[i],sep = ", ")
}

points1
points2

## Check the slopes
y1Solve <- round(solution(sol)-min(solution(sol)),2)[1:(length(solution(sol))/2)]
y2Solve <- round(solution(sol)-min(solution(sol)),2)[(length(solution(sol))/2+1):length(solution(sol))]

(y2Solve-y1Solve)/(x2.1-x1.1)
