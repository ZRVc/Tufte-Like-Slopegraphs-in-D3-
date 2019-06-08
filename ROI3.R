# Get the data
tufte2 <- read.csv("https://raw.githubusercontent.com/ZRVc/Tufte-Style-Slopegraphs-in-D3-/master/TufteGovernment.csv")

## "space" sets the line spacing.  It is the minimum distance between labels.
## "drop" controls the slope.

space <- 16
drop <- 24

## Set the constraints:  
constr <- c(1,2,4)

## Set the tolerance for the slopes: 
slopetol <- 0.00001
spreadtol <- 0.005
equaltiytol <- 0.001

## The package "ROI" needs to be installed.
# install.packages("ROI")
# install.packages("alabama")
# install.packages("numDeriv")
# install.packages("ROI.plugin.alabama")

library(ROI)
library(ROI.plugin.alabama)
library("alabama")

x <- as.numeric(c(unlist(tufte2[5:9,2]),(unlist(tufte2[5:9,3]))))

x1.1 <- x[1:(length(x)/2)]
x2.1 <- x[((length(x)/2)+1):length(x)]


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
  return((v[1]-v[2])^2+(v[2]-v[3])^2+(v[3]-v[4])^2+(v[4]-v[5])^2+(v[6]-v[7])^2+(v[7]-v[8])^2+(v[8]-v[9])^2+(v[9]-v[10])^2)
}



gr2 <- function(v){
  return(c((2*v[1]-2*v[2]),(4*v[2]-2*v[1]-2*v[3]),(4*v[3]-2*v[2]-2*v[4]),(4*v[4]-2*v[3]-2*v[5]),(2*v[5]-2*v[4]),
           (2*v[6]-2*v[7]),(4*v[7]-2*v[6]-2*v[8]),(4*v[8]-2*v[7]-2*v[9]),(4*v[9]-2*v[8]-2*v[10]),(2*v[10]-2*v[9])))
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
ineq0(y)
## First set of constraints: ensure that the slopes are drawn on the same scale.
ineq0 <- function(y,x1=x1.1,x2=x2.1,tol1=tol) {
  
  index1 <- 0
  for(i in 1:length(x1)){
    if(x1[i]!=x2[i]){
      index1 <- c(index1,i)
    }
  }
  index1 <- index1[2:length(index1)]
  index2 <- subset((1:length(x1)),!((1:length(x1)) %in% index1))
  
  u <- rep(0,length(y))
  v1 <- 0
  v2 <- 0
  
  for(i in 1:(length(index1)-1)) {
    w1 <- rep(0,length(y))
    w1[index1[i]] <- 1/(x1[index1[i]]-x2[index1[i]])
    w1[index1[(i+1)]] <- -1/(x1[index1[(i+1)]]-x2[index1[(i+1)]])
    w1[(index1[i]+(length(y)/2))] <- -1/(x1[index1[i]]-x2[index1[i]])
    w1[(index1[i]+(length(y)/2+1))] <- 1/(x1[index1[(i+1)]]-x2[index1[(i+1)]])
    
    u <- rbind(u,w1,w1)
    v1 <- append(v1,c(1,2),after=length(v1))
    v2 <- append(v2,c(-tol,tol),after=length(v2)) 
  }
  
  if(length(index2) > 0){
    for(i in 1:length(index2)) {
      w2 <- rep(0,length(y))
      w2[index2[i]] <- 1
      w2[(index2[i]+(length(y)/2))] <- -1
      
      u <- rbind(u,w2,w2)
      v1 <- append(v1,c(1,2),after=length(v1))
      v2 <- append(v2,c(-tol,tol),after=length(v2)) 
    }
  }    
  
  umod <- u[2:dim(u)[1],]
  v1mod <- v1[2:length(v1)]
  v2mod <- v2[2:length(v2)]
  
  return(unname(cbind(umod,v1mod,v2mod)))
}

## Second set of constraints: ensure that the numbers are displayed in order with the 
## correct line spacing.
ineq1 <- function(y,space=space1) {
  
  y1 <- y[1:(length(y)/2)]
  y2 <- y[(length(y)/2+1):length(y)]
  
  u <- rep(0,length(y))
  v1 <- 0
  v2 <- 0
  
  for(i in 1:(length(y1)-1)) {
    for(j in (i+1):(length(y1))) {
      w <- rep(0,length(y))
      w[i] <- -1
      w[j] <- 1
      u <- rbind(u,w)
      v1 <- append(v1,2,after=length(v1))
      v2 <- append(v2,space,after=length(v2))
    }
  }
  
  for(i in (1:length(y2))) {
    y2index <- which(y2 > y2[i])
    for(j in y2index) {
      w <- rep(0,length(y))
      w[(i+(length(y)/2))] <- -1
      w[(j+(length(y)/2))] <- 1
      u <- rbind(u,w)
      v1 <- append(v1,2,after=length(v1))
      v2 <- append(v2,space,after=length(v2))
    }
  }
  umod <- u[2:dim(u)[1],]
  v1mod <- v1[2:length(v1)]
  v2mod <- v2[2:length(v2)]
  
  return(unname(cbind(umod,v1mod,v2mod)))
}

## Third set of constraints:  Except for the endpoints, every number is between two
## other numbers.  The in-between number's positioning should be closer to whichever 
## of the two numbers is closer.  I.e., if 37.5 is between 39.0 and 35.2, it should
## be positioned closer to 39.0, because 39.0 - 37.5 < 37.5 - 35.2.  This constraint 
## is for the first column.
ineq2 <- function(y,x1=x1.1,x2=x2.1) {
  
  y1 <- y[(length(y)/2+1):length(y)]
  
  index <- which(x1 %in% subset(x1, min(x1) < x1 & max(x1) > x1))
  
  u <- rep(0,length(y))
  v1 <- 0
  v2 <- 0 
  
  for(i in index) {
    lowerpts <- which(x1 < x1[i])
    higherpts <- which(x1 > x1[i])
    lowernabrs <- which(x1 == max(x1[lowerpts]))
    highernabrs <- which(x1 == min(x1[higherpts]))
    
    for(j in lowernabrs) {
      for(h in highernabrs) {
        if(x1[i] - x1[j] > x1[h] - x1[i]) {
          w <- rep(0,(length(y)/2))
          w[j] <- 1
          w[h] <- 1
          w[i] <- -2
          w1 <- c(rep(0,(length(y)/2)),w)
          u <- rbind(u,w1)
          v1 <- append(v1,2,after=length(v1))
          v2 <- append(v2,0,after=length(v2)) 
        }
        if(x1[i] - x1[j] < x1[h] - x1[i]) {
          w <- rep(0,(length(y)/2))
          w[j] <- -1
          w[h] <- -1
          w[i] <- 2
          w1 <- c(rep(0,(length(y)/2)),w)
          u <- rbind(u,w1)
          v1 <- append(v1,2,after=length(v1))
          v2 <- append(v2,0,after=length(v2)) 
        }
      }
    }
  }
  umod <- u[2:dim(u)[1],]
  v1mod <- v1[2:length(v1)]
  v2mod <- v2[2:length(v2)]
  
  return(unname(cbind(umod,v1mod,v2mod)))
}

## Fourth set of constraints:  This is the same as the third constraint, but it's for
## the second column.
ineq3 <- function(y,x1=x1.1,x2=x2.1) {
  
  y2 <- y[(length(y)/2+1):length(y)]
  
  index <- which(x2 %in% subset(x2, min(x2) < x2 & max(x2) > x2))
  
  u <- rep(0,length(y))
  v1 <- 0
  v2 <- 0 
  
  for(i in index) {
    lowerpts <- which(x2 < x2[i])
    higherpts <- which(x2 > x2[i])
    lowernabrs <- which(x2 == max(x2[lowerpts]))
    highernabrs <- which(x2 == min(x2[higherpts]))
    
    for(j in lowernabrs) {
      for(h in highernabrs) {
        if(x2[i] - x2[j] > x2[h] - x2[i]) {
          w <- rep(0,(length(y)/2))
          w[j] <- 1
          w[h] <- 1
          w[i] <- -2
          w1 <- c(rep(0,(length(y)/2)),w)
          u <- rbind(u,w1)
          v1 <- append(v1,2,after=length(v1))
          v2 <- append(v2,0,after=length(v2)) 
        }
        if(x2[i] - x2[j] < x2[h] - x2[i]) {
          w <- rep(0,(length(y)/2))
          w[j] <- -1
          w[h] <- -1
          w[i] <- 2
          w1 <- c(rep(0,(length(y)/2)),w)
          u <- rbind(u,w1)
          v1 <- append(v1,2,after=length(v1))
          v2 <- append(v2,0,after=length(v2)) 
        }
      }
    }
  }
  umod <- u[2:dim(u)[1],]
  v1mod <- v1[2:length(v1)]
  v2mod <- v2[2:length(v2)]
  
  return(unname(cbind(umod,v1mod,v2mod)))
}
## Constraints for first wave of optimization:
ineq4 <- function(y) {
  
  y1 <- y[1:(length(y)/2)]
  y2 <- y[(length(y)/2+1):length(y)]
  
  u <- rep(0,length(y))
  v1 <- 0
  v2 <- 0
  
  for(i in 1:length(y1)) {
    w <- rep(0, length(y))
    w[i] <- 1
    u <- rbind(u,w)
    v1 <- append(v1,3,after=length(v1))
    v2 <- append(v2,tol,after=length(v2)) 
  }
  
  for(i in (1:length(y2))) {
    w <- rep(0, length(y))
    w[(i+(length(y)/2))] <- 1
    u <- rbind(u,w)
    v1 <- append(v1,3,after=length(v1))
    v2 <- append(v2,tol,after=length(v2)) 
  }
  umod <- u[2:dim(u)[1],]
  v1mod <- v1[2:length(v1)]
  v2mod <- v2[2:length(v2)]
  
  return(unname(cbind(umod,v1mod,v2mod)))
}

inequalitymaker1 <- function(y){
  
  iq <- rep(0,(length(y)+2))
  
  iq <- rbind(iq,ineq0(y))
  iq <- rbind(iq,ineq4(y))
  iq <- rbind(iq,ineq2(y))
  iq <- rbind(iq,ineq3(y))
  iq <- iq[2:length(iq[,1]),]
  
  dir1 <- iq[,(length(y)+1)]
  dir1[which(dir1 == 1)] <- ">="
  dir1[which(dir1 == 2)] <- "<="
  
  rhs1 <- iq[,(length(y)+2)]
  
  iq <- iq[,1:length(y)]
  
  return(L_constraint(L=iq, dir=dir1, rhs=rhs1))
  
}

##################################  SOLVING1  #########################################
fo1 <-  F_objective(F=fn3,n=10,G=gr3)

lc1 <- inequalitymaker1(y)

prob1 <- OP(fo1,lc1)
sol1 <- ROI_solve(prob1,solver="alabama",start=y)

points0 <- solution(sol1)
points0[4]-points0[3]
































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


####################################################
x1 <- x1.1
x2 <- x2.1

y1 <- y[1:(length(y)/2)]
y2 <- y[(length(y)/2+1):length(y)]

index1 <- which(x1 %in% x1[duplicated(x1)])
index2 <- which(x2 %in% x2[duplicated(x2)])

#if(length(index1) > 0)
  
  
y1.1 <- y1[index1]
y2.1 <- y2[index1]
y1.1tab <- table(y1.1)
tiedpts1 <- as.numeric(names(y1.1tab))

y1.2 <- y1[index2]
y2.2 <- y2[index2]
y2.2tab <- table(y2.2)
tiedpts2 <- as.numeric(names(y2.2tab))

y1

fn2 <- function(v){
  return((v[1]-105.6)^2+(v[2]-141.6)^2-(v[3]-v[4])^2+(v[5]-204])^2+(v[6]-0)^2
         +(v[7]-12)^2+(v[8]-4.8)^2+(v[9]-182.4)^2+(v[10]-124.8)^2)
}

gr2 <- function(v){
  return(c((2*v[1]-2*105.6),(2*v[2]-2*141.6),-(2*v[3]-2*v[4]),-(2*v[4]-2*v[3]),(2*v[5]-2*204),
           (2*v[6]-0),(2*v[7]-2*12),(2*v[8]-2*4.8),(2*v[9]-2*182.4),(2*v[10]-2*124.8)))
}

x2






tiedpoints1 <- which(x1 %in% x1[duplicated(x1)])
tiedpoints2 <- which(x2 %in% x2[duplicated(x2)])

ineq3 <- function(y,x1=x1.1,x2=x2.1, tol=tol0, colmn=colmn0, etol=etol0) {
  
  if(colmn == 1){
    y3 <- y[1:(length(y)/2)]
    x3 <- x1
    tiedpoints3 <- tiedpoints1
  }
  if(colmn == 2){
    y3 <- y[(length(y)/2+1):length(y)]
    x3 <- x2
    tiedpoints3 <- tiedpoints2
  }
  
  index <- which(x3 %in% subset(x3, min(x3) < x3 & max(x3) > x3))
  
  u <- rep(0,length(y)/2)
  w <- rep(0,(length(y)/2))
  v1 <- 0
  v2 <- 0 
  
  for(m in index) {
    lowerpts <- which(x3 < x3[m])
    higherpts <- which(x3 > x3[m])
    l <- min(which(x3 == max(x3[lowerpts])))
    h <- max(which(x3 == min(x3[higherpts])))
    if(m %in% tiedpoints3) {
      if(l %in% tiedpoints3 && !(h %in% tiedpoints3)) {
        w[l] <- 0
        w[h] <- 1
        w[m] <- 0
        if(x3[h] - x3[m] == x3[m] - x3[l]) {
          u <- rbind(u,w,w)
          v1 <- append(v1,c(1,2),after=length(v1))
          v2 <- append(v2,c(2*y3[m]-y3[l]+tol-etol,2*y3[m]-y3[l]+tol+etol),after=length(v2)) 
        } else if(x3[h] - x3[m] < x3[m] - x3[l]) {
          u <- rbind(u,w)
          v1 <- append(v1,1,after=length(v1))
          v2 <- append(v2,2*y3[m]-y3[l]+tol,after=length(v2))
        } else {
          u <- rbind(u,w)
          v1 <- append(v1,2,after=length(v1))
          v2 <- append(v2,2*y3[m]-y3[l]+tol,after=length(v2))
        }
      } else if(h %in% tiedpoints3 && !(l %in% tiedpoints3)) {
        w[l] <- 1
        w[h] <- 0
        w[m] <- 0
        if(x3[h] - x3[m] == x3[m] - x3[l]) {
          u <- rbind(u,w,w)
          v1 <- append(v1,c(1,2),after=length(v1))
          v2 <- append(v2,c(2*y3[m]-y3[h]-tol-etol,2*y3[m]-y3[h]-tol+etol),after=length(v2))
        } else if(x3[h] - x3[m] < x3[m] - x3[l]) {
          u <- rbind(u,w)
          v1 <- append(v1,1,after=length(v1))
          v2 <- append(v2,2*y3[m]-y3[h]-tol,after=length(v2))
        } else {
          u <- rbind(u,w)
          v1 <- append(v1,2,after=length(v1))
          v2 <- append(v2,2*y3[m]-y3[h]-tol,after=length(v2))
        }
      } else if(!(h %in% tiedpoints3) && !(l %in% tiedpoints3)) {
        w[l] <- 1
        w[h] <- 1
        w[m] <- 0
        if(x3[h] - x3[m] == x3[m] - x3[l]) {
          u <- rbind(u,w,w)
          v1 <- append(v1,c(1,2),after=length(v1))
          v2 <- append(v2,c(2*y3[m]-etol,2*y3[m]+etol),after=length(v2))
        } else if(x3[h] - x3[m] < x3[m] - x3[l]) {
          u <- rbind(u,w)
          v1 <- append(v1,1,after=length(v1))
          v2 <- append(v2,2*y3[m],after=length(v2))
        } else {
          u <- rbind(u,w)
          v1 <- append(v1,2,after=length(v1))
          v2 <- append(v2,2*y3[m],after=length(v2))
        }
      }
    } else {
      if(l %in% tiedpoints3) {
        if(h %in% tiedpoints3) {
          w[l] <- 0
          w[h] <- 0
          w[m] <- 2
          if(x3[h] - x3[m] == x3[m] - x3[l]) {
            u <- rbind(u,w,w)
            v1 <- append(v1,c(1,2),after=length(v1))
            v2 <- append(v2,c(y3[h]+y3[l]-etol,y3[h]+y3[l]+etol),after=length(v2)) 
          } else if(x3[h] - x3[m] < x3[m] - x3[l]) {
            u <- rbind(u,w)
            v1 <- append(v1,2,after=length(v1))
            v2 <- append(v2,y3[h]+y3[l],after=length(v2))
          } else {
            u <- rbind(u,w)
            v1 <- append(v1,1,after=length(v1))
            v2 <- append(v2,y3[h]+y3[l],after=length(v2))
          }
        } else {
          w[l] <- 0
          w[h] <- -1
          w[m] <- 2
          if(x3[h] - x3[m] == x3[m] - x3[l]) {
            u <- rbind(u,w,w)
            v1 <- append(v1,c(1,2),after=length(v1))
            v2 <- append(v2,c(y3[l]-tol-etol,y3[l]-tol+etol),after=length(v2)) 
          } else if(x3[h] - x3[m] < x3[m] - x3[l]) {
            u <- rbind(u,w)
            v1 <- append(v1,2,after=length(v1))
            v2 <- append(v2,y3[l]-tol,after=length(v2)) 
          } else {
            u <- rbind(u,w)
            v1 <- append(v1,1,after=length(v1))
            v2 <- append(v2,y3[l]-tol,after=length(v2)) 
          }
        }
      } else if(h %in% tiedpoints3) {
        w[l] <- -1
        w[h] <- 0
        w[m] <- 2
        if(x3[h] - x3[m] == x3[m] - x3[l]) {
          u <- rbind(u,w,w)
          v1 <- append(v1,c(1,2),after=length(v1))
          v2 <- append(v2,c(y3[h]+tol-etol,y3[h]+tol+etol),after=length(v2))
        } else if(x3[h] - x3[m] < x3[m] - x3[l]) {
          u <- rbind(u,w)
          v1 <- append(v1,2,after=length(v1))
          v2 <- append(v2,y3[h]+tol,after=length(v2))
        } else {
          u <- rbind(u,w)
          v1 <- append(v1,1,after=length(v1))
          v2 <- append(v2,y3[h]+tol,after=length(v2))
        }
      } else {
        w[l] <- -1
        w[h] <- -1
        w[m] <- 2
        if(x3[h] - x3[m] == x3[m] - x3[l]) {
          u <- rbind(u,w,w)
          v1 <- append(v1,c(1,2),after=length(v1))
          v2 <- append(v2,c(-etol,etol),after=length(v2))
        } else if(x3[h] - x3[m] < x3[m] - x3[l]) {
          u <- rbind(u,w)
          v1 <- append(v1,2,after=length(v1))
          v2 <- append(v2,0,after=length(v2)) 
        } else {
          u <- rbind(u,w)
          v1 <- append(v1,1,after=length(v1))
          v2 <- append(v2,0,after=length(v2)) 
        }
      }
    }
  }
  umod <- u[2:dim(u)[1],]
  v1mod <- v1[2:length(v1)]
  v2mod <- v2[2:length(v2)]
  
  return(unname(cbind(umod,v1mod,v2mod)))
}
