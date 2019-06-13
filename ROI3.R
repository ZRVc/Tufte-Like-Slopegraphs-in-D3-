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
equalitytol <- 0.001

## The package "ROI" needs to be installed.
# install.packages("ROI")
# install.packages("alabama")
# install.packages("numDeriv")
# install.packages("ROI.plugin.alabama")

library(ROI)
library(ROI.plugin.alabama)
library("alabama")

x <- as.numeric(c(unlist(tufte2[,2]),(unlist(tufte2[,3]))))

x <- list(x[1:(length(x)/2)], x[((length(x)/2)+1):length(x)])

## Finding the ties
tiedpoints1 <- which(x[[1]] %in% x[[1]][duplicated(x[[1]])])
tiedpoints2 <- which(x[[2]] %in% x[[2]][duplicated(x[[2]])])
tiedpoints <- list(tiedpoints1, tiedpoints2)

## This will set the slope so that a one unit decrease in x
## corresponds to a "drop" pixel drop on the page.
y1 <- drop*(max(c(x[[1]],x[[2]])) - x[[1]])
y2 <- (y1 - drop*(x[[2]]-x[[1]]))

y_start <- c(y1,y2)
##################Objective Functions
## This is the function I'm trying to optimize.
fn <- function(v,z){
  return(sum((z-v)^2))
}
gr <- function(v,z){
  return(2*v-2*z)
}

## Wrapper functions to work with ROI
fn1 <- function(y) {
  return(fn(y,z=y_start))
}
gr1 <- function(y) {
  return(gr(y,z=y_start))
}

## First set of constraints: ensure that the slopes are drawn on the same scale.
ineq0 <- function(y,x0=x,s_tol=slopetol) {
  
  x1 <- x0[[1]]
  x2 <- x0[[2]]
  
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
    v2 <- append(v2,c(-s_tol,s_tol),after=length(v2)) 
  }
  
  if(length(index2) > 0){
    for(i in 1:length(index2)) {
      w2 <- rep(0,length(y))
      w2[index2[i]] <- 1
      w2[(index2[i]+(length(y)/2))] <- -1
      
      u <- rbind(u,w2,w2)
      v1 <- append(v1,c(1,2),after=length(v1))
      v2 <- append(v2,c(-s_tol,s_tol),after=length(v2)) 
    }
  }    
  
  umod <- u[2:dim(u)[1],]
  v1mod <- v1[2:length(v1)]
  v2mod <- v2[2:length(v2)]
  
  return(unname(cbind(umod,v1mod,v2mod)))
}



###Perspective Constraints
ineq3 <- function(y,x0=x, tol=spreadtol, colmn, etol,tiedpoints0 = tiedpoints) {
  
  if(colmn == 1){
    y3 <- y[1:(length(y)/2)]
    x3 <- x0[[1]]
    tiedpoints3 <- tiedpoints0[[1]]
  }
  if(colmn == 2){
    y3 <- y[(length(y)/2+1):length(y)]
    x3 <- x0[[2]]
    tiedpoints3 <- tiedpoints0[[2]]
  }
  
  index <- which(x3 %in% subset(x3, min(x3) < x3 & max(x3) > x3))
  
  u <- rep(0,length(y)/2)
  v1 <- 0
  v2 <- 0 
  
  for(m in index) {
    w <- rep(0,(length(y)/2))
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
  if(colmn == 1){
    umod <- cbind(u[2:dim(u)[1],],matrix(0,nrow=dim(u)[1]-1,ncol=length(y)/2))
  }
  if(colmn == 2){
    umod <- cbind(matrix(0,nrow=dim(u)[1]-1,ncol=length(y)/2),u[2:dim(u)[1],])
  }
  
  v1mod <- v1[2:length(v1)]
  v2mod <- v2[2:length(v2)]
  
  return(unname(cbind(umod,v1mod,v2mod)))
}

##Keep the points in order; keep the tiedpoints within tolerance
################
ineq4 <- function(y, x0=x, tol2=spreadtol, colmn, tiedpoints0=tiedpoints) {
  
  if(colmn == 1){
    y3 <- y[1:(length(y)/2)]
    x3 <- x0[[1]]
    tiedpoints3 <- tiedpoints0[[1]]
  }
  if(colmn == 2){
    y3 <- y[(length(y)/2+1):length(y)]
    x3 <- x0[[2]]
    tiedpoints3 <- tiedpoints0[[2]]
  }
  
  u <- rep(0,length(y3))
  v1 <- 0
  v2 <- 0
  
  minnum <- length(which(x3==min(x3)))
  
  for(i in order(x3,decreasing=T)[1:(length(x3)-minnum)]) {
    w <- rep(0,length(y3))
    if(i %in% tiedpoints3) {
      w[i] <- 1
      u <- rbind(u,w,w)
      v1 <- append(v1,c(1,2),after=length(v1))
      v2 <- append(v2,c(y3[i]-tol2,y3[i]+tol2),after=length(v2))
    } 
    index2 <- which(x3 == max(x3[which(x3 < x3[i])]))
    if(length(index2) > 1) {
      w[i] <- 1
      u <- rbind(u,w)
      v1 <- append(v1,2,after=length(v1))
      v2 <- append(v2,y3[index2[1]]-tol2,after=length(v2))
    } else {
      w[i] <- -1
      w[index2] <- 1
      u <- rbind(u,w)
      v1 <- append(v1,1,after=length(v1))
      v2 <- append(v2,0,after=length(v2))
    }
  }
  if(colmn == 1){
    umod <- cbind(u[2:dim(u)[1],],matrix(0,nrow=dim(u)[1]-1,ncol=length(y)/2))
  }
  if(colmn == 2){
    umod <- cbind(matrix(0,nrow=dim(u)[1]-1,ncol=length(y)/2),u[2:dim(u)[1],])
  }
  
  v1mod <- v1[2:length(v1)]
  v2mod <- v2[2:length(v2)]
  
  return(unname(cbind(umod,v1mod,v2mod)))
}


## Making the first set of constraints
inequalitymaker1 <- function(y) {
  
  iq <- rep(0,(length(y)+2))
  
  iq <- rbind(iq,ineq0(y))
  iq <- rbind(iq,ineq3(y,colmn=1))
  iq <- rbind(iq,ineq3(y,colmn=2))
  iq <- rbind(iq,ineq4(y,colmn=1))
  iq <- rbind(iq,ineq4(y,colmn=2))
  iq <- iq[2:length(iq[,1]),]
                                                  
  dir1 <- iq[,(length(y)+1)]
  dir1[which(dir1 == 1)] <- ">="
  dir1[which(dir1 == 2)] <- "<="
                                                  
  rhs1 <- iq[,(length(y)+2)]
                                                  
  iq <- iq[,1:length(y)]
                                                  
  return(L_constraint(L=iq, dir=dir1, rhs=rhs1))
}


### First objective function
beginfn <- function(v,z=y_start) {
  return(sum((z-v)^2)-((z[7]-v[7])^2)-((z[8]-v[8])^2)-(v[7]-v[8])^2)
}

## First gradient
begingr <- function(v,z=y_start) {
  w1 <- 2*v-2*z
  w1[7] <- 2*v[8] - 2*v[7]
  w1[8] <- 2*v[7] - 2*v[8]
  return(w1)
}

## Wrappers
beginfn1 <- function(y) {
  return(beginfn(y,z=y_start))
}

begingr1 <- function(y) {
  return(begingr(y,z=y_start))
}

## The first problem
fo1 <-  F_objective(F=beginfn1,n=30,G=begingr1)
lc1 <- inequalitymaker1(y)
prob1 <- OP(fo1,lc1)

## The first solution
sol1 <- ROI_solve(prob1,solver="alabama",start=y)
newstart <- solution(sol1)
newstart1 <- newstart*3610

#################################Second Wave

ineq8 <- function(y,x0=x,colmn,tiedpoints0=tiedpoints,space0=space) {
  
  if(colmn == 1){
    y3 <- y[1:(length(y)/2)]
    x3 <- x0[[1]]
    tiedpoints3 <- tiedpoints0[[1]]
  }
  if(colmn == 2){
    y3 <- y[(length(y)/2+1):length(y)]
    x3 <- x0[[2]]
    tiedpoints3 <- tiedpoints0[[2]]
  }
  
  u <- rep(0,length(y3))
  v1 <- 0
  v2 <- 0
  q <- 0
  
  minnum <- length(which(x3==min(x3)))
  
  for(i in order(x3,decreasing=T)[1:(length(x3)-minnum)]) {
    w <- rep(0,length(y3))
    if(i %in% tiedpoints3) {
      grp <- which(x3 == x3[i])
      grp <- grp[-which(grp == i)]
      for(j in grp) {
        wq <- matrix(0, nrow=length(y3), ncol=length(y3))
        wq[i,i] <- 2 
        wq[j,j] <- 2
        wq[i,j] <- -2
        wq[j,i] <- -2
        
        u <- rbind(u,rep(0,length(y3)))
        v1 <- append(v1,1,after=length(v1))
        v2 <- append(v2,space0^2,after=length(v2))
        
        if(colmn == 1){
          wq <- cbind(wq,matrix(0,nrow=length(y3),ncol=length(y3)))
          wq <- rbind(wq,matrix(0,nrow=length(y3),ncol=length(y)))
        }
        
        if(colmn == 2){
          wq <- cbind(matrix(0,nrow=length(y3),ncol=length(y3)),wq)
          wq <- rbind(matrix(0,nrow=length(y3),ncol=length(y)),wq)
        }
        q <- c(q,list(wq))
      }
    }
    index2 <- min(which(x3 == max(x3[which(x3 < x3[i])])))
    w[i] <- -1
    w[index2] <- 1
    u <- rbind(u,w)
    v1 <- append(v1,1,after=length(v1))
    v2 <- append(v2,space0,after=length(v2))
    q <- c(q,list(NULL))
  }
  if(colmn == 1){
    umod <- cbind(u[2:dim(u)[1],],matrix(0,nrow=dim(u)[1]-1,ncol=length(y)/2))
  }
  if(colmn == 2){
    umod <- cbind(matrix(0,nrow=dim(u)[1]-1,ncol=length(y)/2),u[2:dim(u)[1],])
  }
  
  v1mod <- v1[2:length(v1)]
  v2mod <- v2[2:length(v2)]
  
  q <- q[2:length(q)]
  
  l <- unname(cbind(umod,v1mod,v2mod))
  return(list(q,l))
}


inequalitymaker2 <- function(y){
  
  iq <- rep(0,(length(y)+2))
  
  iq <- rbind(iq,ineq0(y))
  
  q <- rep(list(NULL),(length(iq[,1])))
  
  iq <- rbind(iq,unname(ineq8(y,colmn=1)[[2]]))
  iq <- rbind(iq,unname(ineq8(y,colmn=2)[[2]]))
  
  q <- c(q,unname(ineq8(y,colmn=1)[[1]]))
  q <- c(q,unname(ineq8(y,colmn=2)[[1]]))
  
  
  iq <- iq[2:length(iq[,1]),]
  q <- q[2:length(q)]
  
  dir1 <- iq[,(length(y)+1)]
  dir1[which(dir1 == 1)] <- ">="
  dir1[which(dir1 == 2)] <- "<="
  
  rhs1 <- iq[,(length(y)+2)]
  
  iq <- iq[,1:length(y)]
  iq
  return(Q_constraint(Q=q, L=iq, dir=dir1, rhs=rhs1))
  
}

fo2 <-  F_objective(F=fn1,n=30,G=gr1)
lc2 <- inequalitymaker2(newstart1)
prob2 <- OP(fo2,lc2)
sol2 <- ROI_solve(prob2,solver="alabama",start=newstart1)

points0 <- round(solution(sol2)-min(solution(sol2)),2)

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

y1Solve <- round(solution(sol2)-min(solution(sol2)),2)[1:(length(solution(sol2))/2)]
y2Solve <- round(solution(sol2)-min(solution(sol2)),2)[(length(solution(sol2))/2+1):length(solution(sol2))]

(y2Solve-y1Solve)/(x[[2]]-x[[1]])
