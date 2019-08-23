# Get the data
tufte2 <- read.csv("https://raw.githubusercontent.com/ZRVc/Tufte-Style-Slopegraphs-in-D3-/master/TufteGovernment.csv")

## "space" sets the line spacing. It is the minimum distance between labels.
## "drop" controls the slope.

space <- 16
drop <- 24

## Set the constraints:  
## 1: Slope Contraint
## 2: Spacing Contraint (Not Optional)
## 3: Perspective Contraint for Column 1
## 4: Perspective Contraint for Column 2
constr <- c(1,2,4)

## Decide whether or not the order should be preserved (each column)
pres_ord <- c(TRUE,TRUE)

## Set the tolerance for the slopes: 
slopetol <- 0.00000

## Set the tolerance for deviation from true equality in the perspective
## constraints. I haven't done much with this, so I don't have much to
## say about it.
equalitytol <- 0.001

## The package "ROI" needs to be installed.
# install.packages("ROI")
# install.packages("numDeriv")
# install.packages("alabama")
# install.packages("ROI.plugin.alabama")
# install.packages("nloptr")
# install.packages("ROI.plugin.neos")

library("ROI")
library("ROI.plugin.alabama")
library("ROI.plugin.neos")

x <- c(tufte2[,2],tufte2[,3])

x <- list(x[1:(length(x)/2)], x[((length(x)/2)+1):length(x)])

## Finding the ties
tiedpoints1 <- which(x[[1]] %in% x[[1]][duplicated(x[[1]])])
tiedpoints2 <- which(x[[2]] %in% x[[2]][duplicated(x[[2]])])
tiedpoints <- list(tiedpoints1, tiedpoints2)

## This will set the slope so that a one unit decrease in x
## corresponds to a "drop" pixel drop on the page.
y1 <- drop*(max(c(x[[1]],x[[2]])) - x[[1]])
y2 <- (y1 - drop*(x[[2]]-x[[1]]))

## This is the starting point
y_start <- c(y1,y2)

## Find the minimum non-zero difference between points
min_diff_finder <- function(y,x0=x) {
  
  y <- list(y[1:(length(y)/2)], y[(length(y)/2+1):length(y)])
  diff <- Inf
  
  for(c in 1:2){
    index <- which(!duplicated(x0[[c]]))
    for(i in 1:(length(index)-1)) {
      for(j in (i+1):length(index)) {
        if (abs(y[[c]][index[i]]-y[[c]][index[j]]) < diff) {
          diff <- abs(y[[c]][index[i]]-y[[c]][index[j]])
        }
      }
    }
  }
  return(diff)
}

## Find the minimum difference between points
min_diff_finder2 <- function(y) {
  
  y <- list(y[1:(length(y)/2)], y[(length(y)/2+1):length(y)])
  
  diff <- Inf
  
  for(c in 1:2) {
    for(i in 1:(length(y[[c]])-1)) {
      for(j in (i+1):length(y[[c]])) {
        if (abs(y[[c]][i]-y[[c]][j]) < diff) {
          diff <- abs(y[[c]][i]-y[[c]][j])
        }
      }
    }
  } 
  return(diff)
}

## Find the minimum non-zero distance points can be moved without altering perspective
min_persp_change_finder <- function(y, x0=x){
  y <- list(y[1:(length(y)/2)], y[(length(y)/2+1):length(y)])
  
  mins <- c(0,0)
  
  for(c in 1:2) {
    index0 <- which(!duplicated(x0[[c]]))
    index <- order(x0[[c]][index0],decreasing = TRUE)
    
    x_h <- x0[[c]][index][1:(length(index)-2)]
    x_m <- x0[[c]][index][2:(length(index)-1)]
    x_l <- x0[[c]][index][3:(length(index))]
    
    y_h <- y[[c]][index][1:(length(index)-2)]
    y_m <- y[[c]][index][2:(length(index)-1)]
    y_l <- y[[c]][index][3:(length(index))]
    
    if(length(which(abs(2*x_m-x_h-x_l) > 0)) > 0) {
    mins[c] <- min(abs(2*y_m-y_h-y_l)[which(abs(2*x_m-x_h-x_l) > 0)])
    }
  }
  if(sum(mins) > 0) {
    return(min(mins))
  } else {
    return(Inf)
  }
}

## "spreadtol" is the 1/2 the height of the box in which we enclose the
## tied points when breaking them up.
spreadtol <- 0.5*min(min_diff_finder(y_start),min_persp_change_finder(y_start))

########################################################################## Objective Functions
## These are old objective functions I no longer use.  I keep them around for testing the 
## performance of solvers.  I also use one to check the quality of the final soultion.
fn <- function(v,z){
  return(sum((z-v)^2))
}
gr <- function(v,z){
  return(2*v-2*z)
}

## Wrapper functions to work with ROI
fnwr <- function(y) {
  return(fn(y,z=y_start))
}
grwr <- function(y) {
  return(gr(y,z=y_start))
}

### First objective function.  This breaks apart points that are tied.
beginfn <- function(v,x0=x,z=y_start) {
  
  w <- c(0,0)
  
  for(c in 1:2) {
    if(length(tiedpoints[[c]]) > 0) {
      tieloc <- unique(x0[[c]][duplicated(x0[[c]])])
      tiegroup <- 0
      for(n in tieloc) {
        tiegroup <- c(tiegroup,list(which(x0[[c]] == n)))
      }
      
      tiegroup <- tiegroup[2:length(tiegroup)]
      
      for(i in 1:length(tieloc)) {
        for(j in 1:(length(tiegroup[[i]])-1)) {
          for (k in 2:length(tiegroup[[i]])) {
            w[c] <- w[c] + (v[tiegroup[[i]][j]]-v[tiegroup[[i]][k]])^2
          }
        }
      }
    }
  }
  return(-sum(w))
}

## First gradient
begingr <- function(v,z=y_start,x0=x,tiedpoints0=tiedpoints) {
  w <- rep(0,length(v))
  
  for(c in 1:2) {
    if(length(tiedpoints[[c]]) > 0) {
      tieloc <- unique(x0[[c]][duplicated(x0[[c]])])
      tiegroup <- 0
      for(n in tieloc) {
        tiegroup <- c(tiegroup,list(which(x0[[c]] == n)))
      }
      tiegroup <- tiegroup[2:length(tiegroup)]
      
      for(i in 1:length(tieloc)) {
        for(j in tiegroup[[i]]) {
          index <- tiegroup[[i]][which(tiegroup[[i]] != j)]
          w[j] <- 2*sum(v[index])-2*(length(tiegroup[[i]])-1)*v[j]
        }
      }
    }
  }
  return(w)
}

## Wrappers
beginfnwr <- function(y) {
  return(beginfn(y,z=y_start))
}

begingrwr <- function(y) {
  return(begingr(y,z=y_start))
}

###################################################################################### Constraints
## Slope constraints
slope_ineq <- function(y,x0=x,s_tol=slopetol) {
  
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


## Initial perspective constraints
ini_persp_ineq <- function(y,x0=x, tol=spreadtol, colmn, etol=equalitytol,tiedpoints0 = tiedpoints) {
  
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

## Final perspective constraints
fin_persp_ineq <- function(y,x0=x, colmn, tiedpoints0 = tiedpoints) {
  
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
  
  index <- which(x3 %in% subset(x3, min(x3) < x3 & max(x3) > x3 & !(x3 %in% x3[tiedpoints3])))
  
  u <- rep(0,length(y)/2)
  v1 <- 0
  v2 <- 0 
  
  if(length(tiedpoints3) > 0) {
    tieloc <- unique(x3[duplicated(x3)])
    tiegroup <- 0
    for(i in tieloc) {
      tiegroup <- c(tiegroup,list(which(x3 == i)))
    }
    tiegroup <- tiegroup[2:length(tiegroup)]
  }
  
  for(m in index) {
    w <- rep(0,(length(y)/2))
    lowerpts <- which(x3 < x3[m])
    higherpts <- which(x3 > x3[m])
    l <- min(which(x3 == max(x3[lowerpts])))
    h <- max(which(x3 == min(x3[higherpts])))
    
    if(!(h %in% tiedpoints3) && !(l %in% tiedpoints3)) {
      w[l] <- -1
      w[h] <- -1
      w[m] <- 2
      
      if(x3[h] - x3[m] < x3[m] - x3[l]) {
        u <- rbind(u,w)
        v1 <- append(v1,2,after=length(v1))
        v2 <- append(v2,0,after=length(v2)) 
      }
      else if(x3[h] - x3[m] > x3[m] - x3[l]) {
        u <- rbind(u,w)
        v1 <- append(v1,1,after=length(v1))
        v2 <- append(v2,0,after=length(v2))
      }
      
    } else if(h %in% tiedpoints3 && !(l %in% tiedpoints3)) {
      
      hightie <- which(tieloc==x3[h])
      tielenh <- length(tiegroup[[hightie]])
      
      w[l] <- -1
      for(k in tiegroup[[hightie]]) {
        w[k] <- -1/tielenh
      }
      w[m] <- 2
      
      if(x3[h] - x3[m] < x3[m] - x3[l]) {
        u <- rbind(u,w)
        v1 <- append(v1,2,after=length(v1))
        v2 <- append(v2,space*(tielenh-1)/2,after=length(v2)) 
      }
      else if(x3[h] - x3[m] > x3[m] - x3[l]) {
        u <- rbind(u,w)
        v1 <- append(v1,1,after=length(v1))
        v2 <- append(v2,space*(tielenh-1)/2,after=length(v2))
      }
    } else if (l %in% tiedpoints3 && !(h %in% tiedpoints3)) {
      
      lowtie <- which(tieloc==x3[l])
      tielenl <- length(tiegroup[[lowtie]])
      
      for(k in tiegroup[[lowtie]]) {
        w[k] <- 1/tielenl
      }
      w[h] <- 1
      w[m] <- -2
      
      if(x3[h] - x3[m] < x3[m] - x3[l]) {
        u <- rbind(u,w)
        v1 <- append(v1,1,after=length(v1))
        v2 <- append(v2,space*(tielenl-1)/2,after=length(v2)) 
      }
      else if(x3[h] - x3[m] > x3[m] - x3[l]) {
        u <- rbind(u,w)
        v1 <- append(v1,2,after=length(v1))
        v2 <- append(v2,space*(tielenl-1)/2,after=length(v2))
      }
    } else {
      
      lowtie <- which(tieloc==x3[l])
      tielenl <- length(tiegroup[[lowtie]])
      
      hightie <- which(tieloc==x3[h])
      tielenh <- length(tiegroup[[hightie]])
      
      for(k in tiegroup[[lowtie]]) {
        w[k] <- -1/tielenl
      }
      for(n in tiegroup[[hightie]]) {
        w[n] <- -1/tielenh
      }
      w[m] <- 2
      
      if(x3[h] - x3[m] < x3[m] - x3[l]) {
        u <- rbind(u,w)
        v1 <- append(v1,2,after=length(v1))
        v2 <- append(v2,space*((tielenh-1)-(tielenl-1))/2,after=length(v2)) 
      }
      else if(x3[h] - x3[m] > x3[m] - x3[l]) {
        u <- rbind(u,w)
        v1 <- append(v1,1,after=length(v1))
        v2 <- append(v2,space*((tielenh-1)-(tielenl-1))/2,after=length(v2))
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

## Linear separating constraints
ini_sep_ineqL <- function(y, x0=x, tol2=spreadtol, colmn, tiedpoints0=tiedpoints) {
  
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
  
  if(length(tiedpoints3) > 0) {
    tieloc <- unique(x3[duplicated(x3)])
    tiegroup <- 0
    
    for(i in tieloc) {
      tiegroup <- c(tiegroup,list(which(x3 == i)))
    }
    tiegroup <- tiegroup[2:length(tiegroup)]
  }
  
  minnum <- length(which(x3==min(x3)))
  
  for(i in order(x3,decreasing=T)[1:length(x3)]) {
    if(i %in% tiedpoints3) {
      w <- rep(0,length(y3))
      w[i] <- 1
      u <- rbind(u,w,w)
      v1 <- append(v1,c(1,2),after=length(v1))
      v2 <- append(v2,c(y3[i]-tol2,y3[i]+tol2),after=length(v2))
      
      tie <- which(tieloc==x3[i])
      tielen <- length(tiegroup[[tie]])
      if(i < max(tiegroup[[tie]])) {
        for(j in tiegroup[[tie]][(which(tiegroup[[tie]] > i))]) {
          w <- rep(0,length(y3))
          w[i] <- -1
          w[j] <- 1
          u <- rbind(u,w)
          v1 <- append(v1,1,after=length(v1))
          v2 <- append(v2,0,after=length(v2))
        }
      }
    }
  }
  for(i in order(x3,decreasing=T)[1:(length(x3)-minnum)]) {
    index2 <- which(x3 == max(x3[which(x3 < x3[i])]))
    w <- rep(0,length(y3))
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

## Quadratic separating constraints -- not used in this version of the code.  I keep them
## around for testing other solvers.
ini_sep_ineqQ <- function(y, x0=x, tol2=spreadtol, colmn, tiedpoints0=tiedpoints) {
  
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

## Linear spacing constraints
spacing_ineqL <- function(y,x0=x,colmn,space0=space) {
  
  if(colmn == 1){
    y3 <- y[1:(length(y)/2)]
    x3 <- x0[[1]]
  }
  if(colmn == 2){
    y3 <- y[(length(y)/2+1):length(y)]
    x3 <- x0[[2]]
  }
  
  u <- rep(0,length(y3))
  v1 <- 0
  v2 <- 0
  
  mins <- which(x3==min(x3))
  minnum <- length(mins)
  
  for(i in order(x3,decreasing=T)[1:(length(x3)-minnum)]) {
    w <- rep(0,length(y3))
    j <- min(which(x3 == max(x3[which(x3 < x3[i])])))
    
    w[i] <- -1
    w[j] <- 1
    
    u <- rbind(u,w)
    v1 <- append(v1,1,after=length(v1))
    v2 <- append(v2,space0,after=length(v2))
  }
  
  if(minnum > 1) {
  for(i in mins[-length(mins)]){
    w <- rep(0,length(y3))
     w[i] <- -1
     w[(i+1)] <- 1
     
     u <- rbind(u,w)
     v1 <- append(v1,1,after=length(v1))
     v2 <- append(v2,space0,after=length(v2))
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

## Quadratic spacing constraints
spacing_ineqQ <- function(y,x0=x,colmn,tiedpoints0=tiedpoints,space0=space) {
  
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
    index2 <- which(x3 == max(x3[which(x3 < x3[i])]))
    for(j in index2) {
      w <- rep(0,length(y3))
      w[i] <- -1
      w[j] <- 1
      u <- rbind(u,w)
      v1 <- append(v1,1,after=length(v1))
      v2 <- append(v2,space0,after=length(v2))
      q <- c(q,list(NULL))
    }
  }
  for(k in unique(x3[tiedpoints3])) {
    grp <- which(x3 == k)
    for(r in 1:(length(grp)-1)) {
      for(s in (r+1):length(grp)) {
        wq <- matrix(0, nrow=length(y3), ncol=length(y3))
        wq[grp[r],grp[r]] <- 2 
        wq[grp[r],grp[s]] <- -2
        wq[grp[s],grp[s]] <- 2
        wq[grp[s],grp[r]] <- -2
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

inequalitymaker1 <- function(y,pres_ord0 = pres_ord) {
  
  iq <- rep(0,(length(y)+2))
  
  iq <- rbind(iq,slope_ineq(y))
  iq <- rbind(iq,ini_persp_ineq(y,colmn=1))
  iq <- rbind(iq,ini_persp_ineq(y,colmn=2))
  
  for(i in 1:2) {
    if(pres_ord0[i] == FALSE) {
      iq <- rbind(iq,ini_sep_ineqQ(y,colmn=i))
    } else {
      iq <- rbind(iq,ini_sep_ineqL(y,colmn=i))
    }
  }
  
  iq <- iq[2:length(iq[,1]),]
  
  dir1 <- iq[,(length(y)+1)]
  dir1[which(dir1 == 1)] <- ">="
  dir1[which(dir1 == 2)] <- "<="
  
  rhs1 <- iq[,(length(y)+2)]
  
  iq <- iq[,1:length(y)]
  
  return(L_constraint(L=iq, dir=dir1, rhs=rhs1))
}

inequalitymaker2 <- function(y, constr0=constr, pres_ord0=pres_ord) {
  
  iq <- rep(0,(length(y)+2))
  
  if(1 %in% constr0) {
    iq <- rbind(iq,slope_ineq(y))
  }
  if(3 %in% constr0) {
    iq <- rbind(iq,unname(fin_persp_ineq(y,colmn=1)))
  }
  if(4 %in% constr0) {
    iq <- rbind(iq,unname(fin_persp_ineq(y,colmn=2)))
  }
  
  if (sum(pres_ord0) < 2) {
    q <- rep(list(NULL),(length(iq[,1])))

    for(i in 1:2) {
      if(pres_ord0[i] == FALSE) {
        iq <- rbind(iq,unname(spacing_ineqQ(y,colmn=i)[[2]]))
        q <- c(q,unname(spacing_ineqQ(y,colmn=i)[[1]]))
      } else {
        iq <- rbind(iq,unname(spacing_ineqL(y,colmn=i)))
        q <- c(q,rep(list(NULL),length(unname(spacing_ineqL(y,colmn=i))[,1])))
      }
    }
    iq <- iq[2:length(iq[,1]),]
    q <- q[2:length(q)]

    dir1 <- iq[,(length(y)+1)]
    dir1[which(dir1 == 1)] <- ">="
    dir1[which(dir1 == 2)] <- "<="
    
    rhs1 <- iq[,(length(y)+2)]
    
    iq <- iq[,1:length(y)]
    
    if((sum(sapply(q, function(x) length(x))) > 0)) {
      return(Q_constraint(Q=q, L=iq, dir=dir1, rhs=rhs1))
    } else {
      return(L_constraint(L=iq, dir=dir1, rhs=rhs1))
    }
  } else {
    iq <- rbind(iq,unname(spacing_ineqL(y,colmn=1)))
    iq <- rbind(iq,unname(spacing_ineqL(y,colmn=2)))
    
    iq <- iq[2:length(iq[,1]),]
    
    dir1 <- iq[,(length(y)+1)]
    dir1[which(dir1 == 1)] <- ">="
    dir1[which(dir1 == 2)] <- "<="
    
    rhs1 <- iq[,(length(y)+2)]
    
    iq <- iq[,1:length(y)]
    
    return(L_constraint(L=iq, dir=dir1, rhs=rhs1))
  }
}

########################################################## The problem and solution
qo2 <-  Q_objective(2*diag(length(y_start)), -2*y_start)

if(length(tiedpoints[[1]])+length(tiedpoints[[2]]) < 1) {
  scaler <- min_diff_finder2(y_start)
  newstart <- y_start*(space/scaler)
  c2 <- inequalitymaker2(newstart)
  prob2 <- OP(qo2,c2)
  sol2 <- ROI_solve(prob2,solver="alabama", start=newstart)
} else {
  
    ## The first problem
    fo1 <-  F_objective(F=beginfnwr,n=length(y_start),G=begingrwr)
    c1 <- inequalitymaker1(y_start)
    prob1 <- OP(fo1,c1)
    
    ## The first solution
    sol1 <- ROI_solve(prob1,solver="alabama",start=y_start)
    
    newstart <- solution(sol1)
    
    scaler <- min_diff_finder2(newstart)
    newstart <- newstart*(space/scaler)
    
    c2 <- inequalitymaker2(newstart)
    prob2 <- OP(qo2,c2)
    
    if(sum(pres_ord) > 1) {
    sol2 <- ROI_solve(prob2,solver="alabama", start=newstart)
  } else {
    sol2 <- ROI_solve(prob2,solver="neos", method="Couenne")
  }
}
  

points0 <- round(solution(sol2)-min(solution(sol2)),2)

points1 <- points0[1]
for(i in 2:(length(points0)/2)) {
  points1 <- paste(points1,points0[i],sep = ", ")
}

points2 <- points0[(length(points0)/2+1)]
for(i in (length(points0)/2+2):length(points0)) {
  points2 <- paste(points2,points0[i],sep = ", ")
}

## The solution, formatted for pasting into D3.js
points1
points2

## Check the slopes
y1Solve <- round(solution(sol2)-min(solution(sol2)),2)[1:(length(solution(sol2))/2)]
y2Solve <- round(solution(sol2)-min(solution(sol2)),2)[(length(solution(sol2))/2+1):length(solution(sol2))]

(y2Solve-y1Solve)/(x[[2]]-x[[1]])

## Value of objective function at solution
fnwr(solution(sol2))
