## Get the data
tufte2 <- read.csv("https://raw.githubusercontent.com/ZRVc/Tufte-Style-Slopegraphs-in-D3-/42095cdaf56e02762fbb785fea86c9d1b0fadd3d/TufteGovernment.csv")

## The package "Rsolnp" needs to be installed.
## install.packages("Rsolnp")

x <- as.numeric(c(unlist(tufte2[,2]),(unlist(tufte2[,3]))))

x1 <- x[1:(length(x)/2)]
x2 <- x[((length(x)/2)+1):length(x)]

## space sets the linespacing.  It is the minimum distance between labels.
## drop controls the slope.
## The relationship between the spacing coefficient and the drop coefficient seems
## to be very important.  E.g., I get a good solution with space=18 and drop=36, but
## not space=18 and drop=32.

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

start <- c(space*y1,space*y2)

###################################################
## In this part, the functions used for RSolnp are being declared.

## This is the function I'm trying to optimize.

fn <- function(y,x1,x2,space,z){
  return(sum((z - y)^2))
}

## This will set the equality constraints.  I want them all to have the same slope.

eq1 <- function(y,x1,x2,space,z){
  y1 <- y[1:(length(y)/2)]
  y2 <- y[(length(y)/2+1):length(y)]

  z <- 0
  for(i in 1:(length(y1)-1)) {
    w <- ((y1[i]-y2[i])/(x1[i]-x2[i])) - ((y1[(i+1)]-y2[(i+1)])/(x1[(i+1)]-x2[(i+1)]))
    z <- append(z, w, after=length(z))
  }
  return(z[2:length(z)])
}

## This will set the inequality constraints.  I want the percentages to be in order for
## each column (it is assumed that the first column is already in order).  I want the 
## minimum distance between points to be 18.  If possible, each point should be
## closest to the point whose x-value is closest.

ineq1 <- function(y,x1,x2,space,z) {
  y1 <- y[1:(length(y)/2)]
  z1 <- 0
  for(i in 1:(length(y1)-1)) {
    for(j in (i+1):(length(y1))) {
      w <- -y1[i] + y1[j]
      z1 <- append(z1,w,after = length(z1))
    }
    # if(x1[i] > min(x1) && x1[i] < max(x1)){             ## This loop sets each point's y1 value
    #   upper <- min(y1[which(x1 < x1[i])])               ## closer to one with the closer x1 value
    #   lower <- max(y1[which(x1 > x1[i])])               ## it wasn't producing a solution, so
    #   y1low <- which(y1 == lower)                       ## got commented out.
    #   y1high <- which(y1 == upper)
    #   for(k in y1low){
    #     for(m in y1high){
    #       if(x1[i] - x1[m] > x1[k] - x1[i]){
    #         v <- y1[m] + y1[k] - 2*y1[i]
    #         z1 <- append(z1,v,after = length(z1))
    #       }
    #       if(x1[i] - x1[m] < x1[k] - x1[i]){
    #         v <- 2*y1[i] - y1[m] - y1[k]
    #         z1 <- append(z1,v,after = length(z1))
    #       }
    #     }
    #     }
    #   }
  }
  
  y2 <- y[(length(y)/2+1):length(y)]
  z2 <- 0
  for(i in (1:length(y2))) {
    y2index <- which(y2 > y2[i])
    for(j in y2index) {
      w <- -y2[i] + y2[j]
      z2 <- append(z2,w,after = length(z2))
    }
    if(x2[i] > min(x2) && x2[i] < max(x2)){
      upper <- min(y2[which(x2 < x2[i])])
      lower <- max(y2[which(x2 > x2[i])])
      y2low <- which(y2 == lower)
      y2high <- which(y2 == upper)
      for(k in y2low){
        for(m in y2high){
          if(x2[i] - x2[m] > x2[k] - x2[i]){
            v <- y2[m] + y2[k] - 2*y2[i]
            z2 <- append(z2,v,after = length(z2))
          }
          if(x2[i] - x2[m] < x2[k] - x2[i]){
            v <- 2*y2[i] - y2[m] - y2[k]
            z2 <- append(z2,v,after = length(z2))
          }
        }
      }
    }
  }
  return(c(z1[2:length(z1)],z2[2:length(z2)]))
}  


## This function is the same as the last (ineq1), but the values it returns are the lower bounds I want
bounder <- function(y,x1,x2,space,z) {
  y1 <- y[1:(length(y)/2)]
  z1 <- 0
  for(i in 1:(length(y1)-1)) {
    for(j in (i+1):(length(y1))) {
      w <- space
      z1 <- append(z1,w,after = length(z1))
    }
    # if(x1[i] > min(x1) && x1[i] < max(x1)){
    #   upper <- min(y1[which(x1 < x1[i])])
    #   lower <- max(y1[which(x1 > x1[i])])
    #   y1low <- which(y1 == lower) 
    #   y1high <- which(y1 == upper)
    #   for(k in y1low){
    #     for(m in y1high){
    #       if(x1[i] - x1[m] > x1[k] - x1[i]){
    #         v <- 0.1
    #         z1 <- append(z1,v,after = length(z1))
    #       }
    #       if(x1[i] - x1[m] < x1[k] - x1[i]){
    #         v <- 0.1
    #         z1 <- append(z1,v,after = length(z1))
    #       }
    #     }
    #   }
    # }
  }
  
  y2 <- y[(length(y)/2+1):length(y)]
  z2 <- 0
  for(i in (1:length(y2))) {
    y2index <- which(y2 > y2[i])
    for(j in y2index) {
      w <- space
      z2 <- append(z2,w,after = length(z2))
    }
    if(x2[i] > min(x2) && x2[i] < max(x2)){
      upper <- min(y2[which(x2 < x2[i])])
      lower <- max(y2[which(x2 > x2[i])])
      y2low <- which(y2 == lower)
      y2high <- which(y2 == upper)
      for(k in y2low){
        for(m in y2high){
          if(x2[i] - x2[m] > x2[k] - x2[i]){
            v <- 0.1   ## The solution is sensitive to this number
            z2 <- append(z2,v,after = length(z2))
          }
          if(x2[i] - x2[m] < x2[k] - x2[i]){
            v <- 0.1
            z2 <- append(z2,v,after = length(z2))
          }
        }
      }
    }
  }
  return(c(z1[2:length(z1)],z2[2:length(z2)]))
}  

###########################################################
## Using Rsolnp to set the points:
library(Rsolnp)

z <- c(y1,y2)

s1 <- eq1(start,x1=x1,x2=x2)
s <- ineq1(start,x1=x1,x2=x2)

lbounds <- bounder(start,x1=x1,x2=x2,space=space)

sol <- solnp(start,fun=fn,eqfun=eq1,eqB=rep((10^-6),length(s1)),
             ineqfun=ineq1,ineqLB=lbounds,ineqUB=rep(30000,length(lbounds)),
             z=z,space=space,x1=x1,x2=x2)
round(sol$pars-min(sol$pars),1)

## Check the slopes:
y1Solve <- round(sol$pars-min(sol$pars),1)[1:(length(sol$pars)/2)]
y2Solve <- round(sol$pars-min(sol$pars),1)[(length(sol$pars)/2+1):length(sol$pars)]

(y2Solve-y1Solve)/(x2-x1)