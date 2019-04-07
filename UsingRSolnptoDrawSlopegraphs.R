tufte2 <- TufteGovernment

vec <- as.numeric(c(unlist(tufte2[,2]),(unlist(tufte2[,3]))))


#ATTEMPT2
x <- vec
#keep in 

x1 <- x[1:(length(x)/2)]
x2 <- x[((length(x)/2)+1):length(x)]
top <- max(x)



#  for(i in 2:length(x1)){
#    y1[i] <- y1[(i+1)]+24*(x1[(i-1)]-x1[i])
#    if(y1[i] - y1[(i-1)] < 18) {
#      y1[i] <- y1[i+1]+18
#    }

y1 <- 24*(top - x1)
y2 <- (y1 - 24*(x2-x1))

for(i in 1:(length(y1)-1)) {
  if(y1[i] == y1[(i+1)]) {
    y1[(i+1)] <- y1[(i+1)]+1
    y2[(i+1)] <- y2[(i+1)]+1
  }
}

start <- c(18*y1,18*y2)

###################################################

fn <- function(y){
  z <- c(252.0, 321.6, 333.6, 400.8, 441.6, 477.6, 532.8, 533.8, 540.0, 648.0,
         650.4, 734.4, 741.6, 837.6, 880.8, 0.0, 38.4, 124.8, 441.6, 336.0, 348.0, 340.8, 519.4, 460.8, 520.8,
         597.6, 643.2, 580.8, 727.2, 739.2)
  return(sum((z - y)^2))
}


eq1 <- function(y){
  y1 <- y[1:(length(y)/2)]
  y2 <- y[(length(y)/2+1):length(y)]
  x1 <- c(46.9, 44.0, 43.5, 40.7, 39.0, 37.5, 35.2, 35.2, 34.9, 30.4, 30.3, 26.8,
          26.5, 22.5, 20.7)
  x2 <- c(57.4, 55.8, 52.2, 39.0, 43.4, 42.9, 43.2, 35.8, 38.2, 35.7, 32.5, 30.6,
          33.2, 27.1, 26.6)
  
  z <- 0
  for(i in 1:(length(y1)-1)) {
    w <- ((y1[i]-y2[i])/(x1[i]-x2[i])) - ((y1[(i+1)]-y2[(i+1)])/(x1[(i+1)]-x2[(i+1)]))
    z <- append(z, w, after=length(z))
  }
  return(z[2:length(z)])
}


ineq1 <- function(y) {
  x1 <- c(46.9, 44.0, 43.5, 40.7, 39.0, 37.5, 35.2, 35.2, 34.9, 30.4, 30.3, 26.8,
          26.5, 22.5, 20.7)
  x2 <- c(57.4, 55.8, 52.2, 39.0, 43.4, 42.9, 43.2, 35.8, 38.2, 35.7, 32.5, 30.6,
          33.2, 27.1, 26.6)
  
  y1 <- y[1:(length(y)/2)]
  z1 <- 0
  for(i in 1:(length(y1)-1)) {
    for(j in (i+1):(length(y1))) {
      w <- -y1[i] + y1[j]
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

bounder <- function(y) {
  x1 <- c(46.9, 44.0, 43.5, 40.7, 39.0, 37.5, 35.2, 35.2, 34.9, 30.4, 30.3, 26.8,
          26.5, 22.5, 20.7)
  x2 <- c(57.4, 55.8, 52.2, 39.0, 43.4, 42.9, 43.2, 35.8, 38.2, 35.7, 32.5, 30.6,
          33.2, 27.1, 26.6)
  
  y1 <- y[1:(length(y)/2)]
  z1 <- 0
  for(i in 1:(length(y1)-1)) {
    for(j in (i+1):(length(y1))) {
      w <- 18
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
      w <- 18
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
            v <- 0.1
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


lbounds <- bounder(start)


library(Rsolnp)
s1 <- eq1(start)
s <- ineq1(start)

sol <- solnp(start,fun=fn,eqfun=eq1,eqB=rep((10^-6),length(s1)),
             ineqfun=ineq1,ineqLB=lbounds,ineqUB=rep(30000,length(lbounds)))
round(sol$pars-min(sol$pars),1)