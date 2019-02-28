


sage_sim<-function(competition_strength=NULL,distance_strength=NULL,survintercept=1,survslope =1, timesteps=4) {
  
  mean_growth = rep(NA, time=timesteps)
  x <- rep(seq(1, by = 1, l = 10), times = 10)
  y <- sort(rep(seq(0, by = 1.5, l = 10), times = 10))
  ID <- seq(1,length(x), by = 1)
  test<- data.frame(x,y, "height1" = abs(rnorm(10*10, mean = 0.02401094, sd = 0.0249334)), ID = ID)
  
  
  
  sizemat1<-matrix(NA,nrow=nrow(test),ncol=timesteps)
  sizemat1[,1]<-test$height1
  dist.fx<-function(x1,x2,y1,y2) {sqrt((x1-x2)^2+(y1-y2)^2)}
  rm.zero<-function(x){x<- x[x != 0]}
  edge<-test$ID[which(test$x < min(test$x)+2 | test$x > max(test$x)-2 | test$y < min(test$y)+2 | test$y > max(test$y)-2)]
  for(i in 2:timesteps) {
    for(j in 1:nrow(test)) {
      
      dist_vec<-dist.fx(test$x[j],test$x,test$y[j],test$y)
      comp_term<- sum(((sizemat1[i-1]/exp(rm.zero(dist_vec)^2*1.39))),na.rm=TRUE) #size-dependent competition
      sizemat1[j,i] <- (sizemat1[j,i-1]+
        rnorm(1, mean=0.315 + sizemat1[j,i-1] * (5.26/72) + -(2.89)*(comp_term),
              sd=0.148)) * rbinom(1,1,plogis(10+10*sizemat1[j,i-1])) input_data <- read_rdump("eight_schools.data.R")                 
      if (sizemat1[j,i] <= 0 | is.na(sizemat1[j,i])){sizemat1[j,i] <- NA}
    }
    sizemat1[edge,i] <- mean(sizemat1[,i-1], na.rm = T)
    mean_growth[i]=(mean(sizemat1[-edge,i-1]))
    #plot(test$x, test$y, cex = sizemat1[,i], main = i)
  }
  #testcrop<- test[-which(test$x < min(test$x)+2 | test$x > max(test$x)-2 | test$y < min(test$y)+2 | test$y > max(test$y)-2),]
  #sizemat1[testcrop$ID,59]
  matplot(t(sizemat1),type="l", xlim = c(1,timesteps), ylim = c(0,2), xlab = "time (years)", ylab = "size",  main = "ABM simulated growth curves")
  #plot(test$x, test$y, cex = 0.015*sizemat1[,timesteps])
  
  #bio.dists <- as.matrix(dist(cbind(test$x, test$y)))
  #bio.dists.inv <- 1/bio.dists
  #diag(bio.dists.inv) <- 0
  
  #moransI <- round(unlist(Moran.I(sizemat1[test$ID,timesteps], bio.dists.inv, na.rm = TRUE)[1]), digits = 3)
  #cover <- round(range(sizemat1[,timesteps], na.rm = TRUE), digits = 3)
  #cover <- round(sum(sizemat1[,timesteps], na.rm = TRUE), digits = 3)
  #return(mean_growth)
  return(edge)
 
}

