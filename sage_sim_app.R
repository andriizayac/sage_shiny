
mean_gr=c(NA,0.02848421,0.17859560,0.33129110,0.45917858,0.59245727,0.73370669,0.87013429,0.99052708,1.08735052)

sage_sim_app<-function(xinput=input$xInput,yinput=input$yInput,survintercept=input$survinterceptInput,survslope=input$survslopeInput, timesteps=input$timeInput) {
  
  x <- rep(seq(xinput, by = xinput, l = 10), times = 10)
  y <- sort(rep(seq(yinput, by = yinput, l = 10), times = 10))
  ID <- seq(1,length(x), by = 1)
  test<- data.frame(x,y, "height1" = abs(rnorm(10*10, mean = 0.03401094, sd = 0.0249334)), ID = ID)
  
  
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
                                sd=0.348)) * rbinom(1,1,plogis(survintercept+survslope*sizemat1[j,i-1]))                  
      if (sizemat1[j,i] <= 0 | is.na(sizemat1[j,i])){sizemat1[j,i] <- NA}
    }
    sizemat1[edge,i] <- mean(sizemat1[,i-1], na.rm = T)
    #plot(test$x, test$y, cex = sizemat1[,i], main = i)
  }
  #testcrop<- test[-which(test$x < min(test$x)+2 | test$x > max(test$x)-2 | test$y < min(test$y)+2 | test$y > max(test$y)-2),]
  #sizemat1[testcrop$ID,59]
  sizemat1=sizemat1[-edge,]
  return(sizemat1)
}


#mat$ID = c(1:nrow(mat))
matgg=data.frame(t(mat[,1:4]))
matgg$census=c(1:nrow(matgg))
matgg <- melt(matgg,id.var="census")
ggplot(matgg, aes(census, value, color=variable)) + 
  geom_point()+
  theme(legend.position="none")+
  geom_line(aes(color=variable))
