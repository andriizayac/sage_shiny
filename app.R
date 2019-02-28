library(shiny)
library(ggplot2)
library(reshape2)
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
#library(reshape2)
edge=readRDS("edge.rds")

ui=fluidPage(  
  titlePanel("Sagebrush negative density dependence"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("xInput", "X", value=1, min=0.5,max=5),
      sliderInput("yInput", "Y", value=1, min=0.5,max=5),
      sliderInput("survinterceptInput", "Base survival", value=1, min=0.1,max=4),
      sliderInput("survslopeInput", "Survival rate", value=1, min=0.1, max=4 ),
      numericInput("timeInput", "Time year", value =4,min=3, max=6, step=1)
      ),
    mainPanel(
      tableOutput("Summary"),
      br(), br(),
      plotOutput("Growth_curves"),
      br(), br(),
      plotOutput("Mean_growth")
    )
                              ))

server=function(input,output){
    mat <- reactive({(data.frame(sage_sim_app(xinput=input$xInput,
                                             yinput=input$yInput,
                                             survintercept=input$survinterceptInput,
                                             survslope=input$survslopeInput,
                                             timesteps = input$timeInput)))
  })
  output$Growth_curves <- renderPlot({
    
    
    matgg=data.frame(t(mat()))
    matgg$census=c(1:nrow(matgg))
    matgg <- melt(matgg,id.var="census")
    ggplot(matgg, aes(census, value, color=variable)) + 
      geom_point()+
      theme(legend.position="none")+
      geom_line(aes(color=variable))+
      ggtitle("Individual growth curves")+xlab("Time (years)")+ylab(expression(paste("Size ", m^{3})))+
      theme(plot.title = element_text(size = 20, face = "bold"))
      
  })
  output$Mean_growth<- renderPlot({
    #mean_gr=c(NA,0.02848421,0.17859560,0.33129110,0.45917858,0.59245727,0.73370669,0.87013429,0.99052708,1.08735052)
    mean_gr=c(NA,0.02715738,0.32590488,0.62472772,0.98906185,1.34293356,1.75866847,2.10267756,2.46422140,2.96008277)
    mgr=mean_gr[2:(input$timeInput+1)]
    means=data.frame("mean_size"=sapply(mat(), mean, na.rm=T), 
                     "time"=c(1:input$timeInput),
                     "mean_ref"= mgr,
                     "ID"=c(1:length(mgr)))
    ggplot(means, aes(time,mean_size)) + 
      geom_point() +
      geom_smooth(se=FALSE)+
      geom_line(aes(time,mean_ref, color="red"))+
      ggtitle("Average growth rates")+xlab("Time (years)")+ylab(expression(paste("Size ", m^{3})))+
      theme(legend.position="none")+
      theme(plot.title = element_text(size = 20, face = "bold"))
  })
  output$Summary <- renderTable({
    mat1=mat()[edge,]
    cost=(input$xInput * input$yInput)
    summ=data.frame("Survival" = sum(!is.na(mat1[,input$timeInput]))/nrow(mat1),
                    "Reproductive_total" = sum(mat1[,input$timeInput]>0.5,na.rm=T)/nrow(mat1),
                    "Reproductive_survived" = sum(mat1[,input$timeInput]>0.5,na.rm=T)/sum(!is.na(mat1[,input$timeInput])),
                    "Cost"=cost/((sum(mat1[,input$timeInput]>0.5,na.rm=T)/nrow(mat1))*sum(!is.na(mat1[,input$timeInput]))/nrow(mat1)))
                    #"Cost"=cost/(1/sum(mat1[,input$timeInput]>0.5,na.rm=T)/nrow(mat1)))
    summ
  })
}
shinyApp(ui=ui,server=server)




