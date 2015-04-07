library(DiceView)
library(rgenoud)
source("global.R")
library(DiceOptim)

X0 = c(0, 0.25, 0.5, 0.75, 1.0)

shinyServer(function(input, output, session) {
  
  estim.press <- 0
  
  f = reactive({
    ff = function(x) eval(parse(text=input$f),list(x=x))
    return(ff)
  })
  
  XY <- reactiveValues(
    X = X0,
    Y = (function(x)eval(parse(text=f_str),list(x=x)))(X0)
  )
  
  
  observe({
    x=as.numeric(input$xy_click$x)
    y=as.numeric(input$xy_click$y)
    
    if(isolate(input$add_remove)=="add point") {
      isolate({Xnew = c(XY$X,x)})
      XY$X <- Xnew
      isolate({Ynew = c(XY$Y,y)})
      XY$Y <- Ynew
    } else {
      isolate({ix=which.min(abs((XY$X-x)*(XY$Y-y)))})
      isolate({Xnew = XY$X[-ix]})
      XY$X = Xnew
      isolate({Ynew = XY$Y[-ix]})
      XY$Y = Ynew 
    }
    
  })
  
  k = reactive({
    
    range_value=input$range_value 
    #scaling_0=input$scaling_0
    #scaling_1=input$scaling_1
    #nugget_sdvalue=input$nugget_sdvalue
    sd_value=input$sd_value
    
    scaling=F
    nugget=F
    
    
    #       if(scaling) 
    #         
    #         k = km(formula = as.formula("y~1"),
    #                design=data.frame(x=XY$X), response=as.data.frame(XY$Y),
    #                covtype=input$C,
    #                scaling=T,
    #                coef.cov=matrix(c(scaling_0,scaling_1),nrow=1),
    #                nugget.estim=F,
    #                nugget=ifelse(nugget,nugget_sdvalue^2,0),
    #                coef.var=sd_value^2
    #         )
    #       
    #       else
    
    k = km(formula = as.formula("y~1"),
           design=data.frame(x=XY$X), response=as.data.frame(XY$Y),
           covtype=input$C,
           scaling=F,
           coef.cov=range_value,
           nugget.estim=F,
           nugget=ifelse(nugget,nugget_sdvalue^2,0),
           coef.var=sd_value^2
    )
    
    #} else {
    return(k)
  })
  
  observe ({
    
    seed=1#input$seed
    estim=input$estim
    scaling=F
    nugget=F
    
    if (estim > estim.press) {
      
      estim.press <<- estim
      
      set.seed(as.numeric(seed))
      
      k = km(design=data.frame(x=XY$X), response=as.data.frame(XY$Y),
             covtype=input$C,
             scaling=as.logical(scaling),
             nugget.estim=as.logical(nugget),
             optim.method=ifelse(input$optim=="BFGS","BFGS","gen"),
             lower=input$lower,
             estim.method=ifelse(input$estim.method=="Maximum likelihood","MLE","LOO"))
      
      #save(k,file = "k.Rdata")
      isolate({
        updateSliderInput(session,"sd_value",value=sqrt(k@covariance@sd2))
        
        #if (!input$scaling) 
        updateSliderInput(session,"range_value",value=k@covariance@range.val)
        #else {
        #  updateSliderInput(session,"scaling_0",value=k@covariance@eta[1,1])
        #  updateSliderInput(session,"scaling_1",value=k@covariance@eta[1,2])
        #}
        
        #if (input$nugget) updateSliderInput(session,"nugget_sdvalue",value=sqrt(k@covariance@nugget))
      })
    }
  })
  
  output$Kriging <- renderPlot({
    model=k()
    
    sectionview.km(model,Xname="x",yname = "Y_x",title="",ylim=c(-1.2,0.2),xlim=c(0,1))
    
    x = seq(from=0,to=1,by=0.01)
    lines(x,f()(x))
    
    for (i in 1:10) {
      realization = simulate(model,seed=i,newdata=x,cond=TRUE,nugget.sim=0.00001,checkNames=F)
      # Plot this realization
      lines(x,realization,col=rgb(0,0,1,0.2))
    }
    
    abline(h=min(XY$Y),col='red')
  })
  
  output$EI <- renderPlot({
    model=k()
    
    x = seq(from=0,to=1,by=0.001)
    EI_x = sapply(X = x,FUN=function(x)EI(x,model))
    plot(x,EI_x,type='l',col='blue')
    
  })
  
  #output$XY = renderText({
  #  paste(XY$X, XY$Y)
  #})
  
  output$print <- renderPrint({
    k()
  })
  
  #   output$content <- reactivePrint(function() {
  #     input$files
  #   })
  
})

