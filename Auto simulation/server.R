rm(list=ls())

requiredPackages <- c("shiny","VGAM")
for (package in requiredPackages) { #Installs packages if not yet installed
  if (!requireNamespace(package, quietly = TRUE))
    install.packages(package)
}
library(shiny)
library(VGAM)

# Define server logic required to draw the plots for different distributions ----

server = function(input, output){
  #set.seed(1234)
  
  #For calculating a matrix of random sample with 
  #'simulated sample' row and 'sample size' column 
  #for different parameters of different distributions 
  mysamp=NULL
  mysamp= reactive({
    if(input$popDistX== "normal"){
      t(replicate(input$sim,rnorm(input$nX, input$meannorm, input$sdnorm)))
    }else if(input$popDistX== "binomial"){
      t(replicate(input$sim,rbinom(input$nX, input$sizebinom, input$probbinom)))
    }else if(input$popDistX== "poisson"){
      t(replicate(input$sim,rpois(input$nX, input$lambda)))
    }else if(input$popDistX== "exponential"){
      t(replicate(input$sim,rexp(input$nX, input$rate)))
    }else if(input$popDistX== "gamma"){
      t(replicate(input$sim,rgamma(input$nX, input$shapegamma,input$rategamma)))
    }else if(input$popDistX== "weibull"){
      t(replicate(input$sim,rweibull(input$nX, input$shapeweibull,input$scaleweibull)))
    }else if(input$popDistX== "beta"){
      t(replicate(input$sim,rbeta(input$nX, input$shape1,input$shape2)))
    }else if(input$popDistX== "rayleigh"){
      t(replicate(input$sim,rrayleigh(input$nX, input$scalerayl)))
    }else if(input$popDistX== "uniform"){
      t(replicate(input$sim,runif(input$nX, input$min,input$max)))
     }
    else if(input$popDistX== "geometric"){
      t(replicate(input$sim,rgeom(input$nX, input$probgeom)))}
    else if(input$popDistX== "logistic"){
      t(replicate(input$sim,rlogis(input$nX, input$locationlogistic,input$scalelogistic)))
    }else if(input$popDistX== "lognormal"){
      t(replicate(input$sim,rlnorm(input$nX, input$meanlog,input$sdlog)))
    }else if(input$popDistX== "negative binomial"){
      t(replicate(input$sim,rnbinom(input$nX, input$sizenbinom,input$probnbinom)))
    }else if(input$popDistX== "hypergeometric"){
      t(replicate(input$sim,rhyper(input$nX, input$m,input$n,input$k)))
    }
  })
  
  #Calculating the sample mean vector
  meansamp=reactive(apply(mysamp(),1,mean))
  
  #Plotting the histogram of sample means
  histplot=reactive({
    bins <- seq(min(meansamp()), max(meansamp()), length.out = input$bins + 1)
    hist(meansamp(),col="blue",main="Histogram of sample mean",xlab="Random sample",breaks = bins)
  })
  
  #Normal Q-Q plot output when checked
  qqplot=reactive({
    input$QQplot
    if(input$QQplot){
      qqnorm(meansamp())
      qqline(meansamp())
    }else {
      (NULL)
    }
  })
  
  #Plots to check Convergence of sample mean to population mean
  converplot=reactive({
    input$Converplot
    if(input$Converplot){
      plot(1:input$sim,cumsum(meansamp())/(1:input$sim),type="l",ylab="Average Sample mean",xlab="Simulated Sample Size")
      if(input$popDistX== "normal"){
        abline(h=input$meannorm)
      }else if(input$popDistX== "binomial"){
        abline(h=input$sizebinom*input$probbinom)
      }else if(input$popDistX== "poisson"){
        abline(h=input$lambda)
      }else if(input$popDistX== "exponential"){
        abline(h=1/input$rate)
      }else if(input$popDistX== "gamma"){
        abline(h=input$shapegamma/input$rategamma)
      }else if(input$popDistX== "weibull"){
        abline(h=input$scaleweibull*gamma(1+(1/input$shapeweibull)))
      }else if(input$popDistX== "beta"){
        abline(h=input$shape1/(input$shape1+input$shape2))
      }else if(input$popDistX== "rayleigh"){
        abline(h=input$scalerayl*sqrt(pi/2))
      }else if(input$popDistX== "uniform"){
        abline(h=(input$min+input$max)/2)
      }
      else if(input$popDistX== "geometric"){
        abline(h=(1-input$probgeom)/input$probgeom)}
      else if(input$popDistX== "logistic"){
        abline(h=input$locationlogistic)
      }else if(input$popDistX== "lognormal"){
        abline(h=exp(input$meanlog+(input$sdlog)^2/2))
      }else if(input$popDistX== "negative binomial"){
        abline(h=(input$sizenbinom*(1-input$probnbinom))/input$probnbinom)
      }else if(input$popDistX== "hypergeometric"){
        abline(h=input$k*input$m/(input$m+input$n))
      }
    }else {
      (NULL)
    }
  })
  
  #Kernel density plots with theoretical density curves
  densplot=reactive({
    input$densityplot
    if(input$densityplot){
      m=max(density(meansamp())$y)
      plot(density(meansamp()),main="Kernel Density curve",ylim=c(0,max(m)+(max(m)*0.3)),yaxt="n",ylab="")
      polygon(density(meansamp()),col="red")
      if(input$popDistX== "normal"){
        curve(dnorm(x,input$meannorm,input$sdnorm/sqrt(input$nX)),col="blue",lwd=4,add=T)
      }else if(input$popDistX== "binomial"){
        curve(dnorm(x,input$sizebinom*input$probbinom,sqrt(input$sizebinom*input$probbinom*(1-input$probbinom))/sqrt(input$nX)),col="blue",lwd=4,add=T)
      }else if(input$popDistX== "poisson"){
        curve(dnorm(x,input$lambda,sqrt(input$lambda)/sqrt(input$nX)),col="blue",lwd=4,add=T)
      }else if(input$popDistX== "exponential"){
        curve(dnorm(x,1/input$rate,(1/input$rate)/sqrt(input$nX)),col="blue",lwd=4,add=T)
      }else if(input$popDistX== "gamma"){
        curve(dnorm(x,input$shapegamma/input$rategamma,sqrt(input$shapegamma/input$rategamma^2)/sqrt(input$nX)),col="blue",lwd=4,add=T)
      }else if(input$popDistX== "weibull"){
        curve(dnorm(x,input$scaleweibull*gamma(1+(1/input$shapeweibull)),sqrt(input$scaleweibull^2*(gamma(1+(2/input$shapeweibull))-(gamma(1+(1/input$shapeweibull)))^2))/sqrt(input$nX)),col="blue",lwd=4,add=T)
      }else if(input$popDistX== "beta"){
        curve(dnorm(x,input$shape1/(input$shape1+input$shape2),sqrt(input$shape1*input$shape2/((input$shape1+input$shape2)^2*(input$shape1+input$shape2+1)))/sqrt(input$nX)),col="blue",lwd=4,add=T)
      }else if(input$popDistX== "rayleigh"){
        curve(dnorm(x,input$scalerayl*sqrt(pi/2),sqrt((4-pi)*input$scalerayl^2/2)/sqrt(input$nX)),col="blue",lwd=4,add=T)
        abline(h=)
      }else if(input$popDistX== "uniform"){
        curve(dnorm(x,(input$min+input$max)/2,sqrt((input$max-input$min)^2/12)/sqrt(input$nX)),col="blue",lwd=4,add=T)
      }
      else if(input$popDistX== "geometric"){
        curve(dnorm(x,(1-input$probgeom)/input$probgeom,sqrt((1-input$probgeom)/input$probgeom^2)/sqrt(input$nX)),col="blue",lwd=4,add=T)}
      else if(input$popDistX== "logistic"){
        curve(dnorm(x,input$locationlogistic,sqrt(pi^2*input$scalelogistic^2/3)/sqrt(input$nX)),col="blue",lwd=4,add=T)
      }else if(input$popDistX== "lognormal"){
        curve(dnorm(x,exp(input$meanlog+(input$sdlog)^2/2),sqrt(exp((2*input$meanlog)+input$sdlog^2)*(exp(input$sdlog^2)-1))/sqrt(input$nX)),col="blue",lwd=4,add=T)
      }else if(input$popDistX== "negative binomial"){
        curve(dnorm(x,input$sizenbinom*(1-input$probnbinom)/input$probnbinom,(sqrt(input$sizenbinom*(1-input$probnbinom)/input$probnbinom^2))/sqrt(input$nX)),col="blue",lwd=4,add=T)
      }else if(input$popDistX== "hypergeometric"){
        curve(dnorm(x,input$k*input$m/(input$m+input$n),sqrt(input$k*(input$m/(input$m+input$n))*(1-(input$m/(input$m+input$n)))*((input$m+input$n-input$k)/(input$m+input$n-1)))/sqrt(input$nX)),col="blue",lwd=4,add=T)
      }
      legend("topright",legend="Theoretical Density",col="blue",lwd=4,bty="n")
    }else {
      (NULL)
    }
  })
  output$Hist=renderPlot({histplot()})
  output$QQ= renderPlot({qqplot()})
  output$Convplot=renderPlot({converplot()})
  output$Density=renderPlot({densplot()})
  
}

shinyApp(ui = ui, server = server)
