rm(list=ls())

requiredPackages <- c("shiny","VGAM")
for (package in requiredPackages) { #Installs packages if not yet installed
  if (!requireNamespace(package, quietly = TRUE))
    install.packages(package)
}
library(shiny)
library(VGAM)

# Define UI for app that draws Different plots for different distributions ----
ui <- fluidPage(    
  tags$head(
  tags$style(HTML("
            code {
                display:block;
                padding:9.5px;
                margin:0 0 10px;
                margin-top:10px;
                font-size:13px;
                line-height:20px;
                word-break:break-all;
                word-wrap:break-word;
                white-space:pre-wrap;
                background-color:#F5F5F5;
                border:1px solid rgba(0,0,0,0.15);
                border-radius:4px; 
                font-family:monospace;
            }"))),
  
  # App title ----
  titlePanel("Central Limit Theorem"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
   strong("Central limit theorem says, if we draw a sufficiently large number of sample 
        from any population with a finite mean and finite variance, then the distribution 
        of the sample mean will approximately follow a Normal distribution. But does it? Always?
        Let's find out!"),
      
      #Whether to use normal QQ plot
      checkboxInput(inputId = "QQplot",
                    label = "Normal Q-Q plot",
                    value = T
                    ),
      checkboxInput(inputId = "Converplot",
                    label = "Convergence plot",
                    value = T
      ),
      checkboxInput(inputId = "densityplot",
                    label = "Density curve",
                    value = T
      ),
      
      #Choosing different distributions with different parameters
      selectInput(inputId = "popDistX",
                  label = "Population",
                  list("Normal"="normal",
                       "Binomial"="binomial",
                       "Poisson"="poisson",
                       "Exponential"="exponential",
                       "Gamma"="gamma",
                       "Weibull"="weibull",
                       "Beta"="beta",
                       "Rayleigh"="rayleigh",
                       "Uniform"="uniform",
                       "Geometric"="geometric",
                       "Logistic"="logistic",
                       "Lognormal"="lognormal",
                       "Negative Binomial"="negative binomial",
                       "Hypergeometric"="hypergeometric")),
      
      
      
      #Parameter inputs for distributions
      #For normal
      conditionalPanel(
        condition = "input.popDistX == 'normal'",
        numericInput("meannorm", "Mean", min=-500, max=500, value=0, step=0.01),
        numericInput("sdnorm", "St. dev. ", min=0.01, max=50, value=1, step=0.01)
      ),
      #For Binomial
      conditionalPanel(
        condition = "input.popDistX == 'binomial'",
        numericInput("sizebinom", "n", min=5, max=500, value=10, step=1),
        numericInput("probbinom", "p ", min=0.01, max=1, value=0.5, step=0.01)
      ),
      #For Poisson
      conditionalPanel(
        condition = "input.popDistX == 'poisson'",
        numericInput("lambda", "lambda", min=0.01, max=500, value=3, step=0.01)
      ),
      #For Exponential
      conditionalPanel(
        condition = "input.popDistX == 'exponential'",
        numericInput("rate", "lambda", min=0.01, max=500, value=3, step=0.01)
      ),
      #For Gamma
      conditionalPanel(
        condition = "input.popDistX == 'gamma'",
        numericInput("shapegamma", "shape", min=0.01, max=500, value=2, step=0.01),
        numericInput("rategamma", "rate", min=0.01, max=500, value=2, step=0.01)
      ),
      #For Weibull
      conditionalPanel(
        condition = "input.popDistX == 'weibull'",
        numericInput("shapeweibull", "shape", min=0.01, max=500, value=2, step=0.01),
        numericInput("scaleweibull", "scale", min=0.01, max=500, value=2, step=0.01)
      ),
      #For Beta
      conditionalPanel(
        condition = "input.popDistX == 'beta'",
        numericInput("shape1", "shape1", min=0.01, max=500, value=2, step=0.01),
        numericInput("shape2", "shape2", min=0.01, max=500, value=2, step=0.01)
      ),
      #For Rayleigh
      conditionalPanel(
        condition = "input.popDistX == 'rayleigh'",
        numericInput("scalerayl","scale",min=0.01,max=500,value=2,step = 0.01)
      ),
      #For Uniform
      conditionalPanel(
        condition = "input.popDistX == 'uniform'",
        numericInput("min", "min", min=-500, max=500, value=0, step=0.01),
        numericInput("max", "max", min=-400, max=600, value=1, step=0.01)
      ),
      #For Geometric
      conditionalPanel(
        condition = "input.popDistX == 'geometric'",
        numericInput("probgeom", "prob", min=0.01, max=1, value=0.5, step=0.01)
      ),
      #For Logistic
      conditionalPanel(
        condition = "input.popDistX == 'logistic'",
        numericInput("locationlogistic", "location", min=-500, max=500, value=0, step=0.01),
        numericInput("scalelogistic", "scale", min=0.01, max=500, value=1, step=0.01)
      ),
      #For Lognormal
      conditionalPanel(
        condition = "input.popDistX == 'lognormal'",
        numericInput("meanlog", "log(mean)", min=-500, max=500, value=0, step=0.01),
        numericInput("sdlog", "log(st.dev.)", min=0.01, max=500, value=1, step=0.01)
      ),
      #For negative binomial
      conditionalPanel(
        condition = "input.popDistX == 'negative binomial'",
        numericInput("sizenbinom", "n", min=5, max=500, value=10, step=1),
        numericInput("probnbinom", "p ", min=0.01, max=1, value=0.5, step=0.01)
      ),
      #For Hypergeometric
      conditionalPanel(
        condition = "input.popDistX == 'hypergeometric'",
        numericInput("m", "Success in Pop.", min=0, max=500, value=10, step=1),
        numericInput("n", "Pop. Size - Success in Pop. ", min=10, max=1000, value=50, step=1),
        numericInput("k", "sample size ", min=5, max=100, value=20, step=1)
      ),
      #Input for sample size
      numericInput(inputId = "nX",
                   label = "Sample size",
                   min = 40,
                   max = 1e10,
                   value = 100),
      #Input for simulated sample
      numericInput(inputId = "sim",
                   label = "# of simulated sample",
                   min = 10,
                   max = 1e10,
                   value = 100),
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 5,
                  max = 50,
                  value = 30)
      
      
    ),
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      fluidRow(
        splitLayout(
         cellWidths = c("50%","50%"),
        tabPanel("Plot1",
                 # Histogram output
                 plotOutput(outputId = "Hist"),
                 
                 #Convergence plot
                 plotOutput(outputId = "Convplot"),
                 
        ),
        tabPanel("plot2",
                 #QQplot
                 plotOutput(outputId = "QQ"),
                 
                 #Density plot
                 plotOutput(outputId = "Density"))
        
                 

      )
      
    )
  )
)
)
