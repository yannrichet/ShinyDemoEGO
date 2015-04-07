library(DiceKriging)
source("global.R")

shinyUI(pageWithSidebar(
  
  headerPanel(windowTitle = "DiceOptim",tags$div(paste("DiceOptim (version",packageDescription(pkg="DiceOptim")$Version,")"),
                                               tags$img(src="shinyapps_diceoptim.png",width=100),
                                               align="center")),
  
  sidebarPanel(
    
    selectInput("C", "Covariance kernel",choices = c("matern3_2","matern5_2","gauss","exp")),
    
    sliderInput("sd_value", "Std deviation", value=0.1,min=0,max=1,step=0.01),
    
    #checkboxInput("scaling",'Non-stationary kernel ("affine scaling")',F),
    #conditionalPanel('input.scaling==true',
    #                 sliderInput("scaling_0", "Scaling x=0", value=5,min=0,max=100,step=1),
    #                 sliderInput("scaling_1", "Scaling x=1", value=5,min=0,max=100,step=1)),
    #conditionalPanel('input.scaling!=true',
                     sliderInput("range_value", "Range", value=0.1,min=0,max=1,step=0.01),#),
    
    #checkboxInput("nugget","Nugget effect",F),
    #conditionalPanel('input.nugget==true',
    #                 sliderInput("nugget_sdvalue", "Nugget (sd)", value=0,min=0,max=0.5,step=0.001)),
    
    
    hr(),
    
    div(align='center',actionButton("estim","Parameters Estimation")),
    radioButtons("estim.method",label = "Estimation method",choices = c("Maximum likelihood","Leave-one-out"),selected="Maximum likelihood"),
    selectInput("optim", "Optimization method", choices = c("BFGS","GenOUD")),
    #checkboxInput("fo","likelihood fail-over", value=FALSE),
    numericInput("lower","Lower bounds", value=0.01,min=0,max=10,step=0.01)#,
    #sliderInput("seed","Random seed",0,100,0, step=1,animate=TRUE)
    
  ),
  
  mainPanel(
    fluidRow(
      column(6,
             textInput("f","function to minimize f(x)=",f_str)#,"sin(30*(x-0.9)^4)*cos(2*(x-0.9)) + (x-0.9)/2)"))
      ),
      column(6,
             radioButtons("add_remove",label = "Click do:",choices = c("add point","remove point"),inline=T)
      )
    ),
    plotOutput(outputId = "Kriging",clickId="xy_click", height = "400px"),
    plotOutput(outputId = "EI",height = "400px")
    #verbatimTextOutput("print")
    #             ,verbatimTextOutput("content")
  )
  
  
))

