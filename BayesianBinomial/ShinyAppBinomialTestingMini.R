#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(miniUI)

# Define UI for application that draws a histogram
ui <- miniPage(
  
  # Application title
  titlePanel("Bayesian Binomial Analysis"),
  
  # Sidebar with a slider input for number of bins 
  miniButtonBlock(
    numericInput("aBeta",
                 "Shape a:",
                 min = 0,
                 max = 100,
                 value = 1,
                 step = 1),
    numericInput("bBeta",
                 "Shape b:",
                 min = 0,
                 max = 100,
                 value = 1,
                 step = 1)),
  miniButtonBlock(
    numericInput("aData",
                 "No. Success:",
                 min = 0,
                 max = 100,
                 value = 0,
                 step = 1),
    numericInput("bData",
                 "No. Failure:",
                 min = 0,
                 max = 100,
                 value = 0,
                 step = 1)),
  miniButtonBlock(
    checkboxGroupInput(inputId = "testEstimate", 
                       label = "Perform: ",
                       c("Posterior",
                         "Estimation",
                         "Testing")),
    radioButtons("hypo", label = "Alternative hypothesis:",
                 choices = list("Two-sided", "Greater", "Less"))
  ),
  
  # Show a plot of the generated distribution
  miniContentPanel(padding = 0,
                   plotOutput("betaPlot")
  )
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$betaPlot <- renderPlot({
    par(cex.lab = 1.4, cex = 1.8, mar = c(5.1,  4.1, 7.1, 2.1))
    
    lowestVal <- 0.0001
    highestVal <- 1-lowestVal
    
    lBound <- ifelse(input$hypo == "Greater", 0.5, lowestVal)
    uBound <- ifelse(input$hypo == "Less", 0.5, highestVal)
    
    multiPrior <- pbeta(uBound, input$aBeta, input$bBeta) - pbeta(lBound, input$aBeta, input$bBeta)
    multiPosterior <- pbeta(uBound, input$aBeta + input$aData, input$bBeta + input$bData) - 
      pbeta(lBound, input$aBeta + input$aData, input$bBeta + input$bData)
    
    
    postPoint <- dbeta(0.5, input$aBeta + input$aData, input$bBeta + input$bData) / multiPosterior
    priorPoint <- dbeta(0.5, input$aBeta, input$bBeta) / multiPrior
    bf01 <- postPoint / priorPoint
    bf10 <- 1/bf01
    
    credIntLo <- max(lBound, 0)
    credIntHi <- min(uBound, 1)
    ci <- qbeta(c(0.025, 0.975), input$aBeta + input$aData, input$bBeta + input$bData)
    
    maxY <- max(c(dbeta(seq(lBound, uBound, length.out = 1e3), 
                        input$aBeta + input$aData, 
                        input$bBeta + input$bData) / multiPosterior, 
                  dbeta(seq(lBound, uBound, length.out = 1e3), 
                        input$aBeta, 
                        input$bBeta) / multiPrior)) + 1
    
    if (!"Posterior" %in% input$testEstimate) {
      myTitle <- bquote("Prior: Beta Distribution (a =" ~ .(input$aBeta) * ", b =" ~ .(input$bBeta) *")" )
    } else {
      myTitle <- bquote(atop("Prior: Beta Distribution (a =" ~ .(input$aBeta) * ", b =" ~ .(input$bBeta) *")", 
                             "Posterior: Beta Distribution (a =" ~ .(input$aBeta + input$aData) * ", b =" ~ .(input$bBeta + input$bData) *")"))
      
    }
    
    curve(expr = {dbeta(x, input$aBeta, input$bBeta) / multiPrior}, 
          from = lBound,
          to = uBound,
          xlab = bquote("Unknown proportion" ~ theta),
          ylab = "Density",
          main = myTitle, bty = "n", lwd = 2, las = 1, col = "green",
          ylim = c(0, maxY), xlim = c(0, 1))
    
    if("Posterior" %in% input$testEstimate || "Testing" %in% input$testEstimate) {
      curve(expr = {dbeta(x, input$aBeta + input$aData, input$bBeta + input$bData) / multiPosterior}, 
            from = lBound,
            to = uBound,
            lwd = 2,
            col = "red", add = TRUE)
      lines(c(0.5, 0.5), c(0, max(postPoint, priorPoint)))
    }
    
    if("Testing" %in% input$testEstimate) {
      
      if (input$hypo == "Greater") {
        bfName01 <- bquote('BF'[0]['+'])
        bfName10 <- bquote('BF'['+'][0])
      } else if  (input$hypo == "Less") {
        bfName01 <- bquote('BF'[0]['-'])
        bfName10 <- bquote('BF'['-'][0])
      } else {
        bfName01 <- bquote('BF'[0][1])
        bfName10 <- bquote('BF'[1][0])
      }
      
      text(x = 0.15, y = maxY/1.2, bquote(.(bfName01) ~ " = " ~ .(round(bf01, 2))), cex = 1.2)
      text(x = 0.85, y = maxY/1.2, bquote(.(bfName10) ~ " = " ~ .(round(bf10, 2))), cex = 1.2)
      points(c(0.5, 0.5), c(priorPoint, postPoint), pch = 21, col = "black", bg = "gray", cex = 1.4)
    }
    
    if("Estimation" %in% input$testEstimate) {
      mtext(side = 3, text =  bquote('95% CI: ' * " = [" * .(round(ci[1], 3)) * " ," ~ .(round(ci[2], 3)) * "]"), cex = 1.6)
    }
    
  }, height = 800, width = 800 )
}

# Run the application 
shinyApp(ui = ui, server = server)

