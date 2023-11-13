#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(plotrix)
library(shiny)
library(shinydashboard)

# Define UI for application that draws a histogram
ui <-  dashboardPage(
  dashboardHeader(title = "Interaction Regression"),
  
  dashboardSidebar(width = 220,
                   sidebarMenu(style = "position:auto;width:220px;",
                               (radioButtons("whatPred",
                                             "What model do we predict with?",
                                             c("Null", "A", "B", "A + B", "A + B + AB"), 
                                             selected = "Null")),
                               sliderInput("nSamples",
                                           withMathJax("$$n$$"),
                                           min = 5,
                                           max = 50,
                                           value = 5),
                               sliderInput("betaX",
                                           withMathJax("$$\\beta_{A}$$"),
                                           min = -3,
                                           max = 3,
                                           step = 0.1,
                                           value = 0),
                               sliderInput("betaZ",
                                           withMathJax("$$\\beta_{B}$$"),
                                           min = -3,
                                           max = 3,
                                           step = 0.1,
                                           value = 0),
                               sliderInput("betaXZ",
                                           withMathJax("$$\\beta_{AB}$$"),
                                           min = -3,
                                           max = 3,
                                           step = 0.1,
                                           value = 0),
                               checkboxGroupInput("whatDisplay",
                                                  "Display:",
                                                  c("Segments", "Sums", "R2")),
                               checkboxInput("predictWithMainModel",
                                             "Use main effects model as null (full only)", value = FALSE),
                               sliderInput("showAtZ",
                                           "Condition B variable on:",
                                           min = -3,
                                           max = 3,
                                           step = 0.1,
                                           value = 0))
  ),
  
  dashboardBody(
    tabBox(
      side = "left", height = "250px",
      selected = "Prediction",
      tabPanel("Prediction", plotOutput('distPlot')),
      tabPanel("Correlation", plotOutput("corPlot")),
      tabPanel("Output", tableOutput("regTable")))
    
  )
  
  
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$corPlot <- renderPlot({
    
    set.seed(283) ## 
    predictor         <- rnorm(input$nSamples, 0, 3)
    moderator         <- predictor + rnorm(input$nSamples, 0, 3) ## multicollinearity
    error     <- rnorm(input$nSamples, 0, 3) ## error
    intercept <- 0
    outcome   <- intercept + input$betaX*predictor + 
      input$betaZ*moderator + 
      input$betaXZ*predictor*moderator + 
      error
    outcome <- (outcome - mean(outcome)) / sd(outcome)
    
    plotN <- input$nSamples 
    if (input$whatPred == "A + B") {
      myMod <-  lm(outcome ~ predictor + moderator)
      currentB <- myMod$coefficients['predictor']
    } else if (input$whatPred == "A + B + AB") {
      myMod <-  lm(outcome ~ predictor * moderator)
      currentB <- myMod$coefficients['predictor'] + myMod$coefficients['predictor:moderator'] * input$showAtZ
    } else if (input$whatPred == "A") {
      myMod <-  lm(outcome ~ predictor)
      currentB <- myMod$coefficients['predictor']
    } else {
      myMod <- lm(outcome ~ 1)
      currentB <- 0
    }
    
    prediction <- myMod$fitted.values
    
    mycor <- cor(prediction, outcome)
    par(cex = 1.35)
    plot(predictor, outcome, 
         main = paste0("Slope of A, for B = ", input$showAtZ), 
         bg = 'purple', pch = 21, bty = "n", las = 1, ylim = c(-3, 3),
         xlab="A", xlim = c(-3, 3),
         ylab="Outcome")
    if (grepl(x = input$whatPred, pattern = "A")) {
      abline(a = myMod$coefficients['(Intercept)'], b = currentB, col = 'darkgreen', lwd = 3)
      mtext(bquote(beta[pred] ~ " = " ~ .(round(currentB, 2))), cex = 2.5, line = -3)
    }
    
  }, width = 700, height = 700)
  
  output$regTable <- renderTable({
    
    set.seed(283) ## 
    predictor         <- rnorm(input$nSamples, 0, 3)
    moderator         <- predictor + rnorm(input$nSamples, 0, 3) ## multicollinearity
    error     <- rnorm(input$nSamples, 0, 3) ## error
    intercept <- 0
    outcome   <- intercept + input$betaX*predictor + 
      input$betaZ*moderator + 
      input$betaXZ*predictor*moderator + 
      error
    outcome <- (outcome - mean(outcome)) / sd(outcome)
    A <- predictor
    B <- moderator
    plotN <- input$nSamples 
    if (input$whatPred == "Null") {
      myMod <-  lm(outcome ~ 1)
      
    } else if (input$whatPred == "A") {
      myMod <-  lm(outcome ~ A)
      
    } else if (input$whatPred == "B") {
      myMod <-  lm(outcome ~ B )
      
    } else if (input$whatPred == "A + B") {
      myMod <-  lm(outcome ~ A + B)
      
    } else if (input$whatPred == "A + B + AB") {
      myMod <-  lm(outcome ~ A * B)
    }
    
    if (input$predictWithMainModel & input$whatPred == "A + B + AB") {
      nullPredictions <- lm(outcome ~ A+B)$fitted.values
    } else {
      nullPredictions <- rep(mean(outcome), plotN)
    }
    
    prediction <- myMod$fitted.values
    df <- data.frame(Name = rownames(summary(myMod)$coefficients), summary(myMod)$coefficients)
    colnames(df) <- c("Name", "Estimate", "SE", "t-value", "p-value")
    data.table::data.table(df)
  })
  
  
  output$distPlot <- renderPlot({
    # myCols <- palette.colors(n = 9, palette = "Okabe-Ito")
    # input <- list(betaX = 1, betaZ = 0.5, betaXZ = 0.2, input$nSamples = 10)
    set.seed(283) ##
    predictor         <- rnorm(input$nSamples, 0, 3)
    moderator         <- predictor + rnorm(input$nSamples, 0, 3) ## multicollinearity
    error     <- rnorm(input$nSamples, 0, 3) ## error
    intercept <- 0
    outcome   <- intercept + input$betaX*predictor + 
      input$betaZ*moderator + 
      input$betaXZ*predictor*moderator + 
      error
    outcome <- (outcome - mean(outcome)) / sd(outcome)
    
    plotN <- input$nSamples
    if (input$whatPred == "Null") {
      myMod <-  lm(outcome ~ 1)
    } else if (input$whatPred == "A") {
      myMod <-  lm(outcome ~ predictor)
      
    } else if (input$whatPred == "B") {
      myMod <-  lm(outcome ~ moderator)
      
    } else if (input$whatPred == "A + B") {
      myMod <-  lm(outcome ~ predictor + moderator)
      
    } else if (input$whatPred == "A + B + AB") {
      myMod <-  lm(outcome ~ predictor * moderator)
    }
    
    if (input$predictWithMainModel & input$whatPred == "A + B + AB") {
      nullPredictions <- lm(outcome ~ predictor + moderator)$fitted.values
    } else {
      nullPredictions <- rep(mean(outcome), plotN)
    }
    
    prediction <- myMod$fitted.values
    plotSumSquares(outcome, prediction, input, plotN, nullPredictions)   
    
  }, width = 1200, height = 600)
  
}


plotSumSquares <- function(outcome, prediction, input, plotN = 10, nullPredictions) {
  par(mfrow = c(1, 3), cex = 1.35)
  
  totSumSq <- sum((outcome - nullPredictions)^2)
  modSumSq <- sum((prediction - nullPredictions)^2)
  errSumSq <- sum((prediction - outcome)^2)
  
  plot(outcome[1:plotN],xlab='Participant', ylab = "Outcome", las = 1, ylim = c(-2, 2),
       bty = "n", pch = 21, bg = "turquoise", main = "Total Variance")
  
  if (input$predictWithMainModel & input$whatPred == "A + B + AB") {
    points(nullPredictions[1:plotN], pch = 23, bg = "purple", cex = 3)
  } else {  
    abline(h = mean(outcome[1:plotN]), lwd = 2, col = "purple")
  }
  
  
  points(1:plotN,prediction[1:plotN], pch =23, bg='darkred')
  if ("Segments" %in% input$whatDisplay) 
    segments(1:plotN, outcome[1:plotN], 1:plotN, nullPredictions[1:plotN], col='orange', lwd= 2)
  
  legend("topleft", c("Observed", "Predicted", "Null"),
         bty = "n", fill = c("turquoise", "darkred", "purple"))
  if ("Sums" %in% input$whatDisplay) {
    mtext(paste0("Total Sum of Squares = ", round(totSumSq, 2)), cex = 1.4)
  }
  
  # plot 2
  plot(outcome[1:plotN],xlab='Participant', ylab = "Outcome", las = 1, ylim = c(-2, 2),
       bty = "n", pch = 21, bg = "turquoise", main = "Explained Variance")
  if (input$predictWithMainModel & input$whatPred == "A + B + AB") {
    points(nullPredictions[1:plotN], pch = 23, bg = "purple", cex = 3)
  } else {  
    abline(h = mean(outcome[1:plotN]), lwd = 2, col = "purple")
  }
  if ("Segments" %in% input$whatDisplay) 
    segments(1:plotN, prediction[1:plotN], 1:plotN, nullPredictions[1:plotN], col='blue', lwd= 2)
  points(1:plotN, prediction[1:plotN], pch =23, bg='darkred')
  legend("topleft", c("Observed", "Predicted", "Null"),
         bty = "n", fill = c("turquoise", "darkred", "purple"))
  if ("Sums" %in% input$whatDisplay) {
    mtext(paste0("Total Sum of Squares = ", round(modSumSq, 2)), cex = 1.4)
  }
  if ("R2" %in% input$whatDisplay) {
    mtext(paste0("Proportion = ", round(modSumSq / totSumSq, 2)), cex = 1.4, line = 3)
  }
  # Plot 3
  plot(outcome[1:plotN],xlab='Participant', ylab = "Outcome", las = 1, ylim = c(-2, 2),
       bty = "n", pch = 21, bg = "turquoise", main = "Unexplained Variance")
  if ("Segments" %in% input$whatDisplay) 
    segments(1:plotN, prediction[1:plotN], 1:plotN, (outcome[1:plotN]), col='red', lwd= 2)
  points(1:plotN,prediction[1:plotN], pch =23, bg='darkred')
  legend("topleft", c("Observed", "Predicted"),
         bty = "n", fill = c("turquoise", "darkred"))
  if ("Sums" %in% input$whatDisplay) {
    mtext(paste0("Total Sum of Squares = ", round(errSumSq, 2)), cex = 1.4)
  }
  
}



# Run the application 
shinyApp(ui = ui, server = server)
