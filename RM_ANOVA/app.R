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
  dashboardHeader(title = "Within Squares"),
  
  dashboardSidebar(width = 220,
                   sidebarMenu(style = "position:fixed;width:220px;",
                               (radioButtons("whatPred",
                                             "What model do we predict with?",
                                             c("Mean", "Alcohol", "ID", "Alcohol + ID"), 
                                             selected = "Mean")),
                               checkboxGroupInput("whatDisplay",
                                                  "Display:",
                                                  c("Segments", "Sums", "F-stat")),
                               # checkboxInput("overlayFullPred",
                               #               "Overlay full model", value = FALSE),
                               sliderInput("showSampleSize",
                                             "Display (n)", value = 10, min = 4, max = 20))
  ),
  
  dashboardBody(
    plotOutput('distPlot')
  )
  
  
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    # myCols <- palette.colors(n = 9, palette = "Okabe-Ito")
    
    par(mfrow = c(3, 1), cex = 1.35)
    plotSumSquares(mydat, input = input, sumSq = "Total", 
                   alcColors = palette.colors(n = 3, palette = "Okabe-Ito"),
                   speedSymbols = c(21, 22, 23))
    
    plotSumSquares(mydat, input = input, sumSq = "Error", 
                   alcColors = palette.colors(n = 3, palette = "Okabe-Ito"),
                   speedSymbols = c(21, 22, 23), overlayFullPred = FALSE)
    
    plotSumSquares(mydat, input = input, sumSq = "Model", 
                   alcColors = palette.colors(n = 3, palette = "Okabe-Ito"),
                   speedSymbols = c(21, 22, 23))    
    
  }, width = 1200, height = 1800)
  
}



plotSumSquares <- function(data, input, sumSq = "Total", stats = NULL, plotMean = TRUE, 
                             alcColors = NULL, speedSymbols = NULL, overlayFullPred = FALSE) {
  
  # mydat <- read.csv("RM_ANOVA_AlcoholSpeedTime.csv")
  mydat <- read.csv("RM_ANOVA_Alcohol_LONG.csv")
  mydat$alcohol <- as.factor(mydat$alcohol)
  mydat <- mydat[order(mydat$pp), ]
  mydat <- mydat[mydat$pp <= input$showSampleSize, ]
  mydat$pp <- as.factor(mydat$pp)
  
  totN <- nrow(mydat)
  nGroups <- length(unique(mydat$alcohol))
  grandMean <- mean(mydat$accidents)
  nPerGroup <- length(unique(mydat$pp))
  myAlcMod <-  lm(accidents ~ alcohol, data = mydat)
  myPpMod <-  lm(accidents ~ pp, data = mydat)
  myMainMod <- myFullMod <-lm(accidents ~ alcohol + pp, data = mydat)
  
  
  
  plot(mydat$accidents, col = "black" , pch = 21, bg = "blue", 
       cex = 1.8, lwd = 3, las = 1, bty = "n", main = "",
       ylab = "Accidents", xlab = "Observation Nr.", xlim = c(0, length(mydat$accidents)), 
       ylim = c(3, 8), cex.lab = 1.3, cex.axis=1.3)
  totN <- nrow(mydat)
  
  if (input$whatPred == "Alcohol") {
    myMod <-  myAlcMod
    dfMod <- nGroups -1
    dfError <- (nGroups-1) * (nPerGroup-1)
    myBgs <- palette.colors(n = 4, palette = "Okabe-Ito")[as.numeric(mydat$alcohol)+1]
    myShapes <- 23
    myCols <- "black"
    if (sumSq != "Total")
      legend("topleft", c("noneA", "someA", 'highA'), fill = unique(myBgs), col = unique(myBgs), bty = "n", horiz = TRUE)
    
  } else if (input$whatPred == "ID") {
    myMod <- myPpMod
    dfMod <- nPerGroup-1
    dfError <- (nGroups-1) * (nPerGroup)
    myCols <- palette.colors(n = 1, palette = "Okabe-Ito")
    myBgs <- "darkgreen"
    myShapes <- (1:nPerGroup)[mydat$pp]
    if (sumSq != "Total")
      legend("topleft", legend = 1:nPerGroup, pch = unique(myShapes), bty = "n", horiz = TRUE)
    
  } else if (input$whatPred == "Alcohol + ID") {
    myMod <- myMainMod
    dfMod <- (nGroups -1) + (nPerGroup-1)
    dfError <- (nGroups -1) * (nPerGroup-1)
    myShapes <- (1:nPerGroup)[mydat$pp]
    myCols <- palette.colors(n = 4, palette = "Okabe-Ito")[as.numeric(mydat$alcohol)+1]
    myBgs <- "darkgreen"
    if (sumSq != "Total")
      legend("topleft", c("noneA", "someA", 'highA'), fill = unique(myCols), col = unique(myCols), bty = "n", horiz = TRUE)
    
  } else {
    myMod <- lm(accidents ~ 1, data = mydat)
    dfMod <- 0
    dfError <- totN - 1
    myShapes <- 23
    myBgs <- "darkgreen"
    myCols <- "black"
    
  }
  
  predPoints <- myMod$fitted.values
  modAccSumSquares <- sum((predPoints - mean(mydat$accidents))^2)
  modAccMeanSquare <- modAccSumSquares / dfMod
  modErrorSumSquares <- sum((predPoints - mydat$accidents)^2)
  
  fullModErrorSumSq <- sum(myFullMod$residuals^2)
  fullModAccSumSq <- sum((myFullMod$fitted.values - mean(mydat$accidents))^2)
  fullModMeanSquareError <- fullModErrorSumSq / ( (nGroups -1) * (nPerGroup-1))
  nulModError <- sum((mean(mydat$accidents) - mydat$accidents)^2)
  
  if (sumSq == "Total") {
    if (plotMean) {
      abline(h = mean(mydat$accidents), lwd = 3, col = "purple")
    }
    if ("Segments" %in% input$whatDisplay) {
      segments(x0 = 1:nrow(mydat), x1 = 1:nrow(mydat), y0 = mean(mydat$accidents), y1 = mydat$accidents, lwd = 2)
      if ("Sums" %in% input$whatDisplay) {
        mtext(paste0("Total Sum of Squares = ", round(nulModError, 2)), cex = 1.4)
      } else {
        mtext("", cex = 1.8)
      }
    }
  } 
  
  
  if (sumSq == "Model") {
    abline(h = mean(mydat$accidents), lwd = 3, col = "purple")
    points(x = 1:nrow(mydat), y = predPoints, pch = myShapes, bg = myBgs, col = myCols, cex = 1.35, lwd = 3)
    if ("Segments" %in% input$whatDisplay) {
      segments(x0 = 1:nrow(mydat), x1 = 1:nrow(mydat), y0 = predPoints, y1 = mean(mydat$accidents), lwd = 2, col = "darkgray")
      if ("Sums" %in% input$whatDisplay) {
        if (input$whatPred == "Mean" | !("F-stat" %in% input$whatDisplay)) {
          mtext(paste0("Model Sum of Squares = ", round(modAccSumSquares, 3), "\nMean Square = ", round(modAccSumSquares/dfMod, 3)), cex = 1.4, line = 0)
        } else {
          mtext(paste0("Model Sum of Squares = ", round(modAccSumSquares, 3), "\nMean Square = ", round(modAccSumSquares/dfMod, 3), 
                       "\nF = ",round(modAccSumSquares/dfMod, 3),"/",round(fullModMeanSquareError,3), " = ", round(round(modAccSumSquares/dfMod, 3)/fullModMeanSquareError, 3)) , cex = 1.4, line = 0)
        }
      } else {
        mtext("Model improvement", cex = 1.8)
      }
    }
  }
  
  
  if (sumSq == "Error") {
    # abline(h = mean(mydat$dv), lwd = 3, col = "purple")
    
    if ("Segments" %in% input$whatDisplay) {
      
      segments(x0 = 1:nrow(mydat), x1 = 1:nrow(mydat), y0 = predPoints, y1 = mydat$accidents, lwd = 2, col = "red")
      # s <- round(fullModAccSumSq - modAccSumSquares, 3)
      s <- round(modErrorSumSquares, 3)
      if ("Sums" %in% input$whatDisplay) {
        mtext(paste0("Error Sum of Squares = ", s, "\n Mean Square = ", round(s/dfError, 3)), cex = 1.4)
      } else {
        mtext("Model error", cex = 1.8)
      }
      
      if (overlayFullPred) {
        points(x = 1:nrow(mydat), y = myFullMod$fitted.values, pch = 22, bg = "orange", col = "black", cex = 1.35)
        legend("topright", c("Full model predictions", "Current model predictions"), pch = c(22,23), bty = "n", cex = 1.1, col = c("orange", "darkgreen"))
      }
      
    }
    points(x = 1:nrow(mydat), y = predPoints, pch = myShapes, bg = myBgs, col = myCols, cex = 1.35, lwd = 3)
    
  } 
}



# Run the application 
shinyApp(ui = ui, server = server)
