plotSumSquares <- function(data, input, sumSq = "Total", stats = NULL, plotMean = TRUE, myMain = "",
                           alcColors = NULL, speedSymbols = NULL) {
  
  mydat <- read.csv("anova_alcohol_speed_daytime.csv")
  grandMean <- mean(mydat$accidents)
  nGroups <- 9
  myAlcMod <-  lm(accidents ~ alcohol, data = mydat)
  mySpeedMod <- lm(accidents ~ speed, data = mydat)
  myMainMod <- lm(accidents ~ alcohol + speed, data = mydat)
  myFullMod <- lm(accidents ~ alcohol * speed, data = mydat)
  
  
  plot(mydat$accidents, col = "black" , pch = 21, bg = "blue", 
       cex = 1.8, lwd = 3, las = 1, bty = "n", main = "",
       ylab = "Accidents", xlab = "Participant Nr.", xlim = c(0, length(mydat$accidents)), 
       ylim = c(0, 10), cex.lab = 1.3, cex.axis=1.3)
  totN <- nrow(mydat)
  
  if (input$whatPred == "Alcohol") {
    myMod <-  myAlcMod
    dfMod <- 2
    dfError <- totN - 3
    myCols <- palette.colors(n = 3, palette = "Okabe-Ito")
  } else if (input$whatPred == "Speed") {
    myMod <- mySpeedMod
    dfMod <- 2
    predPoints <- mydat$predictedOnSpeed
    dfError <- totN - 3
    myCols <- palette.colors(n = 1, palette = "Okabe-Ito")
  } else if (input$whatPred == "Alcohol + Speed") {
    myMod <- myMainMod
    dfMod <- 4
    predPoints <- mydat$predictedOnMain
    dfError <- totN - 6
  } else if (input$whatPred == "Alcohol + Speed + A:S") {
    myMod <- myFullMod
    dfMod <- 8
    predPoints <- mydat$predictedOnFull
    dfError <- totN - 9
  } else if (input$whatPred == "Mean") {
    myMod <- lm(accidents ~ 1, data = mydat)
    predPoints <- rep(mean(mydat$accidents), totN)
    modSumSquares <- 0
    dfMod <- totN - (1 + 9)
    dfError <- 1
  }
  
  predPoints <- myMod$fitted.values
  modAccSumSquares <- sum((predPoints - mean(mydat$accidents))^2)
  modAccMeanSquare <- modAccSumSquares / dfMod
  modErrorSumSquares <- sum((predPoints - mydat$accidents)^2)
  
  fullModErrorSumSq <- sum(myFullMod$residuals^2)
  fullModAccSumSq <- sum((myFullMod$fitted.values - mean(mydat$accidents))^2)
  fullModMeanSquareError <- fullModErrorSumSq / (totN - 9)
  nulModError <- sum((mean(mydat$accidents) - mydat$accidents)^2)
  
  if (sumSq == "Total") {
    if (plotMean) {
      abline(h = mean(mydat$accidents), lwd = 3, col = "purple")
    }
    if ("Segments" %in% input$whatDisplay) {
      segments(x0 = mydat$subjects, x1 = mydat$subjects, y0 = mean(mydat$accidents), y1 = mydat$accidents, lwd = 2)
      if ("Sums" %in% input$whatDisplay) {
        mtext(paste0("Total Sum of Squares = ", round(nulModError, 2)), cex = 1.4)
      } else {
        mtext(myMain, cex = 1.8)
      }
    }
  } 
  
  
  if (sumSq == "Model") {
    abline(h = mean(mydat$accidents), lwd = 3, col = "purple")
    points(x = mydat$subjects, y = predPoints, pch = 23, bg = "darkgreen", col = "black", cex = 1.35)
    if ("Segments" %in% input$whatDisplay) {
      segments(x0 = mydat$subjects, x1 = mydat$subjects, y0 = predPoints, y1 = mean(mydat$accidents), lwd = 2, col = "darkgray")
      if ("Sums" %in% input$whatDisplay) {
        if (input$whatPred == "Mean" | !("F-stat" %in% input$whatDisplay)) {
          mtext(paste0("Model Sum of Squares = ", round(modAccSumSquares, 3), "\nMean Square = ", round(modAccSumSquares/dfMod, 3)), cex = 1.4, line = 0)
        } else {
          mtext(paste0("Model Sum of Squares = ", round(modAccSumSquares, 3), "\nMean Square = ", round(modAccSumSquares/dfMod, 3), 
                       "\nF = ",round(modAccSumSquares/dfMod, 3),"/",round(fullModMeanSquareError,3), " = ", round(round(modAccSumSquares/dfMod, 3)/fullModMeanSquareError, 3)) , cex = 1.4, line = 0)
        }
      } else {
        mtext(myMain, cex = 1.8)
      }
    }
  }
  
  
  if (sumSq == "Error") {
    # abline(h = mean(mydat$dv), lwd = 3, col = "purple")
    points(x = mydat$subjects, y = predPoints, pch = 23, bg = "darkgreen", col = "black", cex = 1.35)
    
    if ("Segments" %in% input$whatDisplay) {
      
      segments(x0 = mydat$subjects, x1 = mydat$subjects, y0 = predPoints, y1 = mydat$accidents, lwd = 2, col = "red")
      # s <- round(fullModAccSumSq - modAccSumSquares, 3)
      s <- round(modErrorSumSquares, 3)
      if ("Sums" %in% input$whatDisplay) {
        mtext(paste0("Error Sum of Squares = ", s, "\n Mean Square = ", round(s/dfError, 3)), cex = 1.4)
      } else {
        mtext(myMain, cex = 1.8)
      }
    }
  } 
}