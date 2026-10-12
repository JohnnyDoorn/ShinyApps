
# Sums-of-squares plot for the factorial ANOVA applet (FacANOVA) and the SSR lecture
# slides (lecture 17). Two identical copies: FacANOVA/plotSquaresFunction.R in the
# ShinyApps repo and ssr/plotFunctionsSSR.r in teaching-statistics. Edit one, copy it
# to the other, so the slides and the applet stay one thing.
#
# Each point is one participant: fill = alcohol dose, shape = speed dose. The black
# step line is the current model's prediction (one value per cell of 20 participants).
plotSumSquaresFactorial <- function(data, input, sumSq = "Total", myMain = "",
                                    sortBy = "alcohol", overlayFullPred = FALSE,
                                    alcColors = palette.colors(n = 4, palette = "Okabe-Ito")[2:4],
                                    speedSymbols = c(21, 22, 23)) {

  # The lecture labels levels as "none (A)" etc.; strip that so both data sets work.
  doseLevels <- c("none", "some", "much")
  asDose <- function(x) factor(sub(" \\(.\\)$", "", as.character(x)), levels = doseLevels)
  mydat <- data.frame(alcohol   = asDose(data$alcohol),
                      speed     = asDose(data$speed),
                      accidents = data$accidents)
  ord <- if (sortBy == "speed") order(mydat$speed, mydat$alcohol) else order(mydat$alcohol, mydat$speed)
  mydat <- mydat[ord, ]
  totN <- nrow(mydat)
  x <- seq_len(totN)
  grandMean <- mean(mydat$accidents)

  myMod <- switch(input$whatPred,
                  "Mean"                  = lm(accidents ~ 1, data = mydat),
                  "Alcohol"               = lm(accidents ~ alcohol, data = mydat),
                  "Speed"                 = lm(accidents ~ speed, data = mydat),
                  "Alcohol + Speed"       = lm(accidents ~ alcohol + speed, data = mydat),
                  "Alcohol + Speed + A:S" = lm(accidents ~ alcohol * speed, data = mydat))
  # Every F-ratio uses the residual mean square of the full model.
  myFullMod <- lm(accidents ~ alcohol * speed, data = mydat)
  dfMod   <- length(coef(myMod)) - 1
  dfError <- totN - length(coef(myMod))
  predPoints <- unname(fitted(myMod))

  totalSumSquares  <- sum((mydat$accidents - grandMean)^2)
  modSumSquares    <- sum((predPoints - grandMean)^2)
  errorSumSquares  <- sum((mydat$accidents - predPoints)^2)
  fullModMeanSquareError <- sum(residuals(myFullMod)^2) / (totN - length(coef(myFullMod)))

  # Cells (blocks of identical alcohol + speed) and the blocks of the factor sorted on.
  cellRuns   <- rle(paste(mydat$alcohol, mydat$speed))
  cellEnds   <- cumsum(cellRuns$lengths)
  cellStarts <- cellEnds - cellRuns$lengths + 1
  majorSizes <- as.vector(table(if (sortBy == "speed") mydat$speed else mydat$alcohol))
  majorEnds  <- cumsum(majorSizes)

  plot(x, mydat$accidents, type = "n", las = 1, bty = "n", xaxt = "n",
       xlim = c(0, totN), ylim = c(0, 10), cex.lab = 1.3, cex.axis = 1.3,
       ylab = "Accidents", xlab = paste0("Participants, sorted by ", sortBy, " dose"))
  axis(1, at = majorEnds - majorSizes / 2 + 0.5, labels = doseLevels, tick = FALSE, cex.axis = 1.3)
  abline(v = cellEnds[-length(cellEnds)] + 0.5, col = "gray85", lty = 3)
  abline(v = majorEnds[-length(majorEnds)] + 0.5, col = "gray60", lty = 2, lwd = 1.5)

  showSegments <- "Segments" %in% input$whatDisplay
  stepLine <- function(pred, ...) segments(cellStarts - 0.5, pred[cellStarts], cellEnds + 0.5, pred[cellStarts], ...)

  if (sumSq %in% c("Total", "Model"))
    abline(h = grandMean, lwd = 3, col = "purple")

  if (showSegments) {
    if (sumSq == "Total")
      segments(x0 = x, x1 = x, y0 = grandMean, y1 = mydat$accidents, lwd = 2)
    if (sumSq == "Model")
      segments(x0 = x, x1 = x, y0 = predPoints, y1 = grandMean, lwd = 2, col = "darkgray")
    if (sumSq == "Error")
      segments(x0 = x, x1 = x, y0 = predPoints, y1 = mydat$accidents, lwd = 2, col = "red")
  }

  points(x, mydat$accidents, pch = speedSymbols[mydat$speed], bg = alcColors[mydat$alcohol],
         col = "black", cex = 1.8, lwd = 1.5)

  if (sumSq %in% c("Model", "Error"))
    stepLine(predPoints, lwd = 4)

  legAlc <- legend("topleft", legend = doseLevels, fill = alcColors, title = "Alcohol", bty = "n", cex = 1.1)
  legend(legAlc$rect$left + legAlc$rect$w, legAlc$rect$top, legend = doseLevels, pch = speedSymbols,
         pt.bg = "white", pt.cex = 1.5, title = "Speed", bty = "n", cex = 1.1)

  if (sumSq == "Error" && overlayFullPred) {
    stepLine(unname(fitted(myFullMod)), lwd = 3, lty = 2, col = "#CC79A7")
    legend("bottomright", c("Full model predictions", "Current model predictions"), lwd = c(3, 4),
           lty = c(2, 1), col = c("#CC79A7", "black"), bty = "n", cex = 1.1)
  }

  if (showSegments) {
    if ("Sums" %in% input$whatDisplay) {
      if (sumSq == "Total") {
        sumsText <- paste0("Total Sum of Squares = ", round(totalSumSquares, 2))
      } else if (sumSq == "Model") {
        sumsText <- paste0("Model Sum of Squares = ", round(modSumSquares, 3))
        if (dfMod > 0) {
          modMeanSquare <- modSumSquares / dfMod
          sumsText <- paste0(sumsText, "\nMean Square = ", round(modMeanSquare, 3))
          if ("F-stat" %in% input$whatDisplay)
            sumsText <- paste0(sumsText, "\nF = ", round(modMeanSquare, 3), "/", round(fullModMeanSquareError, 3),
                               " = ", round(modMeanSquare / fullModMeanSquareError, 3))
        }
      } else {
        sumsText <- paste0("Residual Sum of Squares = ", round(errorSumSquares, 3),
                           "\nMean Square = ", round(errorSumSquares / dfError, 3))
      }
      mtext(sumsText, cex = 1.4, line = 0)
    } else {
      defaultMain <- c(Total = "", Model = "Model improvement", Error = "Model residual")[[sumSq]]
      mtext(if (nzchar(myMain)) myMain else defaultMain, cex = 1.8)
    }
  }
}
