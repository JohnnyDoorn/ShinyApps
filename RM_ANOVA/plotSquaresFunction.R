
# Sums-of-squares plot for the repeated-measures ANOVA applet (RM_ANOVA) and the SSR
# lecture slides (lecture 18). Two identical copies: RM_ANOVA/plotSquaresFunction.R in the
# ShinyApps repo and ssr/plotFunctionsSSR.r in teaching-statistics. Edit one, copy it to
# the other, so the slides and the applet stay one thing.
#
# Long data: one row per observation (accidents, alcohol, pp). Each point is one
# observation, fill = alcohol dose; the bands group the observations by participant (or
# by alcohol dose). The short black lines are the current model's predictions.
plotSumSquaresRM <- function(data, input, sumSq = "Total", myMain = "",
                             sortBy = "participant", overlayFullPred = FALSE,
                             alcColors = palette.colors(n = 4, palette = "Okabe-Ito")[2:4]) {

  doseLevels <- c("none", "some", "much")
  mydat <- data.frame(accidents = data$accidents,
                      alcohol   = factor(sub(" \\(.\\)$", "", as.character(data$alcohol)), levels = doseLevels),
                      pp        = factor(data$pp, levels = sort(unique(data$pp))))
  ord <- if (sortBy == "alcohol") order(mydat$alcohol, mydat$pp) else order(mydat$pp, mydat$alcohol)
  mydat <- mydat[ord, ]
  totN <- nrow(mydat)
  x <- seq_len(totN)
  grandMean <- mean(mydat$accidents)

  myMod <- switch(input$whatPred,
                  "Mean"         = lm(accidents ~ 1, data = mydat),
                  "Alcohol"      = lm(accidents ~ alcohol, data = mydat),
                  "ID"           = lm(accidents ~ pp, data = mydat),
                  "Alcohol + ID" = lm(accidents ~ alcohol + pp, data = mydat))
  # The F-ratio for alcohol uses the residual mean square of the full model (alcohol + ID).
  myFullMod <- lm(accidents ~ alcohol + pp, data = mydat)
  dfMod   <- length(coef(myMod)) - 1
  dfError <- totN - length(coef(myMod))
  predPoints <- unname(fitted(myMod))

  totalSumSquares  <- sum((mydat$accidents - grandMean)^2)
  modSumSquares    <- sum((predPoints - grandMean)^2)
  errorSumSquares  <- sum((mydat$accidents - predPoints)^2)
  fullModMeanSquareError <- sum(residuals(myFullMod)^2) / (totN - length(coef(myFullMod)))

  # Blocks of the factor sorted on: participants (3 observations each) or alcohol doses.
  major      <- if (sortBy == "alcohol") mydat$alcohol else droplevels(mydat$pp)
  majorSizes <- as.vector(table(major))
  majorEnds  <- cumsum(majorSizes)

  plot(x, mydat$accidents, type = "n", las = 1, bty = "n", xaxt = "n",
       xlim = c(0, totN), ylim = c(3, 8), cex.lab = 1.3, cex.axis = 1.3,
       ylab = "Accidents",
       xlab = if (sortBy == "alcohol") "Observations, sorted by alcohol dose" else "Observations, grouped by participant")
  axis(1, at = majorEnds - majorSizes / 2 + 0.5, labels = levels(major), tick = FALSE, cex.axis = 1.3)
  if (sortBy == "alcohol") {
    abline(v = majorEnds[-length(majorEnds)] + 0.5, col = "gray60", lty = 2)
  } else {
    # Shade every other participant, so each person's three observations read as one block.
    shade <- seq(2, length(majorEnds), by = 2)
    rect(majorEnds[shade] - majorSizes[shade] + 0.5, 3, majorEnds[shade] + 0.5, 8, col = "gray93", border = NA)
  }

  showSegments <- "Segments" %in% input$whatDisplay
  predLines <- function(pred, ...) segments(x - 0.45, pred, x + 0.45, pred, ...)

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

  points(x, mydat$accidents, pch = 21, bg = alcColors[mydat$alcohol], col = "black", cex = 1.8, lwd = 1.5)

  if (sumSq %in% c("Model", "Error"))
    predLines(predPoints, lwd = 4)

  legend("topleft", legend = doseLevels, fill = alcColors, title = "Alcohol", bty = "n",
         horiz = TRUE, cex = 1.1)

  if (sumSq == "Error" && overlayFullPred) {
    predLines(unname(fitted(myFullMod)), lwd = 3, lty = 2, col = "#CC79A7")
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
