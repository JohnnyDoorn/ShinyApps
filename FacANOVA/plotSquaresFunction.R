
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


# Variance partition (DSUJ Figure 13.6 with the numbers filled in), for the FacANOVA
# applet's partition tab and the lecture 17 slides; copies kept identical like the one
# above. Box widths are proportional to the sums of squares. The bottom row shows the sums
# of squares an ANOVA table reports for the chosen type, plus what that type leaves to no
# single effect ("shared"):
#   Type I   sequential: the factor entered first also gets the shared part (nothing left)
#   Type II  what a main effect adds to the other main effect, as on the BONUS slides, so
#            alone = unique + shared
#   Type III each effect given all other terms (the default here, as in JASP and SPSS)
# In a balanced design all types agree and shared is 0; positive shared is redundancy,
# negative is suppression (the parts then add up to more than the model).
partitionFactorial <- function(data, type = "III") {
  doseLevels <- c("none", "some", "much")
  asDose <- function(x) factor(sub(" \\(.\\)$", "", as.character(x)), levels = doseLevels)
  d <- data.frame(y = data$accidents, alcohol = asDose(data$alcohol), speed = asDose(data$speed))
  ssOf <- function(fit) sum((fitted(fit) - mean(d$y))^2)
  fitFull <- lm(y ~ alcohol * speed, data = d)
  alone <- c(alcohol = ssOf(lm(y ~ alcohol, data = d)), speed = ssOf(lm(y ~ speed, data = d)))
  main  <- ssOf(lm(y ~ alcohol + speed, data = d))
  model <- ssOf(fitFull)
  total <- sum((d$y - mean(d$y))^2)
  unique <- c(alcohol = main - alone[["speed"]], speed = main - alone[["alcohol"]])
  # Type III: drop each term's columns from the sum-to-zero coded full model.
  X <- model.matrix(~ alcohol * speed, data = d, contrasts.arg = list(alcohol = "contr.sum", speed = "contr.sum"))
  rss <- function(keep) sum(lm.fit(X[, keep, drop = FALSE], d$y)$residuals^2)
  type3 <- sapply(1:3, function(t) rss(attr(X, "assign") != t) - rss(TRUE))
  terms <- switch(type,
    "I-alcohol" = c(alcohol = alone[["alcohol"]], speed = unique[["speed"]], interaction = model - main),
    "I-speed"   = c(alcohol = unique[["alcohol"]], speed = alone[["speed"]], interaction = model - main),
    "II"        = c(unique, interaction = model - main),
    "III"       = setNames(type3, c("alcohol", "speed", "interaction")))
  list(total = total, model = model, residual = total - model, main = main, alone = alone,
       unique = unique, shared = sum(alone) - main, type = type, terms = terms,
       sharedType = model - sum(terms),
       df = c(total = nrow(d) - 1, model = fitFull$rank - 1, residual = fitFull$df.residual,
              alcohol = nlevels(d$alcohol) - 1, speed = nlevels(d$speed) - 1),
       cells = table(Alcohol = d$alcohol, Speed = d$speed),
       r = cor(as.numeric(d$alcohol), as.numeric(d$speed)))
}

plotVariancePartitionFactorial <- function(data, type = "III", cex = 1.4) {
  p <- partitionFactorial(data, type)
  fmt <- function(x) sprintf("%.2f", x)
  par(mar = c(0.5, 0.5, 0.5, 0.5))
  plot.new(); plot.window(xlim = c(0, 1.25), ylim = c(-1, 3))
  w <- function(ss) ss / p$total
  
  # Draws a box; writes its label inside when it fits (on two lines, else with the df on a
  # third line), otherwise returns FALSE.
  partBox <- function(x0, x1, y0, y1, fill, border, label, textCol) {
    rect(x0, y0, x1, y1, col = fill, border = border, lwd = 2)
    for (candidate in unique(c(label, sub(", df", "\ndf", label, fixed = TRUE)))) {
      if (strwidth(candidate, cex = cex) < 0.95 * (x1 - x0) && strheight(candidate, cex = cex) < 0.9 * (y1 - y0)) {
        text((x0 + x1) / 2, (y0 + y1) / 2, candidate, col = textCol, cex = cex)
        return(TRUE)
      }
    }
    FALSE
  }
  callouts <- list()
  addCallout <- function(x, label, col, yFrom = 0.15)
    callouts[[length(callouts) + 1]] <<- list(x = x, label = label, col = col, yFrom = yFrom)
  
  partBox(0, 1, 2.15, 2.95, "#7FB6D9", "#3F7FAF",
          sprintf("Total variability\nSS = %s, df = %d", fmt(p$total), p$df[["total"]]), "white")
  xModel <- w(p$model)
  partBox(0, xModel, 1.15, 1.95, "#4A9E7E", "#2C5E4F",
          sprintf("Model (alcohol, speed, interaction)\nSS = %s, df = %d", fmt(p$model), p$df[["model"]]), "white")
  residualLabel <- sprintf("Residual\nSS = %s\ndf = %d", fmt(p$residual), p$df[["residual"]])
  if (!partBox(xModel, 1, 1.15, 1.95, "#DDAA44", "#C0702E", residualLabel, "white"))
    text(1.015, 1.55, residualLabel, adj = 0, col = "#C0702E", cex = cex)
  
  # Bottom row, left to right: alcohol, (shared), speed, interaction. With suppression a
  # dashed line marks where the model ends; the boxes drawn after it cover it where they overlap.
  shared <- p$sharedType
  if (shared < -0.005) segments(xModel, 0, xModel, 1.15, lty = 2, lwd = 2, col = "gray25")
  sharedLabel <- if (shared > 0.005) "Shared (redundancy)" else if (shared < -0.005) "Shared (suppression)" else "Shared"
  firstEntered <- c("I-alcohol" = "alcohol", "I-speed" = "speed")[type]
  parts <- list(
    list(ss = p$terms[["alcohol"]], fill = "#EDE36E", border = "#D4B23A", textCol = "gray20",
         label = sprintf("Alcohol%s\nSS = %s, df = %d", if (identical(unname(firstEntered), "alcohol")) " (entered first)" else "",
                         fmt(p$terms[["alcohol"]]), p$df[["alcohol"]])),
    # Type I leaves nothing over: the shared part sits in the factor entered first.
    if (shared >= 0 && is.na(firstEntered))
      list(ss = shared, fill = "#BDBDBD", border = "gray40", textCol = "gray20",
           label = sprintf("%s\nSS = %s", sharedLabel, fmt(shared))),
    list(ss = p$terms[["speed"]], fill = "#FCF0D9", border = "#C9773A", textCol = "gray20",
         label = sprintf("Speed%s\nSS = %s, df = %d", if (identical(unname(firstEntered), "speed")) " (entered first)" else "",
                         fmt(p$terms[["speed"]]), p$df[["speed"]])),
    list(ss = p$terms[["interaction"]], fill = "#C44E3C", border = "#8E2E22", textCol = "white",
         label = sprintf("Alcohol × speed\nSS = %s, df = %d", fmt(p$terms[["interaction"]]), p$df[["alcohol"]] * p$df[["speed"]])))
  x0 <- 0
  for (part in Filter(Negate(is.null), parts)) {
    x1 <- x0 + w(part$ss)
    if (!partBox(x0, x1, 0.15, 0.95, part$fill, part$border, part$label, part$textCol))
      addCallout((x0 + x1) / 2, gsub("\n", ", ", part$label), if (part$textCol == "white") part$border else "gray20")
    x0 <- x1
  }
  if (shared < -0.005) {
    # Suppression: the unique parts overshoot the model by the (negative) shared part,
    # shown as a hatched strip under the overshoot.
    rect(xModel, 0, xModel + w(-shared), 0.1, density = 20, col = "gray25", border = "gray25")
    addCallout(xModel + w(-shared) / 2, sprintf("%s, SS = %s", sharedLabel, fmt(shared)), "gray25", yFrom = 0)
  }
  
  # Labels that did not fit go below the bottom row, one line each, with a leader line.
  # Labels right of center end at their leader and run top to bottom by increasing x (left
  # of center: start at it, decreasing x), so no leader crosses a label above it.
  xs <- vapply(callouts, `[[`, numeric(1), "x")
  callouts <- callouts[c(order(-xs)[xs[order(-xs)] <= 0.5], order(xs)[xs[order(xs)] > 0.5])]
  for (k in seq_along(callouts)) {
    co <- callouts[[k]]; yText <- -0.25 - 0.3 * (k - 1)
    right <- co$x > 0.5
    segments(co$x, co$yFrom, co$x, yText, col = co$col, lwd = 1.5)
    text(co$x + if (right) -0.01 else 0.01, yText, co$label, adj = if (right) 1 else 0, col = co$col, cex = cex)
  }
  invisible(p)
}
