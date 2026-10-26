# Helpers for the MultiRegression applet (collinearity and interaction, album sales names): simulate the data, fit the hierarchical models,
# build JASP-style tables and draw the plots. They live outside app.R so they can be
# checked on their own (the tables reproduce DSUJ's album sales output, chapter 8).

# ---- data -------------------------------------------------------------------------

# Two standard-normal predictors with correlation r, named after DSUJ's album sales example
# (Adverts, Airplay), and Sales = b1 Adverts + b2 Airplay + b12 Adverts Airplay + e
# with error SD 1, so the effects are in SD units. With exact = TRUE the sample itself has
# mean 0, SD 1 and correlation exactly r (so M2's VIF is exactly 1 / (1 - r^2)); the
# outcome still gets fresh random error.
simulateData <- function(n, r, b1, b2, b12, seed = 1, exact = TRUE) {
  set.seed(seed)
  z1 <- rnorm(n)
  z2 <- rnorm(n)
  if (exact) {
    z1 <- as.numeric(scale(z1))
    z2 <- as.numeric(scale(residuals(lm(z2 ~ z1))))
  }
  x1 <- z1
  x2 <- r * z1 + sqrt(1 - r^2) * z2
  y  <- b1 * x1 + b2 * x2 + b12 * x1 * x2 + rnorm(n)
  data.frame(Sales = y, Adverts = x1, Airplay = x2)
}

# ---- models -----------------------------------------------------------------------

# terms: one character vector of term labels per model, M0 first (character(0)).
fitHierarchy <- function(data, dv, terms) {
  lapply(terms, function(tt) {
    lm(reformulate(if (length(tt)) tt else "1", response = dv), data = data)
  })
}

modelLabel <- function(i) paste0("M", intToUtf8(0x2080 + i))     # M with a subscript digit
termLabel  <- function(x) gsub(":", " \u273B ", x, fixed = TRUE)  # JASP writes X1 * X2

# VIF per predictor column: 1 / (1 - R^2) of that column on the other predictors.
vifs <- function(fit) {
  X <- model.matrix(fit)[, -1, drop = FALSE]
  if (ncol(X) < 2) return(setNames(rep(1, ncol(X)), colnames(X)))
  setNames(sapply(seq_len(ncol(X)), function(j) {
    1 / (1 - summary(lm(X[, j] ~ X[, -j]))$r.squared)
  }), colnames(X))
}

modelSummaryRows <- function(fits) {
  n <- nobs(fits[[1]])
  prevR2 <- 0
  prevK  <- 0
  rows <- vector("list", length(fits))
  for (i in seq_along(fits)) {
    s  <- summary(fits[[i]])
    k  <- length(coef(fits[[i]])) - 1
    r2 <- if (k == 0) 0 else s$r.squared
    df1 <- k - prevK
    df2 <- n - k - 1
    r2c <- r2 - prevR2
    Fc  <- if (df1 > 0) (r2c / df1) / ((1 - r2) / df2) else NA
    rows[[i]] <- data.frame(model = modelLabel(i - 1), R = sqrt(r2), R2 = r2,
                            adjR2 = if (k == 0) 0 else s$adj.r.squared, RMSE = s$sigma,
                            R2c = r2c, Fc = Fc, df1 = df1, df2 = df2,
                            p = if (df1 > 0) pf(Fc, df1, df2, lower.tail = FALSE) else NA)
    prevR2 <- r2
    prevK  <- k
  }
  do.call(rbind, rows)
}

anovaRows <- function(fits) {
  rows <- lapply(seq_along(fits), function(i) {
    fit <- fits[[i]]
    k <- length(coef(fit)) - 1
    if (k == 0) return(NULL)                 # JASP omits the intercept model
    y <- model.response(model.frame(fit))
    n <- length(y)
    ssReg <- sum((fitted(fit) - mean(y))^2)
    ssRes <- sum(residuals(fit)^2)
    F <- (ssReg / k) / (ssRes / (n - k - 1))
    data.frame(model = modelLabel(i - 1), source = c("Regression", "Residual", "Total"),
               SS = c(ssReg, ssRes, ssReg + ssRes), df = c(k, n - k - 1, n - 1),
               MS = c(ssReg / k, ssRes / (n - k - 1), NA),
               F = c(F, NA, NA), p = c(pf(F, k, n - k - 1, lower.tail = FALSE), NA, NA))
  })
  do.call(rbind, rows)
}

coefficientRows <- function(fits) {
  rows <- lapply(seq_along(fits), function(i) {
    fit <- fits[[i]]
    co  <- summary(fit)$coefficients
    X   <- model.matrix(fit)
    y   <- model.response(model.frame(fit))
    v   <- vifs(fit)
    terms <- rownames(co)
    data.frame(model = modelLabel(i - 1), term = termLabel(terms),
               b = co[, 1], SE = co[, 2],
               beta = ifelse(terms == "(Intercept)", NA, co[, 1] * apply(X, 2, sd) / sd(y)),
               t = co[, 3], p = co[, 4],
               tolerance = ifelse(terms == "(Intercept)", NA, 1 / v[terms]),
               VIF = ifelse(terms == "(Intercept)", NA, v[terms]),
               row.names = NULL)
  })
  do.call(rbind, rows)
}

# Part (semi-partial) and partial correlations from t: partial = t / sqrt(t^2 + df),
# part = t * sqrt((1 - R^2) / df), df = residual df of that model.
partPartialRows <- function(fits) {
  rows <- lapply(seq_along(fits), function(i) {
    fit <- fits[[i]]
    co  <- summary(fit)$coefficients
    if (nrow(co) < 2) return(NULL)
    co  <- co[-1, , drop = FALSE]
    df  <- fit$df.residual
    r2  <- summary(fit)$r.squared
    t   <- co[, 3]
    data.frame(model = modelLabel(i - 1), term = termLabel(rownames(co)),
               partial = t / sqrt(t^2 + df), part = t * sqrt((1 - r2) / df), row.names = NULL)
  })
  do.call(rbind, rows)
}

# ---- JASP-style HTML tables ---------------------------------------------------------

jaspTableCss <- "
.jasp-table { font-family: 'Helvetica Neue', Arial, sans-serif; font-size: 14px; margin: 14px 0 26px; }
.jasp-title { font-weight: bold; margin-bottom: 6px; }
.jasp-table table { border-collapse: collapse; }
.jasp-table th, .jasp-table td { padding: 3px 12px; text-align: right; white-space: nowrap; }
.jasp-table th { font-weight: normal; }
.jasp-table thead tr:first-child th { border-top: 2px solid #000; }
.jasp-table thead tr:last-child th { border-bottom: 1px solid #000; }
.jasp-table th.over { text-align: center; border-bottom: 1px solid #000; }
.jasp-table tbody tr:last-child td { border-bottom: 2px solid #000; }
.jasp-table tr.group td { border-top: 1px solid #bbb; }
.jasp-table tbody tr:first-child td { border-top: none; }
.jasp-table .left { text-align: left; }
.jasp-foot { font-size: 12px; margin-top: 4px; }
"

fmt  <- function(x, dp = 3) ifelse(is.na(x), "", formatC(x, format = "f", digits = dp))
fmtP <- function(p) ifelse(is.na(p), "", ifelse(p < .001, "&lt; .001", formatC(p, format = "f", digits = 3)))
fmtI <- function(x) ifelse(is.na(x), "", as.character(x))

# cells: data.frame of formatted strings; the first column is the model, shown only on the
# first row of each model (JASP's combined cells), with a thin line between models.
jaspTable <- function(title, header, cells, left = 1, over = NULL, notes = NULL) {
  first <- c(TRUE, cells[[1]][-1] != cells[[1]][-nrow(cells)])
  cells[[1]][!first] <- ""
  cls <- function(j) if (j %in% left) " class=\"left\"" else ""
  head1 <- if (!is.null(over)) {
    paste0("<tr>", paste(rep("<th></th>", ncol(cells) - over$span), collapse = ""),
           "<th class=\"over\" colspan=\"", over$span, "\">", over$title, "</th></tr>")
  } else ""
  head2 <- paste0("<tr>", paste0("<th", sapply(seq_along(header), cls), ">", header, "</th>", collapse = ""), "</tr>")
  body <- vapply(seq_len(nrow(cells)), function(i) {
    paste0("<tr", if (first[i] && i > 1) " class=\"group\"" else "", ">",
           paste0("<td", sapply(seq_len(ncol(cells)), cls), ">", unlist(cells[i, ]), "</td>", collapse = ""),
           "</tr>")
  }, character(1))
  foot <- if (length(notes)) paste0("<div class=\"jasp-foot\">", paste0("<i>Note.</i> ", notes, collapse = "<br/>"), "</div>") else ""
  paste0("<div class=\"jasp-table\"><div class=\"jasp-title\">", title, "</div><table><thead>",
         head1, head2, "</thead><tbody>", paste(body, collapse = ""), "</tbody></table>", foot, "</div>")
}

modelNotes <- function(terms) {
  vapply(seq_along(terms)[-1], function(i) {
    paste0(modelLabel(i - 1), " includes ", paste(termLabel(terms[[i]]), collapse = ", "))
  }, character(1))
}

jaspTablesHtml <- function(fits, terms, dv = "Y") {
  ms <- modelSummaryRows(fits)
  t1 <- jaspTable(paste0("Model Summary - ", dv),
                  c("Model", "R", "R\u00B2", "Adjusted R\u00B2", "RMSE", "R\u00B2 Change", "F Change", "df1", "df2", "p"),
                  data.frame(ms$model, fmt(ms$R), fmt(ms$R2), fmt(ms$adjR2), fmt(ms$RMSE), fmt(ms$R2c),
                             fmt(ms$Fc), fmtI(ms$df1), fmtI(ms$df2), fmtP(ms$p)),
                  notes = modelNotes(terms))
  an <- anovaRows(fits)
  t2 <- jaspTable("ANOVA", c("Model", "", "Sum of Squares", "df", "Mean Square", "F", "p"),
                  data.frame(an$model, an$source, fmt(an$SS), fmtI(an$df), fmt(an$MS), fmt(an$F), fmtP(an$p)),
                  left = 1:2,
                  notes = c(modelNotes(terms), "The intercept model is omitted, as no meaningful information can be shown."))
  co <- coefficientRows(fits)
  t3 <- jaspTable("Coefficients",
                  c("Model", "", "Unstandardized", "Standard Error", "Standardized", "t", "p", "Tolerance", "VIF"),
                  data.frame(co$model, co$term, fmt(co$b), fmt(co$SE), fmt(co$beta), fmt(co$t), fmtP(co$p),
                             fmt(co$tolerance), fmt(co$VIF)),
                  left = 1:2, over = list(title = "Collinearity Statistics", span = 2))
  pp <- partPartialRows(fits)
  t4 <- jaspTable("Part And Partial Correlations", c("Model", "", "Partial", "Part"),
                  data.frame(pp$model, pp$term, fmt(pp$partial), fmt(pp$part)), left = 1:2)
  paste(t1, t2, t3, t4)
}

# ---- plots ------------------------------------------------------------------------

# Sampling distribution of b for Adverts under H0 (b = 0), the one JASP's t-test uses: centred
# on 0 with this sample's SE and t(df), so t = b / SE and p match the Coefficients table. Drawn
# like the continuous NHST applet; as on the p-value slides there is no alpha or rejection region,
# and the observed b plus everything more extreme is black (the p-value). Same x-axis in both
# panels, so a wider null distribution in M2 (collinearity) is visible directly.
plotSamplingNull <- function(fit1, fit2) {
  fits <- list(fit1, fit2)
  b    <- sapply(fits, function(f) coef(f)[["Adverts"]])
  se   <- sapply(fits, function(f) summary(f)$coefficients["Adverts", 2])
  df   <- sapply(fits, function(f) f$df.residual)
  tt   <- b / se
  p    <- 2 * pt(-abs(tt), df)
  lim  <- max(abs(b), 4 * se) * 1.05
  x    <- seq(-lim, lim, length.out = 1e3)
  dens <- sapply(1:2, function(j) dt(x / se[j], df[j]) / se[j])
  yMax <- max(dens) * 1.2
  titles <- expression(bold("b for Adverts in" ~ M[1] ~ "under" ~ H[0]),
                       bold("b for Adverts in" ~ M[2] ~ "under" ~ H[0]))

  par(mfrow = c(1, 2), cex = 0.8, cex.lab = 1.2, cex.main = 1.2, mar = c(5, 5, 5, 1))
  for (j in 1:2) {
    plot(x, dens[, j], type = "h", lwd = 2, col = ifelse(abs(x) >= abs(b[j]), "black", "darkgreen"),
         bty = "n", las = 1, ylim = c(0, yMax), xlab = "b for Adverts", ylab = "Density", main = titles[j])
    abline(v = b[j], lwd = 2, lty = 2)
    mtext(paste0("SE = ", fmt(se[j]), ",  t = ", fmt(tt[j]), ",  p ",
                 if (p[j] < .001) "< .001" else paste0("= ", fmt(p[j]))), side = 3, line = 0.2, cex = 0.9)
    legend("topleft", c("Observed b", "As or more extreme (p)"), lty = c(2, NA), lwd = c(2, NA),
           pch = c(NA, 15), pt.cex = 2, col = "black", bty = "n", cex = 0.8)
  }
}

# JASP's matrix plot: distributions on the diagonal, scatter plots above it and the
# correlation with its 95% CI below it.
plotMatrix <- function(data, vars = c("Sales", "Adverts", "Airplay")) {
  k <- length(vars)
  par(mfrow = c(k, k), mar = c(3.5, 3.5, 1.5, 1), cex = 0.9, mgp = c(2.2, 0.7, 0))
  for (row in seq_len(k)) for (col in seq_len(k)) {
    x <- data[[vars[col]]]
    y <- data[[vars[row]]]
    if (row == col) {
      h <- hist(x, breaks = "Sturges", plot = FALSE)
      d <- density(x)
      plot(h, freq = FALSE, col = "grey85", border = "grey40", las = 1, main = vars[col],
           xlab = vars[col], ylab = "Density", ylim = c(0, max(h$density, d$y) * 1.1))
      lines(d, lwd = 2)
    } else if (row < col) {
      plot(x, y, pch = 21, bg = "grey70", col = "grey30", bty = "n", las = 1,
           xlab = vars[col], ylab = vars[row])
      abline(lm(y ~ x), lwd = 2)
    } else {
      ct <- cor.test(x, y)
      plot.new()
      text(0.5, 0.62, paste0("r = ", fmt(ct$estimate)), cex = 1.6)
      text(0.5, 0.38, paste0("95% CI: [", fmt(ct$conf.int[1]), ", ", fmt(ct$conf.int[2]), "]"), cex = 1.1)
    }
  }
}

# Interaction is not collinearity. Left: the predictors against each other (collinearity).
# Right: Y against X1, coloured by X2, with the slope of X1 at three values of X2 from the
# interaction model (simple slopes) and at the value `at` chosen in the app.
plotInteraction <- function(data, at = 0) {
  fit <- lm(Sales ~ Adverts * Airplay, data = data)
  b   <- coef(fit)
  r   <- cor(data$Adverts, data$Airplay)
  par(mfrow = c(1, 2), cex = 0.8, cex.lab = 1.2, cex.main = 1.2, mar = c(5, 5, 6, 1))

  plot(data$Adverts, data$Airplay, pch = 21, bg = "grey70", col = "grey30", bty = "n", las = 1,
       xlab = "Adverts", ylab = "Airplay")
  abline(lm(Airplay ~ Adverts, data = data), lwd = 2)
  title("Collinearity: are Adverts and Airplay related?", line = 4)
  mtext(paste0("r = ", fmt(r), ",  VIF = ", fmt(1 / (1 - r^2))), side = 3, line = 1.5, cex = 1)

  pal  <- colorRampPalette(c("steelblue", "grey85", "darkorange"))(101)
  z    <- (data$Airplay - mean(data$Airplay)) / sd(data$Airplay)
  fill <- pal[round((pmin(pmax(z, -2), 2) + 2) / 4 * 100) + 1]
  plot(data$Adverts, data$Sales, pch = 21, bg = fill, col = "grey30", bty = "n", las = 1, xlab = "Adverts", ylab = "Sales")
  w    <- mean(data$Airplay) + c(-1, 0, 1) * sd(data$Airplay)
  cols <- c("steelblue", "grey40", "darkorange")
  for (k in 1:3) abline(b[1] + b[3] * w[k], b[2] + b[4] * w[k], col = cols[k], lwd = 3)
  abline(b[1] + b[3] * at, b[2] + b[4] * at, lwd = 2, lty = 2)
  title("Interaction: does the slope of Adverts depend on Airplay?", line = 4)
  pInt <- summary(fit)$coefficients[4, 4]
  mtext(paste0("Interaction in the sample: b = ", fmt(b[4]), ", p ",
               if (pInt < .001) "< .001" else paste0("= ", fmt(pInt))), side = 3, line = 1.5, cex = 1)
  legend("topleft", bty = "n", cex = 0.8, lwd = c(3, 3, 3, 2), lty = c(1, 1, 1, 2),
         col = c(cols, "black"),
         legend = c(paste0("Airplay = -1 SD: slope ", fmt(b[2] + b[4] * w[1], 2)),
                    paste0("Airplay = mean: slope ",  fmt(b[2] + b[4] * w[2], 2)),
                    paste0("Airplay = +1 SD: slope ", fmt(b[2] + b[4] * w[3], 2)),
                    paste0("Airplay = ", at, ": slope ", fmt(b[2] + b[4] * at, 2))))
}

# Ported from the earlier MultiRegression applet (plotSumSquares): total, explained and
# unexplained variance per participant, against the null model (the mean) or, for the full
# model, against the main effects model. Only the first plotN participants are drawn; the
# sums use everyone.
plotSumSquares <- function(outcome, prediction, nullPredictions, display, mainNull, plotN = 50) {
  plotN <- min(plotN, length(outcome))
  par(mfrow = c(1, 3), cex = 1.1, mar = c(5, 4.5, 6, 1))

  totSumSq <- sum((outcome - nullPredictions)^2)
  modSumSq <- sum((prediction - nullPredictions)^2)
  errSumSq <- sum((prediction - outcome)^2)
  yl <- range(outcome[1:plotN], prediction[1:plotN], nullPredictions[1:plotN])
  idx <- 1:plotN

  drawNull <- function() {
    if (mainNull) points(idx, nullPredictions[idx], pch = 23, bg = "purple", cex = 1.5)
    else abline(h = mean(outcome), lwd = 2, col = "purple")
  }

  plot(idx, outcome[idx], xlab = "Participant", ylab = "Outcome", las = 1, ylim = yl,
       bty = "n", pch = 21, bg = "turquoise")
  title("Total Variance", line = 4)
  drawNull()
  points(idx, prediction[idx], pch = 23, bg = "darkred")
  if ("Segments" %in% display) segments(idx, outcome[idx], idx, nullPredictions[idx], col = "orange", lwd = 2)
  legend("topleft", c("Observed", "Predicted", "Null"), bty = "n", fill = c("turquoise", "darkred", "purple"))
  if ("Sums" %in% display) mtext(paste0("Total Sum of Squares = ", round(totSumSq, 2)), cex = 1.1, line = 2)

  plot(idx, outcome[idx], xlab = "Participant", ylab = "Outcome", las = 1, ylim = yl,
       bty = "n", pch = 21, bg = "turquoise")
  title("Explained Variance", line = 4)
  drawNull()
  if ("Segments" %in% display) segments(idx, prediction[idx], idx, nullPredictions[idx], col = "blue", lwd = 2)
  points(idx, prediction[idx], pch = 23, bg = "darkred")
  legend("topleft", c("Observed", "Predicted", "Null"), bty = "n", fill = c("turquoise", "darkred", "purple"))
  if ("Sums" %in% display) mtext(paste0("Model Sum of Squares = ", round(modSumSq, 2)), cex = 1.1, line = 2)
  if ("R2" %in% display) mtext(paste0("Proportion = ", round(modSumSq / totSumSq, 2)), cex = 1.1, line = 0.5)

  plot(idx, outcome[idx], xlab = "Participant", ylab = "Outcome", las = 1, ylim = yl,
       bty = "n", pch = 21, bg = "turquoise")
  title("Unexplained Variance", line = 4)
  if ("Segments" %in% display) segments(idx, prediction[idx], idx, outcome[idx], col = "red", lwd = 2)
  points(idx, prediction[idx], pch = 23, bg = "darkred")
  legend("topleft", c("Observed", "Predicted"), bty = "n", fill = c("turquoise", "darkred"))
  if ("Sums" %in% display) mtext(paste0("Residual Sum of Squares = ", round(errSumSq, 2)), cex = 1.1, line = 2)
}

# The data in 3D with the regression plane of M2 (flat) or M3 (twisted when there is an
# interaction). plotly rather than rgl (as on the L21 slide), because rgl needs OpenGL
# libraries on the server.
plot3D <- function(data, model = "full") {
  fit <- if (model == "full") lm(Sales ~ Adverts * Airplay, data = data) else lm(Sales ~ Adverts + Airplay, data = data)
  g1  <- seq(min(data$Adverts), max(data$Adverts), length.out = 30)
  g2  <- seq(min(data$Airplay), max(data$Airplay), length.out = 30)
  z   <- outer(g1, g2, function(a, b) predict(fit, newdata = data.frame(Adverts = a, Airplay = b)))
  p <- plotly::plot_ly()
  p <- plotly::add_markers(p, x = data$Adverts, y = data$Airplay, z = data$Sales, name = "Albums",
                           marker = list(size = 3, color = "grey", line = list(color = "black", width = 1)))
  p <- plotly::add_surface(p, x = g1, y = g2, z = t(z), opacity = 0.7, showscale = FALSE, name = "Model",
                           colorscale = list(c(0, 1), c("darkgreen", "darkgreen")))
  plotly::layout(p, showlegend = FALSE, margin = list(l = 0, r = 0, b = 0, t = 0),
                 scene = list(xaxis = list(title = "Adverts"), yaxis = list(title = "Airplay"),
                              zaxis = list(title = "Sales"), aspectmode = "cube",
                              camera = list(eye = list(x = -1.3, y = -1.45, z = 0.6))))
}
