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


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("The Squares"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("nGroups",
                  "n categories:",
                  min = 2,
                  max = 6,
                  step = 1,
                  value = 2),
      sliderInput("groupN",
                  "Sample size per group (n):",
                  min = 2,
                  max = 50,
                  step = 1,
                  value = 5),
      sliderInput("diffGroups",
                  "Difference in groups:",
                  min = 0,
                  max = 5,
                  step = .1,
                  value = 1),
      radioButtons("whatPred",
                   "What do we predict with?",
                   c("Mean", "Group means"), selected = "Mean"),
      checkboxInput("showComparison", "Show model improvement", value = FALSE),
      checkboxGroupInput("whatDisplay",
                         "Display:",
                         c("Segments", "Sums", "F-stat"))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotOutput("distPlot")),
        tabPanel("Table", tableOutput("squaresTable")),
        tabPanel("Sampling distribution", plotOutput("sampDistPlot"))
      ),
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  myReactive <- reactive({
    
    set.seed(112)
    ncp <- 0
    condNames <- LETTERS[1:input$nGroups]
    myCols <- palette.colors(n = input$nGroups+1, palette = "Okabe-Ito")
    
    groupOneMean <- 5
    # Generate data for a factorial design with three groups
    groups <- factor(rep(1:input$nGroups, each = input$groupN))
    dv <- rnorm(n = input$nGroups * input$groupN, 
                mean = groupOneMean, 
                sd = 1)
    groupEffect <- rep((0:(input$nGroups-1)) * (input$diffGroups / (input$nGroups-1)), each = input$groupN)
    dv <- round(dv - rep(tapply(dv, groups, mean), each = input$groupN)  + groupEffect , 1)
    totN <- length(dv)
    
    data <- data.frame(
      pp = (1:totN),
      group = groups,
      dv = groupOneMean + dv
    )
    # write.csv(data, "data.csv")
    totN <- length(data$dv)
    # Calculate the grand mean
    grandMean <- mean(data$dv)
    
    # Calculate the group mean
    groupMeans <- tapply(data$dv, data$group, mean)
    
    # Decompose distances into model error and predictive accuracy
    nulModelError <- data$dv - grandMean
    altModelError <- data$dv - groupMeans[as.numeric(data$group)]
    modelAccuracy <- groupMeans[as.numeric(data$group)] - grandMean
    
    modelMS <- (sum(modelAccuracy^2) / (input$nGroups - 1) ) # model variance
    errorMS <- (sum(altModelError^2) /  (totN - input$nGroups))  # error variance
    totalMS <- (sum(nulModelError^2) / (totN - 1)) # total variance
    
    expVar <- (sum(modelAccuracy^2)) / (sum(nulModelError^2))
    
    dfTreat <- input$nGroups -1
    dfError <-  (input$groupN*input$nGroups) - input$nGroups
    
    fVal <- modelMS / errorMS
    
    myMetrics <- data.frame('Total Sum of Squares' = sum(nulModelError^2),
                            'Model Sum of Squares' = sum(modelAccuracy^2),
                            'Error Sum of Squares' = sum(altModelError^2),
                            'Model df' = dfTreat,
                            'Error df' = dfError,
                            'Model Mean Squares' = modelMS,
                            'Error Mean Squares' = errorMS,
                            'F' = fVal)
    
    return(list(data = data, myMetrics = myMetrics))
  })
  
  output$distPlot <- renderPlot({
    
    par(mfrow = c(1, 3), cex = 1.5)
    if ("Mean" == input$whatPred) {
      plotSumSquares(myReactive()[["data"]], input = input, "Total", stats = myReactive()[["myMetrics"]])
    } else {
      plotSumSquares(myReactive()[["data"]], input = input, "Total", stats = myReactive()[["myMetrics"]], plotMean = FALSE)
    }
    if ("Group means" == input$whatPred) {
      plotSumSquares(myReactive()[["data"]], input = input, "Error", stats = myReactive()[["myMetrics"]])
      if (input$showComparison) {
        plotSumSquares(myReactive()[["data"]], input = input, "Model", stats = myReactive()[["myMetrics"]])
      } else {
        plot.new()
      }
    } else {
      plot.new()
    }
    
  }, width = 1200, height = 800)
  
  output$squaresTable <- renderTable(myReactive()[["myMetrics"]], digits = 2)
  
  output$sampDistPlot <- renderPlot({
    

    fVal <- myReactive()[["myMetrics"]][["F"]]
    ncp <- 0
    xVals <- seq(0.1, 10, length.out = 1e3)
    dfTreat <- myReactive()[["myMetrics"]][["Model.df"]]
    dfError <- myReactive()[["myMetrics"]][["Error.df"]]
    myAlpha <- 0.05

    rightAbLineLoc <- qf(1-myAlpha,df1 = dfTreat, df2 = dfError, lower.tail = TRUE, ncp = 0) 
    rightArea <- round(pf(rightAbLineLoc, df1 = dfTreat, df2 = dfError, lower.tail = FALSE, ncp = ncp), 3)
    
    twoCols <- c("darkgreen", "purple")
    allCols <- ifelse(xVals < rightAbLineLoc, "darkgreen", "purple")
    
    
    par(cex = 1.4, cex.lab = 1.6)
    densValues <- df(xVals, df1 = dfTreat, df2 = dfError)
    
    plot(xVals, densValues, 
         col  = allCols, 
         type = "h",
         lwd = 2,
         las = 1,
         ylab = "Probability",
         xlab = "F-Statistic",
         ylim = c(0, 1),
         xlim = c(0, max(xVals)),
         main = paste0('Sampling Distribution \nF = ', round(fVal, 4)) ,
         bty = "n"
    )
    
    
    ablineclip(v = rightAbLineLoc, lwd = 2, lty = 2, y1 = 0, y2 = 0.9)
    text(rightAbLineLoc, 0.95, paste0("Critical F = ",round(rightAbLineLoc,2)), cex = 1)
    
    
    arrows(x0 = fVal, x1 = fVal, y0 = 0, y1 = 0.65, lwd = 4, col = "darkblue")
    exVals <- xVals[abs(xVals) > abs(fVal)]
    
    thisP <- round(pf(fVal, df1 = dfTreat, df2 = dfError, lower.tail = FALSE, ncp = 0), 3)
    text(fVal, 0.68, paste0("p = ",thisP), cex = 2)
    lines(exVals, df(exVals,  df1 = dfTreat, df2 = dfError, ncp = 0), type = "h")
  },
  width = 800, height = 800)
}

darken_color <- function(color, factor = 3) {
  col_rgb <- col2rgb(color)
  darkened_rgb <- col_rgb / factor
  rgb(darkened_rgb[1,], darkened_rgb[2,], darkened_rgb[3,], maxColorValue = 255)
}

plotSumSquares <- function(data, input, sumSq = "Total", stats = NULL, plotMean = TRUE) {
  
  myCols <- palette.colors(n = input$nGroups+1, palette = "Okabe-Ito")
  # Darkening the colors
  darkCols <- sapply(myCols, darken_color)
  
  plot(data$dv, col = "black" , pch = 21, bg = myCols[as.numeric(data$group)+1], cex = 1.8, lwd = 3, las = 1, bty = "n", 
       ylab = "Score", xlab = "Participant Nr.", xlim = c(0, length(data$dv)), ylim = c(2, 10), cex.lab = 1.3, cex.axis=1.3)

  totN <- length(data$dv)
  # Calculate the grand mean
  grandMean <- mean(data$dv)
  
  # Calculate the group mean
  groupMeans <- tapply(data$dv, data$group, mean)
  
  # Decompose distances into model error and predictive accuracy
  nulModelError <- data$dv - grandMean
  altModelError <- data$dv - groupMeans[as.numeric(data$group)]
  modelAccuracy <- groupMeans[as.numeric(data$group)] - grandMean
  
  modelMS <- (sum(modelAccuracy^2) / (input$nGroups - 1) ) # model variance
  errorMS <- (sum(altModelError^2) /  (totN - input$nGroups))  # error variance
  totalMS <- (sum(nulModelError^2) / (totN - 1)) # total variance
  
  expVar <- (sum(modelAccuracy^2)) / (sum(nulModelError^2))
  fullModMeanSquareError <- round(stats[['Error.Sum.of.Squares']] / (totN - (input$nGroups)), 3)

  if (sumSq == "Total") {
    abline(h = mean(data$dv), lwd = 3, col = "purple")
    if ("Segments" %in% input$whatDisplay) {
      segments(x0 = data$pp, x1 = data$pp, y0 = mean(data$dv), y1 = data$dv, lwd = 2)
      if ("Sums" %in% input$whatDisplay) {
        mtext(paste0("Total Sum of Squares = ", round(sum(nulModelError^2), 2)), cex = 1.8)
      } else {
        mtext("", cex = 1.8)
      }
    }
  } 
  
  if (input$whatPred == "Group means") {
    predPoints <- data$predictedOnGroup
    modSumSquares <- stats[['Model.Sum.of.Squares']]
    dfMod <- input$nGroups - 1
    dfError <- totN - (input$nGroups)
  } else if (input$whatPred == "Mean") {
    predPoints <- rep(grandMean, totN)
    modSumSquares <- 0
    dfMod <- totN - (1 + input$nGroups)
    dfError <- 1
  }
  
  predPoints <- groupMeans[data$group]
  
  if (sumSq == "Model") {
    abline(h = mean(data$dv), lwd = 3, col = "purple")
    for (i in 1:input$nGroups) {
      ablineclip(x1 = which(data$group == levels(data$group)[i])[1]-0.65,
                 x2 = which(data$group == levels(data$group)[i])[input$groupN]+0.65,
                 h = groupMeans[i], lwd = 7, col = "black")
      ablineclip(x1 = which(data$group == levels(data$group)[i])[1]-0.5,
                 x2 = which(data$group == levels(data$group)[i])[input$groupN]+0.5,
                 h = groupMeans[i], lwd = 3, col = myCols[i+1])
    }
    # points(x = data$pp, y = predPoints, pch = 23, bg = darkCols[as.numeric(data$group)+1], col = "black", cex = 1.35)
    
    if ("Segments" %in% input$whatDisplay) {
      segments(x0 = data$pp, x1 = data$pp, y0 = predPoints, y1 = mean(data$dv), lwd = 2, col = myCols[as.numeric(data$group)+1])
      if ("Sums" %in% input$whatDisplay) {
        if (input$whatPred == "Mean" | !("F-stat" %in% input$whatDisplay)) {
          mtext(paste0("Model Sum of Squares = ", round(modSumSquares, 3)), cex = 1.4, line = 0)
        } else {
          mtext(paste0("Model Sum of Squares = ", round(modSumSquares, 3), 
                       "\nMean Square = ", round(modSumSquares/dfMod, 3), 
                       "\nF = ",round(modSumSquares/dfMod, 3),"/",fullModMeanSquareError, 
                       " = ", round(round(modSumSquares/dfMod, 3)/fullModMeanSquareError, 3)) , cex = 1.4, line = 0)
        }
      } else {
        mtext("Model improvement", cex = 1.8)
      }
    }
  }
  
  
  if (sumSq == "Error") {
    for (i in 1:input$nGroups) {
      ablineclip(x1 = which(data$group == levels(data$group)[i])[1]-0.65,
                 x2 = which(data$group == levels(data$group)[i])[input$groupN]+0.65,
                 h = groupMeans[i], lwd = 7, col = "black")
      ablineclip(x1 = which(data$group == levels(data$group)[i])[1]-0.5,
                 x2 = which(data$group == levels(data$group)[i])[input$groupN]+0.5,
                 h = groupMeans[i], lwd = 3, col = myCols[i+1])
    }

    # points(x = data$pp, y = predPoints, pch = 23, bg = darkCols[as.numeric(data$group)+1], col = "black", cex = 1.35)
    if ("Segments" %in% input$whatDisplay) {
      segments(x0 = data$pp, x1 = data$pp, y0 = predPoints, y1 = data$dv, lwd = 2)
      totSumSquares <- round(stats[['Total.Sum.of.Squares']] - modSumSquares, 3)
      if ("Sums" %in% input$whatDisplay) {
        if (input$whatPred == "Mean" | !("F-stat" %in% input$whatDisplay)) {
          mtext(paste0("Error Sum of Squares = ", totSumSquares), cex = 1.4)
        } else {
          mtext(paste0("Error Sum of Squares = ", totSumSquares, "\n Mean Square = ", round(totSumSquares/dfError, 3)), cex = 1.4)
        }
      } else {
        mtext("Model error", cex = 1.8)
      }
    }
  }
  
}


# Run the application 
shinyApp(ui = ui, server = server)
