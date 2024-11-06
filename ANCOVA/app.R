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
  titlePanel("The Covarying Squares"),
  
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
      sliderInput("covValue",
                  "Association strength dv/cov (observed cor will differ due to sampling variability) :",
                  min = 0,
                  max = 1,
                  step = .1,
                  value = 1),
      radioButtons("whatPred",
                   "What do we predict with?",
                   c("Mean", "Group means", "Cov", "Group means + cov"), selected = "Mean"),
      checkboxGroupInput("whatDisplay",
                         "Display:",
                         c("Segments", "Sums", "F-stat")),
      downloadButton("downloadData", "Download")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotOutput("distPlot")),
        tabPanel("Table", tableOutput("squaresTable")),
        tabPanel("Scatterplot Cov", plotOutput("scatterPlotCov")),
        tabPanel("Coefficients", tableOutput("coefTable"))
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
    
    totN <- input$nGroups * input$groupN
    
    groupOneMean <- 5
    # Generate data for a factorial design with three groups
    groups <- factor(rep(1:input$nGroups, each = input$groupN))
    age <- round(rnorm(totN, 30, 5), 1)
    
    dv <- rnorm(n = totN, 
                mean = input$covValue * age, 
                sd = 3)
    groupEffect <- rep((0:(input$nGroups-1)) * (input$diffGroups / (input$nGroups-1)), each = input$groupN)
    dv <- 2 * dv / sd(dv)
    
    dv <- round(dv - rep(tapply(dv, groups, mean), each = input$groupN)  + groupEffect , 1)
    
    data <- data.frame(
      pp = (1:totN),
      group = groups,
      dv = groupOneMean + dv,
      age = age
    )
    write.csv(data, "data.csv")
    
    groupMeans <- tapply(data$dv, data$group, mean)
    grandMean <- mean(data$dv)
    myFullMod <- lm(dv ~ group + age, data = data)
    myCovMod <- lm(dv ~ age, data = data)
    
    predictedOnGroup <- data[["predictedOnGroup"]] <-  groupMeans[as.numeric(data$group)]
    predictedOnCov <- data[["predictedOnCov"]] <- myCovMod$fitted.values
    predictedFull <- data[["predictedFull"]] <- myFullMod$fitted.values
    
    fullModelError <- myFullMod$residuals
    
    sumSquareFull <- sum((predictedFull - grandMean)^2)
    sumSquareCov <- sum((predictedOnCov - grandMean)^2)
    sumSquareGroup <- sum((predictedOnGroup - grandMean)^2)
    
    errorMS <- (sum(fullModelError^2) /  (totN - input$nGroups - 1))  # error variance
    groupModelMS <- (sumSquareGroup / (input$nGroups - 1)) 
    covModelMS <- (sumSquareCov / 1)
    nulModelError <- data$dv - grandMean
    
    finalSumSquareCov <- sumSquareFull - sumSquareGroup
    finalSumSquareGroup <- sumSquareFull - finalSumSquareCov
    
    dfGroup <- input$nGroups -1
    dfCov <- 1
    dfError <-  (input$groupN*input$nGroups) - (input$nGroups + 1)
    
    myMetrics <- data.frame('Total Sum of Squares' = sum(nulModelError^2),
                            'Group Sum of Squares' = finalSumSquareGroup,
                            'Cov Sum of Squares' = finalSumSquareCov,
                            'Full Model Sum of Squares' = sumSquareFull,
                            'Error Sum of Squares' = sum(fullModelError^2),
                            'Group df' = dfGroup,
                            'Cov df' = 1,
                            'Error df' = dfError,
                            'Group Mean Squares' = finalSumSquareGroup / dfGroup,
                            'Cov Mean Squares' = finalSumSquareCov,
                            'Error Mean Squares' = sum(fullModelError^2) / dfError,
                            'F Group' = (finalSumSquareGroup / dfGroup) / (sum(fullModelError^2) / dfError),
                            'F Cov' = (finalSumSquareCov / dfCov) / (sum(fullModelError^2) / dfError)
    )
    
    myMetrics <- data.frame('Statistic' = colnames(myMetrics), 'Value' = unlist(unname(myMetrics)))
    
    return(list(data = data, myMetrics = myMetrics))
  })
  
  output$distPlot <- renderPlot({
    
    par(mfrow = c(1, 3), cex = 1.35)
    plotSumSquares(myReactive()[["data"]], input = input, sumSq = "Total",  stats = myReactive()[["myMetrics"]])
    plotSumSquares(myReactive()[["data"]], input = input, sumSq = "Error",  stats = myReactive()[["myMetrics"]])
    plotSumSquares(myReactive()[["data"]], input = input, sumSq =  "Model",  stats = myReactive()[["myMetrics"]])
    
    
  }, width = 1500, height = 800)
  
  output$squaresTable <- renderTable(myReactive()[["myMetrics"]], digits = 2)
  
  output$scatterPlotCov <- renderPlot({
    
    data <- myReactive()[["data"]]
    myCols <- palette.colors(n = input$nGroups+1, palette = "Okabe-Ito")
    par(cex = 1.4, cex.lab = 1.6)
    
    if (input$whatPred == "Cov") {
      myMod <- lm(dv ~ age, data = data)
    } else {
      myMod <- lm(dv ~ group + age, data = data)
    } 
    
    regCoef <- round(myMod$coefficients['age'], 3)
    plot(data$age, data$dv, bty = "n", las = 1, pch = 21, bg = myCols[data$group], col = "black", 
         cex = 1.5, xlab = "Covariate", ylab = "DV", ylim = c(0, 10), 
         main = paste0('Regression coefficient of cov = ', regCoef))
    abline(lm(dv ~ age, data = data), lwd = 2, col = "darkgreen")
  },
  width = 800, height = 800)
  
  output$coefTable <- renderTable({
    data <- myReactive()[["data"]]
    
    if (input$whatPred == "Group means") {
      myMod <- lm(dv ~ group, data = data)
    } else if (input$whatPred == "Cov") {
      myMod <- lm(dv ~ age, data = data)
    } else if (input$whatPred == "Group means + cov") {
      myMod <- lm(dv ~ group + age, data = data)
    } else if (input$whatPred == "Mean") {
      myMod <- lm(dv ~ 1, data = data)
    }
    
    data.frame(coef = names(myMod$coefficients),
               value = round(myMod$coefficients, 3))
    
  }, digits = 2)
  
  output$downloadData <- downloadHandler(
    filename = function() {
      # Use the selected dataset as the suggested file name
      paste0(input$dataset, ".csv")
    },
    content = function(file) {
      # Write the dataset to the `file` that will be downloaded
      write.csv(myReactive()[["data"]], file)
    }
  )
  
}

darken_color <- function(color, factor = 1.6) {
  col_rgb <- col2rgb(color)
  darkened_rgb <- col_rgb / factor
  rgb(darkened_rgb[1,], darkened_rgb[2,], darkened_rgb[3,], maxColorValue = 255)
}

plotSumSquares <- function(data, input, sumSq = "Total", stats = NULL, plotMean = TRUE) {
  
  myCols <- palette.colors(n = input$nGroups+1, palette = "Okabe-Ito")
  darkCols <- sapply(myCols, darken_color)
  
  plot(data$dv, col = "black" , pch = 21, bg = myCols[as.numeric(data$group)+1], 
       cex = 1.8, lwd = 3, las = 1, bty = "n",
       ylab = "Score", xlab = "Participant Nr.", xlim = c(0, length(data$dv)), 
       ylim = c(2, 10), cex.lab = 1.3, cex.axis=1.3)
  
  totN <- length(data$dv)
  # Calculate the grand mean
  grandMean <- mean(data$dv)
  
  # Calculate the group mean
  groupMeans <- tapply(data$dv, data$group, mean)
  nulModelError <- data$dv - grandMean
  
  if (sumSq == "Total") {
    if (plotMean) {
      abline(h = mean(data$dv), lwd = 3, col = "purple")
    }
    if ("Segments" %in% input$whatDisplay) {
      segments(x0 = data$pp, x1 = data$pp, y0 = mean(data$dv), y1 = data$dv, lwd = 2)
      if ("Sums" %in% input$whatDisplay) {
        mtext(paste0("Total Sum of Squares = ", round(sum(nulModelError^2), 2)), cex = 1.4)
      } else {
        mtext("", cex = 1.8)
      }
    }
  } 
  
  if (input$whatPred == "Group means") {
    predPoints <- data$predictedOnGroup
    modSumSquares <- stats[stats[, 1] == 'Group.Sum.of.Squares', 2]
    dfMod <- input$nGroups - 1
    dfError <- totN - (input$nGroups)
    
  } else if (input$whatPred == "Cov") {
    predPoints <- data$predictedOnCov
    modSumSquares <- stats[stats[, 1] == 'Cov.Sum.of.Squares', 2]
    dfMod <- 1
    dfError <- totN - (1)
  } else if (input$whatPred == "Group means + cov") {
    predPoints <- data$predictedFull
    modSumSquares <- stats[stats[, 1] == 'Full.Model.Sum.of.Squares', 2]
    dfMod <- input$nGroups
    dfError <- totN - (1 + input$nGroups)
  } else if (input$whatPred == "Mean") {
    predPoints <- rep(grandMean, totN)
    modSumSquares <- 0
    dfMod <- totN - (1 + input$nGroups)
    dfError <- 1
  }
  
  fullModMeanSquareError <- round(stats[stats[, 1] == 'Error.Sum.of.Squares', 2] / (totN - (1 + input$nGroups)), 3)
  
  
  if (sumSq == "Model") {
    abline(h = mean(data$dv), lwd = 3, col = "purple")
    points(x = data$pp, y = predPoints, pch = 23, bg = darkCols[as.numeric(data$group)+1], col = "black", cex = 1.35)
    if ("Segments" %in% input$whatDisplay) {
      segments(x0 = data$pp, x1 = data$pp, y0 = predPoints, y1 = mean(data$dv), lwd = 2, col = myCols[as.numeric(data$group)+1])
      if ("Sums" %in% input$whatDisplay) {
        if (input$whatPred == "Mean" | !("F-stat" %in% input$whatDisplay)) {
          mtext(paste0("Model Sum of Squares = ", round(modSumSquares, 3), "\nMean Square = ", round(modSumSquares/dfMod, 3)), cex = 1.4, line = 0)
        } else {
          mtext(paste0("Model Sum of Squares = ", round(modSumSquares, 3), "\nMean Square = ", round(modSumSquares/dfMod, 3), 
                       "\nF = ",round(modSumSquares/dfMod, 3),"/",fullModMeanSquareError, " = ", round(round(modSumSquares/dfMod, 3)/fullModMeanSquareError, 3)) , cex = 1.4, line = 0)
        }
      } else {
        mtext("Model improvement", cex = 1.8)
      }
    }
  }
  
  
  if (sumSq == "Error") {
    # abline(h = mean(data$dv), lwd = 3, col = "purple")
    points(x = data$pp, y = predPoints, pch = 23, bg = darkCols[as.numeric(data$group)+1], col = "black", cex = 1.35)
    
    if ("Segments" %in% input$whatDisplay) {
      
      segments(x0 = data$pp, x1 = data$pp, y0 = predPoints, y1 = data$dv, lwd = 2, col = darkCols[as.numeric(data$group)+1])
      totSumSquares <- round(stats[stats[, 1] == 'Total.Sum.of.Squares', 2] - modSumSquares, 3)
      if ("Sums" %in% input$whatDisplay) {
        mtext(paste0("Error Sum of Squares = ", totSumSquares, "\n Mean Square = ", round(totSumSquares/dfError, 3)), cex = 1.4)
      } else {
        mtext("Model error", cex = 1.8)
      }
    }
  } 
}


# Run the application 
shinyApp(ui = ui, server = server)
