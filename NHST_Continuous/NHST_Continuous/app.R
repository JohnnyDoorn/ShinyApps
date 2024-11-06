#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotrix)
# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Continuous Explorations"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("nFlips",
                  "Sample size (n):",
                  min = 2,
                  max = 60,
                  step = 1,
                  value = 10),
      # sliderInput("delta",
      #             "Mean Difference (center of sampling distribution)",
      #             min = -2,
      #             max = 2,
      #             step = 0.05,
      #             value = 0),
      # sliderInput("sd",
      #             "Standard deviation",
      #             min = 0.1,
      #             max = 2,
      #             step = 0.05,
      #             value = 1),
      radioButtons("decision",
                   "Decision: (overlay decision regions)",
                   choices = c("Nothing", "Reject H0", "Do not reject H0")
      ),
      radioButtons("alpha",
                   withMathJax("$$\\alpha$$"),
                   choices = c(0.01, 0.05, 0.2),
                   selected = 0.05),
      radioButtons("altHyp",
                   "Two-sided?",
                   choices = c("Yes", "Negative only", "Positive only")
      ),
      checkboxInput("displayT",
                    "Display p-value for observed t-stat",value = FALSE
      ),
      numericInput("tVal",
                   "Observed t-stat:",
                   value = 1.5,
                   step = 0.2,
                   min = -4,
                   max = 4)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotOutput("distPlot"))
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$distPlot <- renderPlot({

    thisSD <- 1
    myNCP <- 0 #input$delta /(thisSD)
    myDF <- input$nFlips - 1
    xVals <- seq(-5, 5, length.out = 1e3)

    halfAlpha <- as.numeric(input$alpha)/2
    tVal <- input$tVal

    leftAbLineLoc <- qt(halfAlpha, df = myDF, ncp = 0, lower.tail = TRUE)
    rightAbLineLoc <- qt(1-halfAlpha, df = myDF, ncp = 0, lower.tail = TRUE)
    leftArea <- round(pt(leftAbLineLoc, myDF, ncp = myNCP), 3)
    rightArea <- round(pt(rightAbLineLoc, myDF, ncp = myNCP, lower.tail = FALSE), 3)

    if (input$altHyp == "Yes") {
      halfAlpha <- as.numeric(input$alpha)/2
      lowerTail <- tVal < 0

      leftAbLineLoc <- qt(halfAlpha, df = myDF, ncp = 0, lower.tail = TRUE)
      rightAbLineLoc <- qt(1-halfAlpha, df = myDF, ncp = 0, lower.tail = TRUE)
      leftArea <- round(pt(leftAbLineLoc, myDF, ncp = myNCP), 3)
      rightArea <- round(pt(rightAbLineLoc, myDF, ncp = myNCP, lower.tail = FALSE), 3)
    } else if (input$altHyp == "Negative only"){
      halfAlpha <- as.numeric(input$alpha)/2

      leftAbLineLoc <- qt(as.numeric(input$alpha), df = myDF, ncp = 0, lower.tail = TRUE)
      rightAbLineLoc <- qt(1, df = myDF, ncp = 0, lower.tail = TRUE)
      leftArea <- round(pt(leftAbLineLoc, myDF, ncp = myNCP), 3)
      rightArea <- round(pt(rightAbLineLoc, myDF, ncp = myNCP, lower.tail = FALSE), 3)
      lowerTail <- TRUE

    } else if (input$altHyp == "Positive only"){
      halfAlpha <- as.numeric(input$alpha)/2

      leftAbLineLoc <- qt(1, df = myDF, ncp = 0, lower.tail = FALSE)
      rightAbLineLoc <- qt(as.numeric(input$alpha), df = myDF, ncp = 0, lower.tail = FALSE)
      leftArea <- round(pt(leftAbLineLoc, myDF, ncp = myNCP), 3)
      rightArea <- round(pt(rightAbLineLoc, myDF, ncp = myNCP, lower.tail = FALSE), 3)
      lowerTail <- TRUE
    }

    twoCols <- c("darkgreen", "purple")
    if (input$decision == "Nothing") {
      allCols <- rep("darkgreen", length(xVals))
    } else if (input$decision == "Reject H0") {
      allCols <- ifelse(xVals > leftAbLineLoc & xVals < rightAbLineLoc, "darkgreen", "purple")
    } else {
      allCols <- ifelse(xVals > leftAbLineLoc & xVals < rightAbLineLoc, "purple", "darkgreen")
    }

    par(cex = 1.4, cex.lab = 1.6)
    densValues <- dt(xVals, input$nFlips - 1, ncp = myNCP)

    plot(xVals, densValues,
         col  = allCols,
         type = "h",
         lwd = 2,
         las = 1,
         ylab = "Density",
         xlab = "T-Statistic",
         ylim = c(0, 0.5),
         xlim = c(min(xVals), max(xVals)),
         # yaxt = 'n',
         main = paste0('Sampling Distribution\n(df = ', myDF,")")
    )


    if (input$decision != "Nothing") {
      abline(v = leftAbLineLoc, lwd = 2, lty = 2)
      abline(v = rightAbLineLoc, lwd = 2, lty = 2)

      text(-4, 0.45, leftArea, col = allCols[1], cex = 2)
      text(4, 0.45, rightArea, col = allCols[length(allCols)], cex = 2)

      text(0, 0.45, 1 - rightArea - leftArea, col = allCols[round(length(allCols)/2, 0)], cex = 2)
    }

    if (input$displayT) {
      twoSided <- input$altHyp == "Yes"
      arrows(x0 = tVal, x1 = tVal, y0 = 0, y1 = 0.4, lwd = 4, col = "darkred")
      if(twoSided) {
        lowerTail <- tVal < 0
        exVals <- xVals[abs(xVals) > abs(tVal)]
      } else if (input$altHyp == "Negative only"){
        lowerTail <- TRUE
        exVals <- xVals[xVals < tVal]
      } else {
        lowerTail <- FALSE
        exVals <- xVals[xVals > tVal]
      }
      thisP <- round(pt(tVal, df = myDF, lower.tail = lowerTail), 3) * (2 - 1 * !twoSided)
      text(tVal, 0.42, paste0("p = ",thisP), cex = 2)
      lines(exVals, dt(exVals, input$nFlips - 1, ncp = myNCP), type = "h")
    }
  },
  width = 800, height = 800)

}

# Run the application
shinyApp(ui = ui, server = server)
