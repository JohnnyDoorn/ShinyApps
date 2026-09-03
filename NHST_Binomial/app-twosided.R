#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Binomial Explorations"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("nFlips",
                  "Number of trials:",
                  min = 1,
                  max = 20,
                  step = 1,
                  value = 10),
      sliderInput("theta",
                  "Population proportion",
                  min = 0,
                  max = 1,
                  step = 0.05,
                  value = 0.5),
      radioButtons("decision",
                   "Decision:",
                   choices = c("Nothing", "Reject H0", "Do not reject H0")
      ),
      radioButtons("alpha",
                   withMathJax("$$\\alpha$$"),
                   choices = c(0.01, 0.05, 0.2), 
                   selected = 0.05),
      radioButtons("altHyp",
                   "Two-sided?",
                   choices = c("Yes", "Negative only", "Positive only")
      )
      # radioButtons("flip",
      #              "Flip the dist!",
      #              choices = c("No", "Yes"))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotOutput("distPlot")),
        tabPanel("Table", tableOutput("binomTable"))
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    nulTheta <- 0.5
    halfAlpha <- as.numeric(input$alpha)/2
    
    altHypothesis <- input$altHyp
    
    # Determine the critical regions based on the chosen alternative hypothesis
    if (altHypothesis == "Yes") {
      nLeftBars <- qbinom(halfAlpha, input$nFlips, nulTheta, lower.tail = TRUE)
      nRightBars <- qbinom(halfAlpha, input$nFlips, nulTheta, lower.tail = FALSE)
      leftArea <- round(pbinom(nLeftBars - 1, input$nFlips, input$theta), 3)
      rightArea <- round(pbinom(input$nFlips - nLeftBars, input$nFlips, input$theta, lower.tail = FALSE), 3)
      
    } else if (altHypothesis == "Positive only") {
      nLeftBars <- 0
      nRightBars <- qbinom(halfAlpha*2, input$nFlips, nulTheta, lower.tail = FALSE)
      leftArea <- 0
      rightArea <- round(pbinom(nRightBars, input$nFlips, input$theta, lower.tail = FALSE), 3)
      
    } else if (altHypothesis == "Negative only") {
      nLeftBars <- qbinom(halfAlpha*2, input$nFlips, nulTheta, lower.tail = TRUE)
      nRightBars <- 0
      leftArea <- round(pbinom(nLeftBars - 1, input$nFlips, input$theta), 3)
      rightArea <- 0
      
    }
    
    leftAbLineLoc <- qbinom(halfAlpha + (halfAlpha * (altHypothesis != "Yes")), input$nFlips, nulTheta, lower.tail = TRUE) - 0.5
    rightAbLineLoc <- qbinom(halfAlpha + (halfAlpha * (altHypothesis != "Yes")), input$nFlips, nulTheta, lower.tail = FALSE) + 0.5
    
    xVals <- 0:input$nFlips
    if (input$decision == "Nothing") {
      allCols <- rep("darkgreen", input$nFlips+1) 
    } else if (input$decision == "Reject H0") {
      allCols <- ifelse(xVals >= nLeftBars & xVals <= nRightBars, "darkgreen", "purple")
      if (altHypothesis == "Negative only")
        allCols <- ifelse(xVals >= nLeftBars, "darkgreen", "purple")
    } else {
      allCols <- ifelse(xVals >= leftAbLineLoc & xVals <= rightAbLineLoc, "purple", "darkgreen")
    }
    
    par(cex = 1.5, cex.lab = 1.6)
    densValues <- dbinom(0:input$nFlips, input$nFlips, prob = input$theta)
    yMax <- max(densValues) * 1.2
    
    plot(0:input$nFlips, densValues, 
         col  = allCols, 
         type = "h",
         lwd = 20,
         las = 1,
         ylab = "Probability",
         xlab = "Number of 'Successes'",
         ylim = c(0, yMax),
         xlim = c(-1, input$nFlips + 1),
         # yaxt = 'n', 
         main = 'Sampling Distribution'
    )
    
    
    if (input$decision != "Nothing") {
      
      if (altHypothesis != "Positive only")
        abline(v = leftAbLineLoc, lwd = 2, lty = 2)
      if (altHypothesis != "Negative only")
        abline(v = rightAbLineLoc, lwd = 2, lty = 2)
      
      text(-0.5, yMax * 0.95, leftArea, col = allCols[1], cex = 2)
      text(input$nFlips + 0.5, yMax * 0.95, rightArea, col = allCols[length(allCols)], cex = 2)
      text(mean(c(leftAbLineLoc, rightAbLineLoc)), yMax * 0.95, 1 - rightArea - leftArea, col = allCols[nLeftBars + 2], cex = 2)
    }
    
    # Add the cumulative probabilities above the bars
    text(0:input$nFlips, densValues + yMax * 0.04, round(densValues, 3), col = allCols, cex = 1.2)
    
  },
  width = 1000, height = 800)
  
  output$binomTable <- renderTable(
    data.frame(Heads = 0:input$nFlips,
               Prob = dbinom(0:input$nFlips, input$nFlips, prob = input$theta),
               CumulativeProb = pbinom(0:input$nFlips, input$nFlips, prob = input$theta)), digits = 3
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
