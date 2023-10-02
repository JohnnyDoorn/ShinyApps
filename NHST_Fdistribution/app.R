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
  titlePanel("Continuous Explorations: F-Distribution"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("nGroup",
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
                  value = 10),
      sliderInput("ncp",
                  "ncp:",
                  min = 0,
                  max = 5,
                  step = .1,
                  value = 0),
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
      checkboxInput("displayF",
                    "Display p-value for observed t-stat",value = FALSE
      ),
      numericInput("fVal",
                   "Observed F-stat:",
                   value = 3,
                   step = 0.2,
                   min = 0.5,
                   max = 10)
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
    
    xVals <- seq(0.1, 10, length.out = 1e3)
    ncp <- input$ncp
    dfTreat <- input$nGroup -1
    dfError <-  (input$groupN*input$nGroup) - input$nGroup
    
    fVal <- input$fVal
    
    myAlpha <- as.numeric(input$alpha)
    
    rightAbLineLoc <- qf(1-myAlpha,df1 = dfTreat, df2 = dfError, lower.tail = TRUE, ncp = 0) 
    rightArea <- round(pf(rightAbLineLoc, df1 = dfTreat, df2 = dfError, lower.tail = FALSE, ncp = ncp), 3)

  
    
    twoCols <- c("darkgreen", "purple")
    if (input$decision == "Nothing") {
      allCols <- rep("darkgreen", length(xVals))
    } else if (input$decision == "Reject H0") {
      allCols <- ifelse(xVals < rightAbLineLoc, "darkgreen", "purple")
    } else {
      allCols <- ifelse(xVals < rightAbLineLoc, "purple", "darkgreen")
    }
    
    par(cex = 1.4, cex.lab = 1.6)
    densValues <- df(xVals, df1 = dfTreat, df2 = dfError, ncp = ncp)
    
    plot(xVals, densValues, 
         col  = allCols, 
         type = "h",
         lwd = 2,
         las = 1,
         ylab = "Probability",
         xlab = "F-Statistic",
         ylim = c(0, 1),
         xlim = c(0, max(xVals)),
         # yaxt = 'n', 
         main = paste0('Sampling Distribution \ndf1 = ', dfTreat, "\ndf2 = ", dfError ),
    )
    
    
    if (input$decision != "Nothing") {
      abline(v = rightAbLineLoc, lwd = 2, lty = 2)
      
      text(max(xVals)/(6/5), 0.85, rightArea, col = allCols[length(allCols)], cex = 2)
      
      text(max(xVals)/6, 0.85, 1 - rightArea, col = allCols[1], cex = 2)
    }
    
    if (input$displayF) {
      twoSided <- input$altHyp == "Yes"
      arrows(x0 = fVal, x1 = fVal, y0 = 0, y1 = 0.4, lwd = 4, col = "darkblue")
      if(twoSided) {
        lowerTail <- fVal < 0
        exVals <- xVals[abs(xVals) > abs(fVal)]
      } else if (input$altHyp == "Negative only"){
        lowerTail <- TRUE
        exVals <- xVals[xVals < fVal]
      } else {
        lowerTail <- FALSE
        exVals <- xVals[xVals > fVal]
      }
      thisP <- round(pf(fVal, df1 = dfTreat, df2 = dfError, lower.tail = FALSE, ncp = 0), 3)
      text(fVal, 0.44, paste0("p = ",thisP), cex = 2)
      lines(exVals, df(exVals,  df1 = dfTreat, df2 = dfError, ncp = 0), type = "h")
    }
  },
  width = 800, height = 800)
  
}

# Run the application 
shinyApp(ui = ui, server = server)
