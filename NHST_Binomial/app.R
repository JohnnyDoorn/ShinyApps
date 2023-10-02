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
                  "Number of coins:",
                  min = 1,
                  max = 30,
                  step = 1,
                  value = 10),
      sliderInput("theta",
                  "P(heads)",
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
      radioButtons("flip",
                   "Flip the dist!",
                   choices = c("No", "Yes"))
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
    nLeftBars <- qbinom(halfAlpha, input$nFlips, nulTheta, lower.tail = TRUE)
    
    if (input$decision == "Nothing") {
      allCols <- rep("darkgreen", input$nFlips+1) 
    } else if (input$decision == "Reject H0") {
      allCols <- c(rep("red", nLeftBars),
                   rep("darkgreen", input$nFlips + 1 - 2*nLeftBars),
                   rep("red", nLeftBars))
    } else {
      allCols <- c(rep("darkgreen", nLeftBars),
                   rep("red", input$nFlips + 1 - 2*nLeftBars),
                   rep("darkgreen", nLeftBars))
    }
    
    par(cex = 1.4, cex.lab = 1.6)
    densValues <- dbinom(0:input$nFlips, input$nFlips, prob = input$theta)
    if (input$flip == "Yes") {
      densValues <- 1- densValues
      densValues <- densValues / sum(densValues)
    }
    
    plot(0:input$nFlips, densValues, 
         col  = allCols, 
         type = "h",
         lwd = 20,
         las = 1,
         ylab = "Probability",
         xlab = "Number of heads",
         ylim = c(0, 0.5),
         xlim = c(-1, input$nFlips+1),
         # yaxt = 'n', 
         main = 'Sampling Distribution'
    )
    
    leftAbLineLoc <- qbinom(halfAlpha, input$nFlips, nulTheta, lower.tail = TRUE) - 0.5
    rightAbLineLoc <- qbinom(halfAlpha, input$nFlips, nulTheta, lower.tail = FALSE) + 0.5
    leftArea <- round(pbinom(nLeftBars-1, input$nFlips, input$theta), 3)
    rightArea <- round(pbinom(input$nFlips-nLeftBars, input$nFlips, input$theta, lower.tail = FALSE), 3)
    
    if (input$decision != "Nothing") {
      abline(v = leftAbLineLoc, lwd = 2, lty = 2)
      abline(v = rightAbLineLoc, lwd = 2, lty = 2)
      
      text(-0.5, 0.4, leftArea, col = allCols[1], cex = 2)
      text(input$nFlips+0.5, 0.4, rightArea, col = allCols[1], cex = 2)
      
      text(mean(c(leftAbLineLoc, rightAbLineLoc)), 0.4, 1 - rightArea - leftArea, col = allCols[nLeftBars+2], cex = 2)
    }
    # pbinom(input$nFlips + 1 - nLeftBars, input$nFlips, input$theta)
    
    
  },
  width = 800, height = 800)
  
  output$binomTable <- renderTable(
    
    data.frame(Heads = 0:input$nFlips,
               Prob = dbinom(0:input$nFlips, input$nFlips, prob = input$theta),
               CumulativeProb = pbinom(0:input$nFlips, input$nFlips, prob = input$theta)), digits = 3
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
