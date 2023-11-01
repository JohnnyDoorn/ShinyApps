#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotrix) # package plotrix is needed for function "ablineclip""


# Define UI for application that draws a histogram
ui <- fluidPage(
  withMathJax(),
  # Application title
  titlePanel("Visualizing the Equation for the Correlation Coefficient"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("nSamples",
                  withMathJax("$$n$$"),
                  min = 5,
                  max = 50,
                  value = 5),
      sliderInput("corXY",
                  withMathJax("$$\\text{cor}_{x, y}$$"),
                  min = -1,
                  max = 1,
                  step = 0.1,
                  value = 0),
      sliderInput("sdX",
                  withMathJax("$$\\text{sd}_{x}$$"),
                  min = 1,
                  max = 3,
                  step = 0.2,
                  value = 1),
      sliderInput("sdY",
                  withMathJax("$$\\text{sd}_{y}$$"),
                  min = 1,
                  max = 3,
                  step = 0.2,
                  value = 1),
      fluidRow(
        column(width = 3,
               checkboxGroupInput("metrics",
                                  "Choose metrics:",
                                  choiceNames =
                                    list("Cor", "Cov", "Slope"),
                                  choiceValues =
                                    list("cor", "cov", "slope")
               )),
        column(width = 3,
               checkboxGroupInput("showLeastSquares",
                                  "Least Squares:",
                                  choiceNames =
                                    list("Show"),
                                  choiceValues =
                                    list("show")
               )),
        column(width = 6, 
               checkboxGroupInput("showUserLine",
                                  "User Line:",
                                  choiceNames =
                                    list("Show"),
                                  choiceValues =
                                    list("show")
               ),
               fluidRow(      
                 sliderInput("usrLineA",
                             withMathJax("$$\\beta_0$$"),
                             min = -2,
                             max = 2,
                             step = 0.1,
                             value = 0),
                 sliderInput("usrLineB",
                             withMathJax("$$\\beta_1$$"),
                             min = -2,
                             max = 2,
                             step = 0.1,
                             value = 0))
        )),
      numericInput("rSeed",
                   "Set seed:",
                   123,
                   min = 1, 
                   max = 1e5)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  output$distPlot <- renderPlot({
    
    axisLimValX <- axisLimValY <- c(-6, 6)
    set.seed(input$rSeed)
    x <- rnorm(input$nSamples, mean = 0, sd = 1)
    x <- x / sd(x) * (input$sdX)^2
    x <- x - mean(x)
    axisLimValX <- axisLimValY <- c(-6, 6)
    axisLimVal <- 6
    # https://stackoverflow.com/questions/47775389/how-to-simulate-a-vector-that-is-correlated-in-a-different-way-to-two-other-ex
    y <- rnorm(input$nSamples)
    y <- (input$corXY * scale(x)[,1] + sqrt(1 - input$corXY^2) * 
            scale(resid(lm(y ~ x)))[,1]) * input$sdY
    
    usrCol <- rainbow(10, alpha = 0.3)[9]
    lmCol <- rainbow(10, alpha = 0.3)[2]
    
    # if the following line and the line containing "dev.off()" are executed, the plot will be saved as a png file in the current working directory
    # png("Presidental.png", width = 18, height = 18, units = "cm", res = 800, pointsize = 10) 
    par(cex.main = 1.5, mar = c(5, 5, 5, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5 , font.lab = 2, cex.axis = 1.3, bty = "n", las = 1)
    plot(x, y, col = "black", pch = 21, bg = rainbow(10, alpha = 0.3)[7], cex = 2, xlab = "x", ylab = "y", lwd = 2,
         xlim = axisLimValX, ylim = axisLimValY,
         axes = FALSE, asp = 1)
    axis(1)
    axis(2)
    myMod <- lm(y ~ x)
    par(las = 0)
    # mtext("Presidential Height Ratio", side = 1, line = 2.5, cex = 1.5)
    # mtext("Relative Support for President", side = 2, line = 3.7, cex = 1.5)
    
    myResids <- myMod$residuals
    myPreds <- predict(myMod)
    
    userPreds <- input$usrLineA + input$usrLineB * x
    userResids <- y - userPreds
    
    if ("cor" %in% input$metrics) {
      text(-3.8, 5.9 * (sign(input$corXY+0.01)), bquote(r[xy] ~ "=" ~ .(round(cor(x, y), 2))), cex = 2)  
    }
    if ("cov" %in% input$metrics) {
      text(-3.8, 5.2 * (sign(input$corXY+0.01)), bquote(cov[xy] ~ "=" ~ .(round(cor(x, y), 2))), cex = 2)  
    }
    if ("slope" %in% input$metrics) {
      text(-3.8, 4.5 * (sign(input$corXY+0.01)), bquote(beta[1] ~ "=" ~ .(round(myMod$coefficients[["x"]], 2))), cex = 2)  
    }
    
    
    cliModifier <- 0.25 
    if ("show" %in% input$showLeastSquares) {
      lapply(1:length(x), function(i) {
        polygon(rep(c(x[i]+myResids[i], x[i]), each =2), rep(c(y[i], myPreds[i], myPreds[i], y[i])), col = lmCol)
      })
      text(-3.8, 3.8 * (sign(input$corXY+0.01)), bquote(SumSq[leastSquares] ~ "=" ~ .(round(sum(myResids^2), 2))), cex = 2)  
      ablineclip(myMod, lwd = 3, x1 = -axisLimVal-cliModifier, x2 = axisLimVal+cliModifier, 
                 y1 = -axisLimVal-cliModifier, y2 = axisLimVal+cliModifier,
                 col = "darkorange") 
    }
    
    if ("show" %in% input$showUserLine) {
      lapply(1:length(x), function(i) {
        polygon(rep(c(x[i]+userResids[i], x[i]), each =2), rep(c(y[i], userPreds[i], userPreds[i], y[i])), col = usrCol )
      })
      text(-3.8, 3.1 * (sign(input$corXY+0.01)), bquote(SumSq[user] ~ "=" ~ .(round(sum(userResids^2), 2))), cex = 2)  
      ablineclip(input$usrLineA, input$usrLineB, lwd = 3, x1 = -axisLimVal-cliModifier, x2 = axisLimVal+cliModifier, 
                 y1 = -axisLimVal-cliModifier, y2 = axisLimVal+cliModifier, 
                 col = "darkblue") 
    }
    
    
    
  },
  width = 800, height = 800)
}

# Run the application 
shinyApp(ui = ui, server = server)
