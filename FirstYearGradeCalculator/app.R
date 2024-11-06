#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("First Year Grade Calculator"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      radioButtons("nMcOptions",
                   "Number of MC alternatives:",
                   choices = c("2", "3", "4"), 
                   selected = "3"),
      numericInput("nMcQuestions",
                   "How many MC points are available?",
                   min = 1,
                   step = 1,
                   max = 100,
                   value = 25),
      sliderInput("nMcCorrect",
                  "Your MC points scored:",
                  min = 0,
                  step = 1,
                  value = 10,
                  max = 25),
      numericInput("nOeQuestions",
                   "How many OE points are available?",
                   min = 1,
                   step = 1,
                   max = 25,
                   value = 5),
      sliderInput("nOeCorrect",
                   "Your OE Points Scored:",
                   min = 0,
                   step = 0.5,
                   value = 0,
                   max = 5),
      numericInput("deductionPoints",
                   "How many deduction points (e.g., from missed WAs)?",
                   min = 1,
                   step = 0.1,
                   max = 10,
                   value = 0)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Use observe to dynamically update the slider max value
  observe({
    updateSliderInput(session, "nMcCorrect", max = input$nMcQuestions)
    updateSliderInput(session, "nOeCorrect", max = input$nOeQuestions)
  })
  
  output$distPlot <- renderPlot({
    textCex <- 2.3
    # generate bins based on input$bins from ui.R
    cc <- input$nMcQuestions / as.numeric(input$nMcOptions)
    mc_mark <- ((input$nMcCorrect - cc) / (input$nMcQuestions - cc)) * 10
    oe_mark <- (input$nOeCorrect / input$nOeQuestions) * 10
    yourGrade <- 0.8 * mc_mark + 0.2 * oe_mark
    yourGrade <- max(c(min(c(yourGrade, 10)), 0))
    plot(1, 1, bty = "n", axes = FALSE, type = "n", xlab = "", ylab = "", ylim=c(0,2))
    text(1, 1.5, paste0("MC mark = ", round(mc_mark, 2)), cex = textCex)
    text(1, 1, paste0("OE mark = ", round(oe_mark, 2)), cex = textCex)
    text(1, 0.5, paste0("Unrounded mark = ", round(yourGrade - input$deductionPoints, 2)), cex = textCex)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
