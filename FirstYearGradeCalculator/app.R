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
                        "Number of MC options:",
                        choices = c("2", "3", "4"), 
                        selected = "3"),
            numericInput("nMcQuestions",
                         "Number of MC questions:",
                         min = 1,
                         step = 1,
                         max = 100,
                         value = 25),
            numericInput("nMcCorrect",
                         "Multiple Choice Points:",
                         min = 0,
                         step = 1,
                         value = 1,
                         max = 100),
            numericInput("nOeCorrect",
                         "Open Question Points:",
                         min = 0,
                         step = 1,
                         value = 1,
                         max = 5)
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
      # generate bins based on input$bins from ui.R
      cc <- input$nMcQuestions / as.numeric(input$nMcOptions)
      mc_mark <- ((input$nMcCorrect - cc) / (input$nMcQuestions - cc)) * 10
      oe_mark <- (input$nOeCorrect / 5) * 10
      yourGrade <- 0.8 * mc_mark + 0.2 * oe_mark
      yourGrade <- max(c(min(c(yourGrade, 10)), 0))
      plot(1, 1, bty = "n", axes = FALSE, type = "n", xlab = "", ylab = "")
      text(1, 1, paste0("Your grade = ", round(yourGrade, 2)), cex = 4)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
