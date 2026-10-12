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
library(shinydashboard)

# Shared with the SSR lecture 17 slides (ssr/plotFunctionsSSR.r); keep the copies identical.
source("plotSquaresFunction.R")
mydat <- read.csv("anova_alcohol_speed_daytime.csv")

# Define UI for application that draws a histogram
ui <-  dashboardPage(
  dashboardHeader(title = "More Squares"),
  
  dashboardSidebar(width = 220,
                   sidebarMenu(style = "position:fixed;width:220px;",
                               (radioButtons("whatPred",
                                             "What model do we predict with?",
                                             c("Mean", "Alcohol", "Speed", "Alcohol + Speed", "Alcohol + Speed + A:S"), 
                                             selected = "Mean")),
                               radioButtons("sortBy",
                                            "Sort participants by",
                                            c("Alcohol" = "alcohol", "Speed" = "speed"),
                                            selected = "alcohol"),
                               checkboxGroupInput("whatDisplay",
                                                  "Display:",
                                                  c("Segments", "Sums", "F-stat")),
                               checkboxInput("overlayFullPred",
                                             "Overlay full model", value = FALSE))
  ),
  
  dashboardBody(
    plotOutput('distPlot')
  )
  
  
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    # myCols <- palette.colors(n = 9, palette = "Okabe-Ito")
    
    par(mfrow = c(3, 1), cex = 1.35)
    for (s in c("Total", "Error", "Model"))
      plotSumSquaresFactorial(mydat, input = input, sumSq = s, sortBy = input$sortBy,
                              overlayFullPred = s == "Error" && input$overlayFullPred)
    
  }, width = 1200, height = 1800)
  
}


# Run the application 
shinyApp(ui = ui, server = server)
