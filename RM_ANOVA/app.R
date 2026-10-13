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

# Shared with the SSR lecture 18 slides (ssr/plotFunctionsSSR.r); keep the copies identical.
source("plotSquaresFunction.R")
mydat <- read.csv("RM_ANOVA_Alcohol_LONG.csv")

# Define UI for application that draws a histogram
ui <-  dashboardPage(
  dashboardHeader(title = "Within Squares"),
  
  dashboardSidebar(width = 220,
                   sidebarMenu(style = "position:fixed;width:220px;",
                               (radioButtons("whatPred",
                                             "What model do we predict with?",
                                             c("Mean", "Alcohol", "ID", "Alcohol + ID"), 
                                             selected = "Mean")),
                               radioButtons("sortBy",
                                            "Sort observations by",
                                            c("Participant" = "participant", "Alcohol" = "alcohol"),
                                            selected = "participant"),
                               checkboxGroupInput("whatDisplay",
                                                  "Display:",
                                                  c("Segments", "Sums", "F-stat")),
                               # checkboxInput("overlayFullPred",
                               #               "Overlay full model", value = FALSE),
                               sliderInput("showSampleSize",
                                             "Display (n)", value = 10, min = 4, max = 20))
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
    shown <- mydat[mydat$pp <= input$showSampleSize, ]
    for (s in c("Total", "Error", "Model"))
      plotSumSquaresRM(shown, input = input, sumSq = s, sortBy = input$sortBy)
    
  }, width = 1200, height = 1800)
  
}



# Run the application 
shinyApp(ui = ui, server = server)
