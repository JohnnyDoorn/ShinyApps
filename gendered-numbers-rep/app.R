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
library(summarytools)
library(DT)
library(crosstable)

dat <- read.csv("~/Downloads/GenderedNumbers_ReplicationAttemptBinary.csv")[, -(1:2) ]
colnames(dat) <- c(paste0("Number-", 0:9), "Gender")

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Gendered Numbers Replication"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("whatNum", label = "Number:", value = 0, 
                  min = 0, max = 9, step = 1),
      checkboxInput("binaryGen", "Adopt binary view on gender", value = FALSE),
      checkboxInput("showExp", "Show expected values", value = FALSE)),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      DTOutput("regTable"),
      verbatimTextOutput("rOutput")
    )
  )
)






# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  
  output$regTable <- renderDT({
    
    if (input$binaryGen) {
      dat <- subset(dat, Gender == "Woman" | Gender == "Man")
    }
    
    Association <- dat[[paste0("Number-", input$whatNum)]]
    Gender <- dat$Gender
    thisTable <- table(Gender, Association)
    dfTab <- as.data.frame.matrix(thisTable)
    if (input$showExp) {
      dfTab <- cbind(dfTab, chisq.test(thisTable)$expected)
      colnames(dfTab) <- c("Feminine", "Masculine", "Expected-F", "Expected-M")
    }
    dfTab[["Total"]] <- rowSums(dfTab[, 1:2])
    dfTab['Total', ] <- colSums(dfTab)
    datatable(dfTab, rownames   = TRUE, 
              extensions = 'Buttons',
              options = list(searching = FALSE, 
                             scrollY   = 350, 
                             paging    = F, 
                             info      = F,                          
                             dom       = 'Bfrtip',
                             buttons   = c('')))
  })
  
  
  output$rOutput <- renderPrint({
    if (input$binaryGen) {
      dat <- subset(dat, Gender == "Woman" | Gender == "Man")
    }
    
    Association <- dat[[paste0("Number-", input$whatNum)]]
    Gender <- dat$Gender
    thisTable <- table(Gender, Association)
    chisq.test(thisTable, correct = FALSE)
  })
  
  
  
  
}



# Run the application 
shinyApp(ui = ui, server = server)
