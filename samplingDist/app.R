#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library("gsheet")


# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Blue or Gold Dress???"),
  # Sidebar with a slider input for number of bins 
  sidebarPanel(
    # radioButtons(inputId = "source",
    #              label = "Source:",
    #              choices = c("Observed", "Simulated")),
    # checkboxInput(inputId = "showPop",
    #               label = "Show Total",value = FALSE),
    checkboxInput(inputId = "prevYears",
                  label = "Include previous years",value = FALSE),
    checkboxInput(inputId = "showTheoreticalSamp",
                  label = "Show Theoretical Distribution",value = FALSE),
    checkboxInput(inputId = "include05",
                  label = "Mark majority line",value = FALSE)
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    tabsetPanel(type = "tabs",
                # tabPanel("Constructing Sampling Dist", plotOutput("distPlot")),
                # tabPanel("Complete Data", htmlOutput("message"), 
                #          style="font-size: 40px;"),
                tabPanel("Observed Data", plotOutput("message")),
                tabPanel("Sampling Distribution",  plotOutput("sampPlot"))
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  results <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1RQp7gSbljHW-_SSU--ufcANJ0yycbAm2FJ8uplu5b2o/edit?usp=sharing")
  noMonth <- sapply(strsplit(results$Timestamp, split = "/"), function(x) x[[3]])
  years <- sapply(strsplit(noMonth, split = " "), function(x) x[[1]])

  # Create a reactive expression for filtered results based on input$prevYears
  filteredResults <- reactive({
    if (!input$prevYears) {
      # If prevYears is FALSE, show only results from 2024
      results[years == "2024", ]
    } else {
      # If prevYears is TRUE, show all years
      results
    }
  })
  
  # Reactive expression for responses (filtered)
  resp <- reactive({
    filtered <- filteredResults()
    ifelse(filtered[[2]] == "Black & blue", "blackBlue", "whiteGold")
  })
  
  # Reactive expression to calculate population size
  popSize <- reactive({
    length(resp())
  })
  
  # Reactive expression for the observed proportion of "whiteGold"
  obsProp <- reactive({
    sum(resp() == "whiteGold") / popSize()
  })

  
  output$sampPlot <- renderPlot({
    popValue <- obsProp()
    n <- popSize()
    
    binomSamples <- rbinom(1e6, n, popValue)/ n
    par(cex = 1.4, cex.lab = 1.4)
    hist(binomSamples, col = rainbow(30), xlab = "Observed Proportion", xlim = c(0.2,0.8),
         main = "", freq = FALSE, las = 1)
    if (input$showTheoreticalSamp) {
      thisSD <- sqrt((popValue * (1-popValue)) / n)
      curve(dnorm(x, mean = popValue, sd = thisSD), from = 0, to = 1, lwd = 4, col= "black",add =TRUE)
      mtext(paste0("Mean = Prop(W&G) = ", round(popValue, 3), "\nSE = sqrt((p * (1-p)) / n) = ", round(thisSD, 3)), cex = 2)
    }
    if(input$include05){
      abline(v = 0.5, lwd = 4, col = "black")
      abline(v = 0.5, lwd = 2, col = "darkorange")
      
    }
    
  })
  
  output$message <- renderPlot({
    
    blue <- sum(filteredResults()[[2]] == "Black & blue")
    gold <- sum(filteredResults()[[2]] != "Black & blue")

    par(cex = 1.4, oma = c(3, 0, 0, 0))
    barplot(c(blue, gold)/length(filteredResults()[[2]]), names.arg = c("Black & blue", "White & gold") , col = c("darkblue", "gold"), las=1)
    mtext(side = 1, text = paste0("Black & blue: ",blue, " (",round(blue/length(filteredResults()[[2]]), 2), 
                                  ")\nWhite & gold: ", gold," (",round(gold/length(filteredResults()[[2]]), 2), ")") , cex = 2,
          outer = TRUE)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
