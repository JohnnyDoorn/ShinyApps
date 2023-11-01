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
    checkboxInput(inputId = "showTheoreticalSamp",
                  label = "Show Theoretical Distribution",value = FALSE)
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    tabsetPanel(type = "tabs",
                # tabPanel("Constructing Sampling Dist", plotOutput("distPlot")),
                # tabPanel("Complete Data", htmlOutput("message"), 
                #          style="font-size: 40px;"),
                tabPanel("Observed Data", plotOutput("message")),
                tabPanel("Estimating the Parameter?",  plotOutput("sampPlot"))
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  results <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1RQp7gSbljHW-_SSU--ufcANJ0yycbAm2FJ8uplu5b2o/edit?usp=sharing")
  noMonth <- sapply(strsplit(results$Timestamp, split = "/"), function(x) x[[3]])
  years <- sapply(strsplit(noMonth, split = " "), function(x) x[[1]])
  results <- results[years == "2023", ]
  resp <- oriResp <- ifelse(results[[2]] == "Black & blue", "blackBlue", "whiteGold")
  popSize <- length(resp)
  
  # output$distPlot <- renderPlot({
  #   
  #   # if (input$source != "Observed") {
  #   set.seed(123)
  #   resp <- sample(resp, size = 1e4, replace = TRUE)
  #   # }
  #   popValue <- obsProp <- sum(resp == "whiteGold")/ length(resp)
  #   
  #   grNumbers <- rep(1:length(resp), each = input$ns)
  #   maxGroup <- length(resp) %/%  input$ns
  #   obsProps <- sapply(1:maxGroup, function(x){ sum(resp[which(grNumbers == x)] == "whiteGold")/  input$ns  })
  #   obsProps <- obsProps[!is.na(obsProps)]
  #   maxIndex <- min(input$showN, length(obsProps))
  #   maxY <- ifelse(input$showN < 30, 12, round(max(table(obsProps[1:maxIndex])) + 2))
  #   
  #   par(cex.lab = 1.2, cex = 1.4)
  #   myTable <- table(factor(round(obsProps[1:maxIndex], 2), levels = round((0:input$ns) / input$ns, 2)))
  #   barplot(myTable, ylim = c(0, maxY), space = 0,  col = c("orange", "purple"), ylab = "Counts", xlab = "Observed proportion",
  #           main = bquote(bar(p) ~ "=" ~ .(round(mean(obsProps[1:maxIndex]), 4))))
  #   mtext(side = 3, text = paste0("Population size = ", popSize))
  #   
  #   if (input$showPop) {
  #     abline(v = popValue * length(myTable) , lwd = 4, col = "darkgreen")
  #   }
  # })
  
  output$sampPlot <- renderPlot({
    
    # if (input$source != "Observed") {
    #   set.seed(123)
    #   resp <- sample(resp, size = 1e4, replace = TRUE)
    #   popSize <- Inf
    # }
    popValue <- obsProp <- sum(oriResp == "whiteGold")/ length(oriResp)
    
    binomSamples <- rbinom(1e6, popSize, obsProp)/ popSize
    par(cex = 1.4, cex.lab = 1.4)
    hist(binomSamples, col = rainbow(10), xlab = "Observed Proportion", xlim = c(0,1),
         main = "", freq = FALSE)
    if (input$showTheoreticalSamp) {
      thisSD <- sqrt((obsProp * (1-obsProp)) / popSize)
      curve(dnorm(x, mean = obsProp, sd = thisSD), from = 0, to = 1, lwd = 4, col= "black",add =TRUE)
      mtext(paste0("Mean = ", round(obsProp, 3), "\nSE = sqrt((p * (1-p)) / n) = ", round(thisSD, 3)), cex = 2)
    }
    
  })
  
  output$message <- renderPlot({
    
    blue <- sum(results[[2]] == "Black & blue")
    gold <- sum(results[[2]] != "Black & blue")
    # mydf <- data.frame('Blue' = c(blue, round(blue/length(results[[2]]), 2)), 
    #                    'White' = c(gold, round(gold/length(results[[2]]), 2)))
    # mydf}, striped = TRUE)
    par(cex = 1.4, oma = c(3, 0, 0, 0))
    barplot(c(blue, gold)/length(results[[2]]), names.arg = c("Black & blue", "White & gold") , col = c("darkblue", "gold"), las=1)
    mtext(side = 1, text = paste0("Black & blue: ",blue, " (",round(blue/length(results[[2]]), 2), 
                                  ")\nWhite & gold: ", gold," (",round(gold/length(results[[2]]), 2), ")") , cex = 2,
          outer = TRUE)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
