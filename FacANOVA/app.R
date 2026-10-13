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

# Unbalance the 3 x 3 design (20 per cell) by keeping fewer participants in some cells, so
# that alcohol and speed become associated. Positive: keep the cells where the doses match
# (heavy drinkers also took much speed); negative: keep the cells where they are opposite.
# The cells furthest from that pattern keep 15% at +-1 (3 of 20), and at least 2 each.
unbalanceDesign <- function(data, association) {
  doses <- c("none", "some", "much")
  a <- match(data$alcohol, doses)
  s <- match(data$speed, doses)
  distance <- if (association >= 0) abs(a - s) else abs(a + s - 4)
  keep <- 1 - abs(association) * 0.85 * distance / 2
  cells <- split(seq_len(nrow(data)), paste(a, s))
  rows <- unlist(lapply(cells, function(r) {
    n <- max(2, round(length(r) * keep[r[1]]))
    r[round(seq(1, length(r), length.out = n))]
  }))
  data[sort(rows), ]
}

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
                                             "Overlay full model", value = FALSE),
                               sliderInput("association",
                                           "Association between alcohol and speed (0 = balanced design)",
                                           min = -1, max = 1, value = 0, step = 0.25))
  ),

  dashboardBody(
    tabsetPanel(
      tabPanel("Sums of squares",
               plotOutput("distPlot", height = "1800px")),
      tabPanel("Variance partition",
               radioButtons("ssType", "Sums of squares type",
                            c("Type I (alcohol first)" = "I-alcohol", "Type I (speed first)" = "I-speed",
                              "Type II" = "II", "Type III (JASP default)" = "III"),
                            selected = "III", inline = TRUE),
               plotOutput("partitionPlot", height = "620px"),
               fluidRow(
                 column(5, h4("Participants per cell"), tableOutput("cellTable"),
                        textOutput("association")),
                 column(7, h4("Why don't the parts add up?"), tableOutput("aloneTable"))),
               fluidRow(
                 column(12, h4("Sums of squares by type"), tableOutput("typeTable"))))
    )
  )
)

server <- function(input, output, session) {

  shownData <- reactive(unbalanceDesign(mydat, input$association))
  partition <- reactive(partitionFactorial(shownData(), input$ssType))

  output$distPlot <- renderPlot({
    par(mfrow = c(3, 1), cex = 1.35)
    for (s in c("Total", "Error", "Model"))
      plotSumSquaresFactorial(shownData(), input = input, sumSq = s, sortBy = input$sortBy,
                              overlayFullPred = s == "Error" && input$overlayFullPred)
  }, width = 1200, height = 1800)

  output$partitionPlot <- renderPlot({
    plotVariancePartitionFactorial(shownData(), type = input$ssType)
  }, width = 1100, height = 620)

  output$cellTable <- renderTable({
    cells <- addmargins(partition()$cells)
    dimnames(cells) <- list(Alcohol = c("none", "some", "much", "Total"), Speed = c("none", "some", "much", "Total"))
    out <- as.data.frame.matrix(cells)
    cbind(`Alcohol / speed` = rownames(out), out)
  }, digits = 0)

  output$association <- renderText({
    sprintf("Correlation between alcohol and speed dose: r = %.2f", partition()$r)
  })

  # Same layout as the lecture 13 slide "Why don't the parts add up?"
  output$aloneTable <- renderTable({
    p <- partition()
    fmt <- function(x) sprintf("%.2f", x)
    data.frame(` ` = c("Alone (one-predictor model)", "Unique (main-effects model minus the other)",
                       "Main-effects model (alcohol + speed)", "Shared (alone minus unique)"),
               Alcohol = c(fmt(p$alone[["alcohol"]]), fmt(p$unique[["alcohol"]]), "", ""),
               Speed   = c(fmt(p$alone[["speed"]]),   fmt(p$unique[["speed"]]),   "", ""),
               Sum     = c(fmt(sum(p$alone)), fmt(sum(p$unique)), fmt(p$main), fmt(p$shared)),
               check.names = FALSE)
  }, align = "lrrr")
  
  # The same data split four ways: the types differ only in where the shared part goes.
  output$typeTable <- renderTable({
    types <- c("Type I (alcohol first)" = "I-alcohol", "Type I (speed first)" = "I-speed",
               "Type II" = "II", "Type III (JASP default)" = "III")
    parts <- sapply(types, function(t) {
      p <- partitionFactorial(shownData(), t)
      c(p$terms, shared = p$sharedType, model = p$model)
    })
    rowNames <- data.frame(` ` = c("Alcohol", "Speed", "Alcohol × speed", "Shared (no single effect)", "Model"),
                           check.names = FALSE)
    cbind(rowNames, matrix(sprintf("%.2f", parts), nrow = 5, dimnames = list(NULL, names(types))))
  }, align = "lrrrr")
}


# Run the application
shinyApp(ui = ui, server = server)
