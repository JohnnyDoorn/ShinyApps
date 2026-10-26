library(shiny)

# Multiple regression: collinearity and interaction side by side, so students can see that
# they are different things, with the variables of DSUJ's album sales example. Merged on
# 2026-10-26 from the earlier interaction applet (sums of squares, slope at a value of the
# other predictor) and a collinearity applet. helpers.R holds the tables, simulation and plots;
# its tables reproduce DSUJ's album sales output (chapter 8).
source("helpers.R", encoding = "UTF-8")

ui <- fluidPage(

  tags$head(tags$style(HTML(jaspTableCss))),

  titlePanel("Multiple regression: collinearity and interaction"),

  sidebarLayout(
    sidebarPanel(
      sliderInput("n",
                  "Sample size (n):",
                  min = 10,
                  max = 500,
                  step = 5,
                  value = 50),
      sliderInput("r12",
                  "Correlation between Adverts and Airplay:",
                  min = -0.99,
                  max = 0.99,
                  step = 0.01,
                  value = 0,
                  ticks = FALSE),     # ticks would land on -0.79, -0.59, ...
      sliderInput("b1",
                  "Effect of Adverts:",
                  min = -1,
                  max = 1,
                  step = 0.05,
                  value = 0.5),
      sliderInput("b2",
                  "Effect of Airplay:",
                  min = -1,
                  max = 1,
                  step = 0.05,
                  value = 0.5),
      sliderInput("b12",
                  "Interaction Adverts \u00D7 Airplay:",
                  min = -1,
                  max = 1,
                  step = 0.05,
                  value = 0),
      checkboxInput("addInteraction", "Add the interaction as model M\u2083", value = FALSE),
      checkboxInput("exact", "Sample correlation exactly as set", value = TRUE),
      actionButton("newSample", "New sample"),
      helpText("Adverts, Airplay and the error all have SD 1 (z-scores), so the effects are in SD units.",
               "M\u2081 holds Adverts only, M\u2082 adds Airplay.")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Scatterplots", plotOutput("matrixPlot", height = "640px")),
        tabPanel("3D plot",
                 radioButtons("plane", "Plane of:",
                              c("M\u2082: Adverts + Airplay" = "main", "M\u2083: with the interaction" = "full"),
                              selected = "full", inline = TRUE),
                 plotly::plotlyOutput("plot3d", height = "600px")),
        tabPanel("Sums of squares",
                 radioButtons("whatPred", "What model do we predict with?",
                              c("Null" = "null", "Adverts" = "x1", "Airplay" = "x2", "Adverts + Airplay" = "main",
                                "Adverts + Airplay + Adverts \u00D7 Airplay" = "full"),
                              selected = "null", inline = TRUE),
                 checkboxGroupInput("whatDisplay", "Display:", c("Segments", "Sums", "R2"), inline = TRUE),
                 checkboxInput("predictWithMainModel", "Use main effects model as null (full model only)",
                               value = FALSE),
                 plotOutput("ssPlot", height = "480px")),
        tabPanel("JASP output", uiOutput("jaspTables")),
        tabPanel("Interaction",
                 sliderInput("showAt", "Slope of Adverts at Airplay =", min = -2, max = 2, step = 0.1, value = 0),
                 plotOutput("interactionPlot", height = "480px")),
        tabPanel("Sampling distribution", plotOutput("samplingPlot", height = "460px"))
      )
    )
  )
)

server <- function(input, output, session) {

  seed <- reactiveVal(2026)
  observeEvent(input$newSample, seed(seed() + 1))

  dat <- reactive({
    simulateData(input$n, input$r12, input$b1, input$b2, input$b12, seed = seed(), exact = input$exact)
  })

  terms <- reactive({
    tt <- list(character(0), "Adverts", c("Adverts", "Airplay"))
    if (input$addInteraction) tt[[4]] <- c("Adverts", "Airplay", "Adverts:Airplay")
    tt
  })

  fits <- reactive(fitHierarchy(dat(), "Sales", terms()))

  output$matrixPlot <- renderPlot({
    plotMatrix(dat())
  })

  output$plot3d <- plotly::renderPlotly({
    plot3D(dat(), input$plane)
  })

  output$ssPlot <- renderPlot({
    d <- dat()
    form <- switch(input$whatPred,
                   null = Sales ~ 1, x1 = Sales ~ Adverts, x2 = Sales ~ Airplay,
                   main = Sales ~ Adverts + Airplay, full = Sales ~ Adverts * Airplay)
    mainNull <- input$predictWithMainModel && input$whatPred == "full"
    nullPredictions <- if (mainNull) fitted(lm(Sales ~ Adverts + Airplay, data = d)) else rep(mean(d$Sales), nrow(d))
    plotSumSquares(d$Sales, fitted(lm(form, data = d)), nullPredictions, input$whatDisplay, mainNull)
  })

  output$jaspTables <- renderUI({
    HTML(jaspTablesHtml(fits(), terms(), dv = "Sales"))
  })

  output$interactionPlot <- renderPlot({
    plotInteraction(dat(), input$showAt)
  })

  output$samplingPlot <- renderPlot({
    plotSamplingNull(fits()[[2]], fits()[[3]])
  })
}

shinyApp(ui = ui, server = server)
