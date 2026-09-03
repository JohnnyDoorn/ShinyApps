# Load required packages
library(shiny)
library(dplyr)
 
# Define UI
ui <- fluidPage(
  titlePanel("Snack Preference vs. Nationality (Chi-Square Tests)"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload Microsoft Forms CSV",
                accept = c(".csv")),
      tags$hr(),
      # helpText("Upload your Microsoft Forms CSV containing the following columns:"),
      # tags$ul(
      #   tags$li("'What is your nationality?'"),
      #   tags$li("'What is your opinion on Stroopwafels?'"),
      #   tags$li("'What is your opinion on Drop?'"),
      #   tags$li("'What is your opinion on Bitterballen?'")
      # )
    ),
    
    mainPanel(
      h4("Chi-Square Test Results"),
      tableOutput("chiResults"),
      tags$hr(),
      h4("Contingency Tables (Frequencies)"),
      uiOutput("tables_ui")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Read the uploaded CSV
  dataInput <- reactive({
    # req(input$file)
    # read.csv(input$file$datapath, stringsAsFactors = TRUE, sep = ";")
    read.csv("RMS Snack Assessment 2025(Sheet1).csv", stringsAsFactors = TRUE, sep = ";")
  })
  
  # Compute Chi-square tests
  output$chiResults <- renderTable({
    df <- dataInput()
    
    snacks <- c("What.is.your.opinion.on.Stroopwafels.",
                "What.is.your.opinion.on.Drop.",
                "What.is.your.opinion.on.Bitterballen.")
    
    results <- lapply(snacks, function(snack) {
      if (snack %in% names(df)) {
        tbl <- table(df[[snack]], df[["What.is.your.nationality."]])
        test <- suppressWarnings(chisq.test(tbl))
        data.frame(
          Snack = gsub("What.is.your.opinion.on.|\\.", " ", snack),
          Chi.Square = round(test$statistic, 2),
          df = test$parameter,
          p.value = round(test$p.value, 4)
        )
      }
    })
    do.call(rbind, results)
  })
  
  # Generate frequency contingency tables
  output$tables_ui <- renderUI({
    # req(input$file)
    df <- dataInput()
    
    snacks <- c("What.is.your.opinion.on.Stroopwafels.",
                "What.is.your.opinion.on.Drop.",
                "What.is.your.opinion.on.Bitterballen.")
    
    # Build one table per snack
    tables <- lapply(snacks, function(snack) {
      if (snack %in% names(df)) {
        pretty_name <- gsub("What.is.your.opinion.on.|\\.", " ", snack)
        tagList(
          h5(strong(pretty_name)),
          tableOutput(paste0("tbl_", snack)),
          tags$hr()
        )
      }
    })
    
    do.call(tagList, tables)
  })
  
  # Render each frequency contingency table
  observe({
    # req(input$file)
    df <- dataInput()
    snacks <- c("What.is.your.opinion.on.Stroopwafels.",
                "What.is.your.opinion.on.Drop.",
                "What.is.your.opinion.on.Bitterballen.")
    
    for (snack in snacks) {
      if (snack %in% names(df)) {
        local({
          s <- snack
          output[[paste0("tbl_", s)]] <- renderTable({
            table(df[[s]], df[["What.is.your.nationality."]])
          }, rownames = TRUE)
        })
      }
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)
