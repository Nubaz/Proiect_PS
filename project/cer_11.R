cer11_ui <- function(id) {
  ns <- NS(id) # Desemnarea namespace-ului
  
  fluidPage(
    titlePanel("Generarea histogramei și a diagramei boxplot"),
    
    fileInput(ns("file"), label = h3("Introdu fisier")),
    
    hr(),
    fluidRow(column(4, verbatimTextOutput(ns("value")))),
    
    plotOutput(ns("data"))
    
  )
}

cer11_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)
    
    file <- reactive(input$file)
    
    output$value <- renderPrint({
      name <- file()$name
      # Incarca fisierul csv
      x <- read.csv(name, header = FALSE)
      x <- as.integer(unlist(x))
      print(x)
    })
    
    output$data <- renderPlot({
      name <- file()$name
      # Incarca fisierul csv
      x <- read.csv(name, header = FALSE)
      x <- as.integer(unlist(x))
      par(mfrow = c(1, 1))
      
      # Histograma
      hist(x, probability = TRUE, ylab = "", col = "grey",
           axes = FALSE, main = "")
      
      axis(1)
      
      # Densitate
      lines(density(x), col = "red", lwd = 2)
      
      # Boxplot
      par(new = TRUE)
      boxplot(x, horizontal = TRUE, axes = FALSE,
              lwd = 2, col = rgb(0, 1, 1, alpha = 0.15))
    })
  })
}