library(discreteRV)
library(MASS)

cer5_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    titlePanel("Afișarea unei variabile aleatoare"),
    
    sidebarLayout(
      sidebarPanel(
        numericInput(ns("nr_val"), "Număr valori", value = 4, min = 2, max = 100)
      ),
      
      mainPanel(
        h3("Variabila aleatoare"),
        htmlOutput(ns("rv"))
      )
    )
  )
}

cer5_server <- function(id) {
  moduleServer(id, function(input, output, session) {
      vals <- reactiveValues()
      
      observe({
        vals$nr_val <- input$nr_val
        
        vals$nr <- sort(sample.int(100,input$nr_val))
        vals$p <- round(runif(input$nr_val,0.1,0.9),1)
        vals$p2 <- vals$p/sum(vals$p)
        #vals$dRV <- RV(vals$nr, vals$p2, fractions = T)
      })
      
      output$rv <- renderUI({
        withMathJax(html("$$x\sim 2$$"))
      })
  })
}