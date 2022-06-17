library(discreteRV)
library(MASS)

cer5_ui <- function(id) {
  ns <- NS(id) # Desemnarea namespace-ului
  
  fluidPage(
    titlePanel("Afișarea unei variabile aleatoare discrete"),
    
    sidebarLayout(
      sidebarPanel(
        numericInput(ns("nr_val"), "Număr valori:", value = 4, min = 2, max = 300),
        uiOutput(ns("ui_prag"))
      ),
      
      mainPanel(
        h3("Variabila aleatoare"),
        verbatimTextOutput(ns("rv"))
      )
    )
  )
}

cer5_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)
    
    # Stocarea valorilor modificabile intr-o variabila
    vals <- reactiveValues()
  
    # Afișarea selecției poziției de început, pe baza căreia va fi modifcată
    # afișarea v.a. discrete
    output$ui_prag <- renderUI({
        numericInput(ns("prag_val"), "Poziție de început:", value = 1, min = 1, max = input$nr_val)
    })
    
    observe({
      vals$prag <- input$prag_val
    })
    
    # Generarea v.a.
    observe({
      vals$nr_val <- input$nr_val
      
      vals$nr <- sort(sample.int(300,input$nr_val))
      vals$p <- round(runif(input$nr_val,0.1,0.9),1)
      vals$p2 <- vals$p/sum(vals$p)
      vals$dRV <- RV(vals$nr, vals$p2)
    })
    
    # Afisarea v.a. discrete
    output$rv <- renderText({
      paste(collapse = "\n",
            capture.output(
              if(vals$prag == 1 || is.null(vals$prag)) {
                fractions(probs(vals$dRV))
              } else {
                fractions(probs(vals$dRV))[c(seq.int(vals$prag,vals$nr_val))]
              }
            )
      )
    })
  })
}