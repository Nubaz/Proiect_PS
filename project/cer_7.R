library(discreteRV)
library(MASS)

cer7_ui <- function(id) {
  ns <- NS(id) # Desemnarea namespace-ului
 
  
  fluidPage(
    titlePanel("Transformarea unei variabile aleatoare discrete"),
    
    sidebarLayout(
      sidebarPanel(
        numericInput(ns("nr_val"), "Număr valori:", value = 4, min = 2, max = 10),
        textInput(ns("fct"), "Funcția de aplicat:"),
        actionButton(ns("apl_fct"), "Aplică funcția")
      ),
      
      mainPanel(
        h3("Variabila aleatoare"),
        verbatimTextOutput(ns("rv")),
        
        conditionalPanel(
          ns = ns,
          
          condition = "input.apl_fct",
          
          h3("Variabila aleatoare transformată"),
          verbatimTextOutput(ns("rv_2"))
        )
      )
    )
  ) 
}

cer7_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)
    
    vals <- reactiveValues()
    
    # Constuirea v.a. discrete
    observe({
      vals$nr_val <- input$nr_val
      
      vals$nr <- sort(sample.int(10,input$nr_val))
      vals$p <- round(runif(input$nr_val,0.1,0.9),1)
      vals$p2 <- vals$p/sum(vals$p)
      vals$dRV <- RV(vals$nr, vals$p2)
    })

    # Afisarea v.a. discrete
    output$rv <- renderText({
      paste(collapse = "\n",
            capture.output(
              fractions(probs(vals$dRV))
            )
      )
    })

    # Variabilele functiei
    # organizate în alt domeniu reactiv
    f <- reactive(input$fct)
    button <- reactive(input$apl_fct)
    
    # Calculul functiei primite ca parametru
    g <- function(X) {
      eval(parse(text = f()))
    }
    
    # Afisarea v.a. discrete dupa transformare
    text <- eventReactive(button(), {
      paste(collapse = "\n",
            capture.output(
              fractions(probs(fractions(g(RV(vals$nr, vals$p2)))))
            )
      )
    })
    
    output$rv_2 <- renderText({
      text()
    })
  })
}