cer12_ui <- function(id) {
  ns <- NS(id) # Desemnarea namespace-ului
  
  fluidPage(
    titlePanel("Operații cu v.a. discrete"),
    sidebarLayout(
      sidebarPanel(
        numericInput(ns("nr_val"), "Număr valori:", value = 4, min = 2, max = 10),
        actionButton(ns("adunare"), "Adunare"),
        actionButton(ns("scadere"), "Scădere"),
        actionButton(ns("produs"), "Produs"),
        actionButton(ns("raport"), "Raport")
      ),
      
      mainPanel(
        h3("Variabila aleatoare X"),
        verbatimTextOutput(ns("rv")),
        
        h3("Variabila aleatoare Y"),
        verbatimTextOutput(ns("rv1")),
        
        h3("Rezultat"),
        verbatimTextOutput(ns("rv2"))
      )
    )
  )
}

cer12_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)
    
    vals <- reactiveValues()
    vals2 <- reactiveValues()
    
    # Constuirea v.a. discrete X
    observe({
      vals$nr_val <- input$nr_val
      
      vals$nr <- sort(sample.int(10,input$nr_val))
      vals$p <- round(runif(input$nr_val,0.1,0.9),1)
      vals$p2 <- vals$p/sum(vals$p)
      vals$dRV <- RV(vals$nr, vals$p2)
    })
    
    # Constuirea v.a. discrete Y
    observe({
      vals2$nr_val <- input$nr_val
      
      vals2$nr <- sort(sample.int(10,input$nr_val))
      vals2$p <- round(runif(input$nr_val,0.1,0.9),1)
      vals2$p2 <- vals2$p/sum(vals2$p)
      vals2$dRV <- RV(vals2$nr, vals2$p2)
    })
    
    # Afisarea v.a. discrete X
    output$rv <- renderText({
      paste(collapse = "\n",
            capture.output(
              fractions(probs(vals$dRV))
            )
      )
    })
    
    # Afisarea v.a. discrete Y
    output$rv1 <- renderText({
      paste(collapse = "\n",
            capture.output(
              fractions(probs(vals2$dRV))
            )
      )
    })

    buttonAdunare <- reactive(input$adunare)
    buttonScadere <- reactive(input$scadere)
    buttonProdus <- reactive(input$produs)
    buttonRaport <- reactive(input$raport)
    
    tip <- ""
    
    # Afisarea rezultatului dupa adunare
    textAdunare <- eventReactive(buttonAdunare(), {
      tip <- "+"
      paste(collapse = "\n",
            capture.output(
              fractions(probs(vals$dRV + vals2$dRV))
            )
      )
    })
    
    # Afisarea rezultatului dupa scadere
    textScadere <- eventReactive(buttonScadere(), {
      tip <- "-"
      paste(collapse = "\n",
            capture.output(
              fractions(probs(vals$dRV - vals2$dRV))
            )
      )
    })
    
    # Afisarea rezultatului dupa produs
    textProdus <- eventReactive(buttonProdus(), {
      tip <- "*"
      paste(collapse = "\n",
            capture.output(
              fractions(probs(vals$dRV * vals2$dRV))
            )
      )
    })
    
    # Afisarea rezultatului dupa raport
    textRaport <- eventReactive(buttonRaport(), {
      tip <- "/"
      paste(collapse = "\n",
            capture.output(
              fractions(probs(vals$dRV * (vals2$dRV)^-1))
            )
      )
    })
    
    output$rv2 <- renderText({
      textAdunare()
    })
  })
}