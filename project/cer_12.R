cer12_ui <- function(id) {
  ns <- NS(id) # Desemnarea namespace-ului
  
  fluidPage(
    titlePanel("Operații cu v.a. discrete"),
    sidebarLayout(
      sidebarPanel(
        numericInput(ns("nr_val"), "Număr valori:", value = 2, min = 2, max = 10),
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
        
        h3(),
        
        conditionalPanel(
          ns = ns,
          condition = "input.adunare",
          
          h3("Adunarea variabilelor aleatoare"),
          verbatimTextOutput(ns("oper_1"))
        ),
        
        conditionalPanel(
          ns = ns,
          condition = "input.scadere",
          
          h3("Scaderea variabilelor aleatoare"),
          verbatimTextOutput(ns("oper_2"))
        ),
        
        conditionalPanel(
          ns = ns,
          condition = "input.produs",
          
          h3("Produsul variabilelor aleatoare"),
          verbatimTextOutput(ns("oper_3"))
        ),
        
        conditionalPanel(
          ns = ns,
          condition = "input.raport",
          
          h3("Raportul variabilelor aleatoare"),
          verbatimTextOutput(ns("oper_4"))
        ),
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
    
    # Solutie adaptata de pe site-ul Stack Overflow
    prod <- function(X, Y) {
      product.matrix <- t(outer(X[1:length(X)],Y[1:length(Y)],"*"))
      
      probX <- c()
      probY <- c()
      
      for(i in 1:length(X)) {
        probX[i] <- probs(X)[[i]][1]
      }
      for(i in 1:length(Y)) {
        probY[i] <- probs(Y)[[i]][1]
      }
      
      probability.matrix <- t(outer(probX, probY))
      unique.products <- unique(as.vector(product.matrix))
      probability.vector <- rep(0, length(unique.products))
      
      unique.products <- sort(fractions(unique.products))
      
      for(i in 1:length(probability.vector)){
        z <- unique.products[i]
        
        indices <- which(as.vector(product.matrix) == z)
        
        probability.vector[i] <- sum(as.vector(probability.matrix)[indices])
      }
      
      XtimesY <- RV(unique.products, probability.vector)
    }
    
    # Afisarea rezultatului dupa adunare
    output$oper_1 <- eventReactive(buttonAdunare(), {
      paste(collapse = "\n",
            capture.output(
              fractions(probs(vals$dRV + vals2$dRV))
            )
      )
    })
    
    # Afisarea rezultatului dupa scadere
    output$oper_2 <- eventReactive(buttonScadere(), {
      paste(collapse = "\n",
            capture.output(
              fractions(probs(vals$dRV - vals2$dRV))
            )
      )
    })
    
    # Afisarea rezultatului dupa produs
    output$oper_3 <- eventReactive(buttonProdus(), {
      paste(collapse = "\n",
            capture.output(
              fractions(probs(prod(vals$dRV, vals2$dRV)))
            )
      )
    })
    
    # Afisarea rezultatului dupa raport
    output$oper_4 <- eventReactive(buttonRaport(), {
      paste(collapse = "\n",
            capture.output(
              fractions(probs(prod(vals$dRV, (vals2$dRV)^-1)))
            )
      )
    })
  })
}