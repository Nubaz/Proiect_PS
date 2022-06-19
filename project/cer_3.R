cer3_ui <- function(id) {
  ns <- NS(id) # Desemnarea namespace-ului
  
  fluidPage(
    titlePanel("Lucrul cu Evenimente"),
  
    sidebarLayout(
      
      sidebarPanel(
        radioButtons("radio", label = h2("Cum sunt cele doua evenimente?"),
                 choices = list("Incompatibile" = 1, "Independente" = 2, "Nu stim nimic despre ele" = 3), 
                 selected = 1),
        
        numericInput("prob_A", label = h3("Probabilitatea lui A"), value = 0.5, min = 0.1, max = 0.9),
        numericInput("prob_B", label = h3("Probabilitatea lui B"), value = 0.5, min = 0.1, max = 0.9)),
      
      mainPanel(
        h4("P(A si B)"),
        verbatimTextOutput(ns("and")),
        
        h4("P(A sau B)"),
        verbatimTextOutput(ns("or")),
        
        h4("P(A conditionat de B)"),
        verbatimTextOutput(ns("AconB")),
        
        h4("P(B conditionat de A)"),
        verbatimTextOutput(ns("BconA"))
      )
    ))}

cer3_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)
    
    output$and <- renderText({switch(input$radio,
                                     "Imposibil", #Incompatibile
                                     input$prob_A * input$prob_B, #Independente
                                     input$prob_A * input$prob_B)}) #Necunoscut
    
    output$or <- renderText({switch(input$radio,
                                    input$prob_A + input$prob_B - input$prob_A * input$prob_B, #Incompatibile
                                     input$prob_A + input$prob_B, #Independente
                                     input$prob_A + input$prob_B)}) #Necunoscut
    
    output$AconB <- renderText({switch(input$radio,
                                     "Imposibil", #Incompatibile 
                                     input$prob_A, #Independente
                                     input$prob_A * input$prob_B / input$prob_B)}) #Necunuoscut
    
    output$BconA <- renderText({switch(input$radio,
                                     "Imposibil", #Incompatibile
                                     input$prob_B, #Independente
                                     input$prob_A * input$prob_B / input$prob_A)}) #Necunoscut
  })
}