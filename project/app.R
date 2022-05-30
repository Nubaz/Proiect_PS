library(shiny)
library(shinyjs)
library(Rlab)

ui <- fluidPage(
  titlePanel("Repartiții de variabile aleatoare"),
  
  sidebarLayout(
    sidebarPanel(
      # Pentru resetarea la valorile initiale
      shinyjs::useShinyjs(),
      id = "val",
      
      # Selectie de repartitii
      selectInput("rep", "Repartiție:",
                  c("Normală",
                    "Bernoulli",
                    "Binomială",
                    "Geometrică",
                    "Hipergeometrică",
                    "Uniformă",
                    "Poisson",
                    "Gamma",
                    "Beta")
      ),
      
      # Rep. normala
      conditionalPanel(
        condition = "input.rep == 'Normală'",
        numericInput("norm_dom", "Lungime interval:", min = 4, value = 10),
        sliderInput("norm_x", "Număr valori:", min = 3, max = 50, value = 25),
        numericInput("norm_med", "Medie: ", step = 0.1, value = 0),
        numericInput("norm_var", "Dispersie:", min = 0.1, step = 0.1, value = 1),
        actionButton("norm_rst", "Valori inițiale")
      ),
      
      # Rep. Bernoulli
      #conditionalPanel(
      #  condition = "input.rep == 'Bernoulli'",
      #  numericInput("bprob", "Probabilitate:", value = 0.5)
      #),
      
    ),
    
    # Ploturi
    mainPanel(
      h3("Funcții caracteristice"),
      
      tabsetPanel(
        header = h4(textOutput("med_var")),
        
        tabPanel("Densitate/masă", plotOutput("fdensmasa")),
        tabPanel("Repartiție", plotOutput("frep"))
      )
    )
  )
)

server <- function(input, output, session) {
  # Resetarea la valorile initiale
  observeEvent(input$norm_rst, {
    shinyjs::reset("val")
  })
  
  #output$med_var <- renderText({
  #  paste("\U03BC = ", input$norm_med, "\U03BC = ", input$norm_var)
  #})
  
  # Functii de densitate / masa
  output$fdensmasa <- renderPlot({
    # Variabile - rep. continue
    dom <- input$norm_dom / 2
    X <- seq(-dom, dom, length.out = input$norm_x)
    med <- input$norm_med
    sd <- sqrt(input$norm_var)
    
    switch(input$rep,
           "Normală" = {
              plot(X,dnorm(X,med,sd),xlab="x",ylab="Probabilitate")
              grid(nx=20,ny=20)
              lines(X,dnorm(X,med,sd),col="blue",lwd=2)
            }
            #"Bernoulli" = {
            #  plot(c(0,1),dbern(c(0,1),input$bprob),col="red",type="h")
            #}       
    )
  })
  
  # Functii de repartitie
  output$frep <- renderPlot({
    # Variabile - rep. continue
    dom <- input$norm_dom / 2
    X <- seq(-dom, dom, length.out = input$norm_x)
    med <- input$norm_med
    sd <- sqrt(input$norm_var)
    
    switch(input$rep,
           "Normală" = {
             plot(X,pnorm(X,med,sd),xlab="x",ylab="Probabilitate")
             grid(nx=20,ny=20)
             lines(X,pnorm(X,med,sd),col="red",lwd=2)
           })
  })
}

shinyApp(ui, server)