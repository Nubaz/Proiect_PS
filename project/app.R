library(shiny)
library(shinyjs)
library(Rlab)

ui <- fluidPage(
  titlePanel("Repartiții de variabile aleatoare"),
  
  sidebarLayout(
    sidebarPanel(
      # Selectie de repartitii
      selectInput("rep", "Repartiție:",
                  c("Normală",
                    "Uniformă",
                    "Exponentială",
                    "Bernoulli",
                    "Binomială"
                    )
      ),
      
      # Rep. normala
      conditionalPanel(
        # Pentru resetarea la valorile initiale
        shinyjs::useShinyjs(),
        id = "val_n",
        
        condition = "input.rep == 'Normală'",
        numericInput("norm_dom", "Lungime interval:", min = 4, value = 10),
        sliderInput("norm_x", "Număr valori:", min = 3, max = 50, value = 25),
        numericInput("norm_med", "Medie: ", step = 0.1, value = 0),
        numericInput("norm_var", "Dispersie:", min = 0.1, step = 0.1, value = 1),
      ),
      
      # Rep. uniforma
      conditionalPanel(
        # Pentru resetarea la valorile initiale
        shinyjs::useShinyjs(),
        id = "val_u",
        
        condition = "input.rep == 'Uniformă'",
        sliderInput("unif_x", "Număr valori:", min = 4, max = 50, value = 25),
        numericInput("unif_min", "Valoare minimă (a):", min = 0, max = 49, value = 5),
        numericInput("unif_max", "Valoare maximă (b):", min = 1, max = 48, value = 15),
      ),
      
      # Rep. exponentiala
      conditionalPanel(
        # Pentru resetarea la valorile initiale
        shinyjs::useShinyjs(),
        id = "val_ex",
        
        condition = "input.rep == 'Exponentială'",
        sliderInput("exp_x", "Număr valori:", min = 2, max = 10, value = 5),
        numericInput("exp_lambda", "Lambda:", min = 0.1, value = 1, step = 0.1)
      ),
      
      # Rep. Bernoulli
      conditionalPanel(
        # Pentru resetarea la valorile initiale
        shinyjs::useShinyjs(),
        id = "val_bern",
        
        condition = "input.rep == 'Bernoulli'",
        numericInput("bern_prob", "Probabilitate:", min = 0.1, max = 0.9, value = 0.2, step = 0.1)
      ),
      
      # Rep. binomiala
      conditionalPanel(
        # Pentru resetarea la valorile initiale
        shinyjs::useShinyjs(),
        id = "val_binom",
        
        condition = "input.rep == 'Binomială'",
        sliderInput("binom_x", "Număr valori:", min = 2, max = 100, value = 50),
        numericInput("binom_n", "Nr. experimente:", min = 0, max = 100, value = 100, step = 10),
        numericInput("binom_prob", "Probabilitate:", min = 0.1, max = 0.9, value = 0.2, step = 0.1)
      ),
      
      actionButton("rst", "Valori inițiale")
    ),
    
    # Ploturi
    mainPanel(
      h3("Funcții caracteristice"),
      
      tabsetPanel(
        tabPanel("Densitate/masă", plotOutput("fdensmasa")),
        tabPanel("Repartiție", plotOutput("frep"))
      )
    )
  )
)

server <- function(input, output, session) {
  # Resetarea la valorile initiale
  observeEvent(input$rst, {
    shinyjs::reset("val_n")
    shinyjs::reset("val_u")
    shinyjs::reset("val_ex")
    shinyjs::reset("val_bern")
    shinyjs::reset("val_binom")
  })
  
  # Functii de densitate / masa
  output$fdensmasa <- renderPlot({
    # Variabile - rep. normala
    dom <- input$norm_dom / 2
    X_n <- seq(-dom, dom, length.out = input$norm_x)
    med <- input$norm_med
    sd <- sqrt(input$norm_var)
    
    # Variabile - rep. uniforma
    X_u <- seq(0, input$unif_x)
    a <- input$unif_min
    b <- input$unif_max
    
    # Variabile - rep. exponentiala
    X_exp <- seq(0.001,input$exp_x,0.01)
    lambda <- input$exp_lambda
    
    # Variabile - rep. Bernoulli
    p <- input$bern_prob
    
    # Variabile - rep. binomiala
    X_binom <- 0:input$binom_x
    n <- input$binom_n
    p <- input$binom_prob
    
    switch(input$rep,
           "Normală" = {
              plot(X_n,dnorm(X_n,med,sd),xlab="X",ylab="Probabilitate")
              grid(nx=20,ny=20)
              lines(X_n,dnorm(X_n,med,sd),col="blue",lwd=2)
            },
            "Uniformă" = {
              plot(X_u,dunif(X_u,a,b),xlab="X",ylab="Probabilitate")
              grid(nx=20,ny=20)
              lines(0:a-1,dunif(0:a-1,a,b),col="blue",lwd=2)
              lines(a:b,dunif(a:b,a,b),col="blue",lwd=2)
              lines(b+1:max(X_u),dunif(b+1:max(X_u),a,b),col="blue",lwd=2)
            },
           "Exponentială" = {
             plot(X_exp,dexp(X_exp,lambda),xlab="X",ylab="Probabilitate",type="c")
             grid(nx=20,ny=20)
             lines(X_exp,dexp(X_exp,lambda),col="blue",lwd=2)
           },
           "Bernoulli" = {
             plot(c(0,1),dbern(c(0,1),p),xlab="X",ylab="Probabilitate",type="h",col="blue")
             grid(nx=20,ny=20)
           },
           "Binomială" = {
             plot(X_binom,dbinom(X_binom,n,p),xlab="X",ylab="Probabilitate",col="blue")
             grid(nx=20,ny=20)
           })
  })
  
  # Functii de repartitie
  output$frep <- renderPlot({
    # Variabile - rep. normala
    dom <- input$norm_dom / 2
    X_n <- seq(-dom, dom, length.out = input$norm_x)
    med <- input$norm_med
    sd <- sqrt(input$norm_var)
    
    # Variabile - rep. uniforma
    X_u <- seq(0, input$unif_x)
    a <- input$unif_min
    b <- input$unif_max
    
    # Variabile - rep. exponentiala
    X_exp <- seq(0.001,input$exp_x,0.01)
    lambda <- input$exp_lambda
    
    # Variabile - rep. Bernoulli
    p <- input$bern_prob
    
    # Variabile - rep. binomiala
    X_binom <- 0:input$binom_x
    n <- input$binom_n
    p <- input$binom_prob
    
    switch(input$rep,
           "Normală" = {
             plot(X_n,pnorm(X_n,med,sd),xlab="x",ylab="Probabilitate")
             grid(nx=20,ny=20)
             lines(X_n,pnorm(X_n,med,sd),col="red",lwd=2)
           },
           "Uniformă" = {
             plot(X_u,punif(X_u,a,b),xlab="X",ylab="Probabilitate")
             grid(nx=20,ny=20)
             lines(X_u,punif(X_u,a,b),col="red",lwd=2)
           },
           "Exponentială" = {
             plot(X_exp,pexp(X_exp,lambda),xlab="X",ylab="Probabilitate",type="c")
             grid(nx=20,ny=20)
             lines(X_exp,pexp(X_exp,lambda),col="red",lwd=2)
           },
           "Bernoulli" = {
             plot(c(0,1),pbern(c(0,1),p),xlab="X",ylab="Probabilitate",type="h",col="red")
             grid(nx=20,ny=20)
           },
           "Binomială" = {
             plot(X_binom,pbinom(X_binom,n,p),col="red",xlab="X",ylab="Probabilitate")
             grid(nx=20,ny=20)
           })
  })
}

shinyApp(ui, server)