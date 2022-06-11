library(shinyjs)
library(Rlab)

cer1_ui <- function(id) {
  ns <- NS(id) # Desemnarea namespace-ului
  
  tagList(
    titlePanel("Repartiții de variabile aleatoare"),
    
    sidebarLayout(
      sidebarPanel(
        # Selectie de repartitii
        selectInput(ns("rep"), "Repartiție:",
                    c("Normală",
                      "Uniformă",
                      "Exponentială",
                      "Gamma",
                      "Bernoulli",
                      "Binomială",
                      "Geometrică",
                      "Poisson")
        ),
        
        # Rep. normala
        conditionalPanel(
          ns = ns,
          
          # Pentru resetarea la valorile initiale
          useShinyjs(),
          id = ns("val_n"),
          
          condition = "input.rep == 'Normală'",
          numericInput(ns("norm_dom"), "Lungime interval:", min = 4, value = 10),
          sliderInput(ns("norm_x"), "Număr valori:", min = 3, max = 50, value = 25),
          numericInput(ns("norm_med"), "Medie: ", step = 0.1, value = 0),
          numericInput(ns("norm_var"), "Dispersie:", min = 0.1, step = 0.1, value = 1)
        ),
        
        # Rep. uniforma
        conditionalPanel(
          ns = ns,
          
          # Pentru resetarea la valorile initiale
          useShinyjs(),
          id = ns("val_u"),
          
          condition = "input.rep == 'Uniformă'",
          sliderInput(ns("unif_x"), "Număr valori:", min = 4, max = 50, value = 25),
          numericInput(ns("unif_min"), "Valoare minimă (a):", min = 0, max = 49, value = 5),
          numericInput(ns("unif_max"), "Valoare maximă (b):", min = 1, max = 48, value = 15)
        ),
        
        # Rep. exponentiala
        conditionalPanel(
          ns = ns,
          
          # Pentru resetarea la valorile initiale
          useShinyjs(),
          id = ns("val_ex"),
          
          condition = "input.rep == 'Exponentială'",
          sliderInput(ns("exp_x"), "Număr valori:", min = 2, max = 10, value = 5),
          numericInput(ns("exp_lambda"), "Lambda:", min = 0.1, value = 1, step = 0.1)
        ),
        
        # Rep. Gamma
        conditionalPanel(
          ns = ns,
          
          # Pentru resetarea la valorile initiale
          useShinyjs(),
          id = ns("val_gamma"),
          
          condition = "input.rep == 'Gamma'",
          sliderInput(ns("gamma_x"), "Număr valori:", min = 4, max = 100, value = 20),
          numericInput(ns("gamma_k"), "k:", min = 1, value = 2),
          numericInput(ns("gamma_t"), "teta:", min = 1, value = 2)
        ),
        
        # Rep. Bernoulli
        conditionalPanel(
          ns = ns,
          
          # Pentru resetarea la valorile initiale
          useShinyjs(),
          id = ns("val_bern"),
          
          condition = "input.rep == 'Bernoulli'",
          numericInput(ns("bern_prob"), "Probabilitate:", min = 0.1, max = 0.9, value = 0.2, step = 0.1)
        ),
        
        # Rep. binomiala
        conditionalPanel(
          ns = ns,
          
          # Pentru resetarea la valorile initiale
          useShinyjs(),
          id = ns("val_binom"),
          
          condition = "input.rep == 'Binomială'",
          sliderInput(ns("binom_x"), "Număr valori:", min = 2, max = 100, value = 50),
          numericInput(ns("binom_n"), "Nr. experimente:", min = 0, max = 100, value = 100, step = 10),
          numericInput(ns("binom_prob"), "Probabilitate:", min = 0.1, max = 0.9, value = 0.2, step = 0.1)
        ),
        
        # Rep. geometrică
        conditionalPanel(
          ns = ns,
          
          # Pentru resetarea la valorile initiale
          useShinyjs(),
          id = ns("val_geom"),
          
          condition = "input.rep == 'Geometrică'",
          sliderInput(ns("geom_x"), "Număr evenimente:", min = 1, max = 10, value = 5),
          numericInput(ns("geom_p"), "Prob. succesului", min = 0.1, max = 0.9, value = 0.4, step = 0.1)
        ),
        
        # Rep. Poisson
        conditionalPanel(
          ns = ns,
          
          # Pentru resetarea la valorile initiale
          useShinyjs(),
          id = ns("val_pois"),
          
          condition = "input.rep == 'Poisson'",
          sliderInput(ns("pois_x"), "Număr evenimente", min = 3, max = 50, value = 25),
          numericInput(ns("pois_lambda"), "Lambda", min = 1, max = 50, value = 4)
        ),
        
        htmlOutput(ns("data")),
        h3(),
        
        actionButton(ns("rst"), "Valori inițiale")
      ),
      
      # Ploturi
      mainPanel(
        h3("Funcții caracteristice"),
        
        tabsetPanel(
          tabPanel("Densitate/masă", plotOutput(ns("fdensmasa"))),
          tabPanel("Repartiție", plotOutput(ns("frep")))
        )
      )
    ),
    
    mainPanel(
      htmlOutput(ns("text")),
      width = "auto"
    )
  )
}

cer1_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    repartitie <- reactive(input$rep)
    rest <- reactive(input$rst)
    
    # Resetarea la valorile initiale
    observeEvent(rest(), {
      shinyjs::reset("val_n")
      shinyjs::reset("val_u")
      shinyjs::reset("val_ex")
      shinyjs::reset("val_gamma")
      shinyjs::reset("val_bern")
      shinyjs::reset("val_binom")
      shinyjs::reset("val_geom")
      shinyjs::reset("val_pois")
    })
    
    # Elemente caracteristice repartitiilor
    output$data <- 
        renderUI({
          switch(repartitie(),
                 "Normală" = {
                   HTML(paste("<b>Mediana:</b>", input$norm_med, "<br>",
                              "<b>Val. modală:</b>:", input$norm_med 
                   ))
                 },
                 "Uniformă" = {
                   medie <- 1/2 * (input$unif_min + input$unif_max)
                   disp <- round(1/12 * (input$unif_max - input$unif_min)^2,2)
                   
                   HTML(paste("<b>Medie:</b>", medie, "<br>",
                              "<b>Dispersie:</b>", disp, "<br>",
                              "<b>Mediana:</b>", medie, "<br>",
                              "<b>Val. modală:</b>: &#8712 (a,b)" 
                   ))
                 },
                 "Exponentială" = {
                   medie <- round(1/input$exp_lambda,2)
                   mediana <- round(log(2)/input$exp_lambda,2)
                   disp <- round(1/(input$exp_lambda^2),2)
                   
                   HTML(paste("<b>Medie:</b>", medie, "<br>",
                              "<b>Dispersie:</b>", disp, "<br>",
                              "<b>Mediana:</b>", mediana, "<br>",
                              "<b>Val. modală:</b>: 0" 
                   ))
                 },
                 "Gamma" = {
                   medie <- input$gamma_k * input$gamma_t
                   disp <- medie * input$gamma_t
                   
                   if(input$gamma_k >= 1) {
                     val_mod <- (input$gamma_k-1) * input$gamma_t
                   } else {
                     val_mod <- 0
                   }
                   
                   HTML(paste("<b>Medie:</b>", medie, "<br>",
                              "<b>Dispersie:</b>", disp, "<br>",
                              "<b>Mediana:</b> fără expresie formală", "<br>",
                              "<b>Val. modală:</b>:", val_mod 
                   ))
                 },
                 "Bernoulli" = {
                   medie <- input$bern_prob
                   
                   if(input$bern_prob < 1/2) {
                     mediana <- 0
                   } else if(input$bern_prob > 1/2) {
                     mediana <- 1
                   } else {
                     mediana <- "&#8712 [0,1]"
                   }
                   
                   disp <- input$bern_prob * (1-input$bern_prob)
                   
                   if(input$bern_prob < 1/2) {
                     val_mod <- 0
                   } else if(input$bern_prob > 1/2) {
                     val_mod <- 1
                   } else {
                     val_mod <- 0.1
                   }
                   
                   HTML(paste("<b>Medie:</b>", medie, "<br>",
                              "<b>Dispersie:</b>", disp, "<br>",
                              "<b>Mediana:</b>", mediana, "<br>",
                              "<b>Val. modală:</b>:", val_mod 
                   ))
                 },
                 "Binomială" = {
                   medie <- input$binom_n * input$binom_prob
                   val_mod <- floor((input$binom_n+1) * input$binom_prob)
                   disp <- medie * (1-input$binom_prob)
                   
                   HTML(paste("<b>Medie:</b>", medie, "<br>",
                              "<b>Dispersie:</b>", disp, "<br>",
                              "<b>Mediana:</b> fără expresie formală", "<br>",
                              "<b>Val. modală:</b>:", val_mod 
                   ))
                 },
                 "Geometrică" = {
                   medie <- round((1-input$geom_p) / input$geom_p,2)
                   median <- ceiling(-1/log(1-input$geom_p,2)) - 1
                   disp <- round(medie / input$geom_p,2)
                   
                   HTML(paste("<b>Medie:</b>", medie, "<br>",
                              "<b>Dispersie:</b>", disp, "<br>",
                              "<b>Mediana:</b>", median, "<br>",
                              "<b>Val. modală:</b>: 0" 
                   ))
                 },
                 "Poisson" = {
                   medie <- input$pois_lambda
                   median <- floor(input$pois_lambda + 1/3 - 0.02/input$pois_lambda)
                   disp <- input$pois_lambda
                   
                   HTML(paste("<b>Medie:</b>", medie, "<br>",
                              "<b>Dispersie:</b>", disp, "<br>",
                              "<b>Mediana:</b>", median, "<br>",
                              "<b>Val. modale:</b>:", ceiling(input$pois_lambda)-1, ",", floor(input$pois_lambda) 
                   ))
                 }
          )
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
      
      # Variabile - rep. gamma
      X_gamma <- seq(0.001,input$gamma_x,0.01)
      k <- input$gamma_k
      t <- input$gamma_t
      
      # Variabile - rep. Bernoulli
      p_b <- input$bern_prob
      
      # Variabile - rep. binomiala
      X_binom <- 0:input$binom_x
      n <- input$binom_n
      p <- input$binom_prob
      
      # Variabile - rep. geom
      X_g <- 0:input$geom_x
      p_g <- input$geom_p
      
      # Variabile - rep. Poisson
      X_pois <- 0:input$pois_x
      pois_lambda <- input$pois_lambda
      
      switch(repartitie(),
             "Normală" = {
               plot(X_n,dnorm(X_n,med,sd),xlab="X",ylab="f(x)")
               grid(nx=20,ny=20)
               lines(X_n,dnorm(X_n,med,sd),col="blue",lwd=2)
             },
             "Uniformă" = {
               plot(X_u,dunif(X_u,a,b),xlab="X",ylab="f(x)")
               grid(nx=20,ny=20)
               lines(0:a-1,dunif(0:a-1,a,b),col="blue",lwd=2)
               lines(a:b,dunif(a:b,a,b),col="blue",lwd=2)
               lines(b+1:max(X_u),dunif(b+1:max(X_u),a,b),col="blue",lwd=2)
             },
             "Exponentială" = {
               plot(X_exp,dexp(X_exp,lambda),xlab="X",ylab="f(x)",type="c")
               grid(nx=20,ny=20)
               lines(X_exp,dexp(X_exp,lambda),col="blue",lwd=2)
             },
             "Gamma" = {
               plot(X_gamma,dgamma(X_gamma,k,scale=t),xlab="X",ylab="f(x)",type="c")
               grid(nx=20,ny=20)
               lines(X_gamma,dgamma(X_gamma,k,scale=t),col="blue",lwd=2)
             },
             "Bernoulli" = {
               plot(c(0,1),dbern(c(0,1),p_b),xlab="X",ylab="Pr(X=k)",type="h",col="blue",lwd=2)
               grid(nx=20,ny=20)
             },
             "Binomială" = {
               plot(X_binom,dbinom(X_binom,n,p),xlab="X",ylab="Pr(X=k)",col="blue")
               grid(nx=20,ny=20)
             },
             "Geometrică"= {
               plot(X_g,dgeom(X_g,p_g),xlab="X",ylab="Pr(X=k+1)",type="h",col="blue",lwd=2)
               grid(nx=20,ny=20)
             },
             "Poisson" = {
               plot(X_pois,dpois(X_pois,pois_lambda),xlab="X",ylab="Pr(X=k)",type="h",col="blue",lwd=2)
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
      
      # Variabile - rep. gamma
      X_gamma <- seq(0.001,input$gamma_x,0.01)
      k <- input$gamma_k
      t <- input$gamma_t
      
      # Variabile - rep. Bernoulli
      p_b <- input$bern_prob
      
      # Variabile - rep. binomiala
      X_binom <- 0:input$binom_x
      n <- input$binom_n
      p <- input$binom_prob
      
      # Variabile - rep. geom
      X_g <- 0:input$geom_x
      p_g <- input$geom_p
      
      # Variabile - rep. Poisson
      X_pois <- 0:input$pois_x
      pois_lambda <- input$pois_lambda
      
      switch(repartitie(),
             "Normală" = {
               plot(X_n,pnorm(X_n,med,sd),xlab="x",ylab="F(x)")
               grid(nx=20,ny=20)
               lines(X_n,pnorm(X_n,med,sd),col="red",lwd=2)
             },
             "Uniformă" = {
               plot(X_u,punif(X_u,a,b),xlab="X",ylab="F(x)")
               grid(nx=20,ny=20)
               lines(X_u,punif(X_u,a,b),col="red",lwd=2)
             },
             "Exponentială" = {
               plot(X_exp,pexp(X_exp,lambda),xlab="X",ylab="F(x)",type="c")
               grid(nx=20,ny=20)
               lines(X_exp,pexp(X_exp,lambda),col="red",lwd=2)
             },
             "Gamma" = {
               plot(X_gamma,pgamma(X_gamma,k,scale=t),xlab="X",ylab="F(x)",type="c")
               grid(nx=20,ny=20)
               lines(X_gamma,pgamma(X_gamma,k,scale=t),col="red",lwd=2)
             },
             "Bernoulli" = {
               plot(c(0,1),pbern(c(0,1),p_b),xlab="X",ylab="Pr(X<=k)",type="h",col="red",lwd=2)
               grid(nx=20,ny=20)
             },
             "Binomială" = {
               plot(X_binom,pbinom(X_binom,n,p),col="red",xlab="X",ylab="Pr(X<=k)")
               grid(nx=20,ny=20)
             },
             "Geometrică" = {
               plot(X_g,pgeom(X_g,p_g),col="red",xlab="X",ylab="Pr(X<=k)",type="h",lwd=2)
               grid(nx=20,ny=20)
             },
             "Poisson" = {
               plot(X_pois,ppois(X_pois,pois_lambda),col="red",xlab="X",ylab="Pr(X<=k)",type="h",lwd=2)
               grid(nx=20,ny=20)
             })
    })
    
    # Descrierile repartitiilor
    output$text <- renderUI({
      switch(repartitie(),
             "Normală" = {
               withMathJax(HTML(readLines("descrieri/normala.html", warn = F, encoding = 'UTF-8')))
             },
             "Uniformă" = {
               withMathJax(HTML(readLines("descrieri/uniforma.html", warn = F, encoding = 'UTF-8')))
             },
             "Exponentială" = {
               withMathJax(HTML(readLines("descrieri/exponentiala.html", warn = F, encoding = 'UTF-8')))
             },
             "Gamma" = {
               withMathJax(HTML(readLines("descrieri/gamma.html", warn = F, encoding = 'UTF-8')))
             },
             "Bernoulli" = {
               withMathJax(HTML(readLines("descrieri/bernoulli.html", warn = F, encoding = 'UTF-8')))
             },
             "Binomială" = {
               withMathJax(HTML(readLines("descrieri/binomiala.html", warn = F, encoding = 'UTF-8')))
             },
             "Geometrică" = {
               withMathJax(HTML(readLines("descrieri/geometrica.html", warn = F, encoding = 'UTF-8')))
             },
             "Poisson" = {
               withMathJax(HTML(readLines("descrieri/poisson.html", warn = F, encoding = 'UTF-8')))
             },
      )
    })
  })
}