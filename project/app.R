library(shiny)
library(shinydashboard)

source("cer_1.R")

ui <- dashboardPage(
  dashboardHeader(title = "Repartiții de v.a."),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Cerinta 1", tabName = "cer1")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "cer1",
          cer1_ui(1)  
      )
    )
  )
)

server <- function(input, output, session) {
  cer1_server(1)
}

shinyApp(ui, server)
