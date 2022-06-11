library(shiny)
library(shinydashboard)

eval(parse("cer_1.R", encoding="UTF-8"))

ui <- dashboardPage(
  dashboardHeader(title = "Repartiții de v.a."),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Meniu cu repartiții de v.a.", tabName = "cer1")
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
