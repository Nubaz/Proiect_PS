library(shiny)
library(shinydashboard)

eval(parse("cer_1.R", encoding="UTF-8"))
eval(parse("cer_5.R", encoding="UTF-8"))
eval(parse("cer_7.R", encoding="UTF-8"))
eval(parse("cer_11.R", encoding="UTF-8"))
eval(parse("cer_12.R", encoding="UTF-8"))

ui <- dashboardPage(
  dashboardHeader(title = "Repartiții de v.a."),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Meniu cu repartiții de v.a.", tabName = "cer1"),
      menuItem("Afișarea unei v.a. discrete", tabName = "cer5"),
      menuItem("Transformarea unei v.a. discrete", tabName = "cer7"),
      menuItem("Diagrama boxplot, histogramă", tabName = "cer11"),
      menuItem("Operații cu v.a. discrete", tabName = "cer12")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "cer1",
          cer1_ui(1)
      ),
      tabItem(tabName = "cer5",
          cer5_ui(5)
      ),
      tabItem(tabName = "cer7",
          cer7_ui(7)
      ),
      tabItem(tabName = "cer11",
          cer11_ui(11)
      ),
      tabItem(tabName = "cer12",
          cer12_ui(12)
      )
    )
  )
)

server <- function(input, output) {
  cer1_server(1)
  cer5_server(5)
  cer7_server(7)
  cer11_server(11)
  cer12_server(12)
}

shinyApp(ui, server)
