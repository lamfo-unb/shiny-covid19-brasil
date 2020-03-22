library(shinydashboard)
library(shinycssloaders)

header <- dashboardHeader(
  title = 'Coronavirus no Brasil',
  tags$li(a(href = 'http://lamfo.unb.br/index.php?lang=pt-br',
            img(src = 'lamfo-logo.png',
                title = "Lamfo-UnB", height = "40px"),
            style = "padding-top:5px; padding-bottom:5px;"),
          class = "dropdown")
)

sidebar <- dashboardSidebar(
  shiny::selectInput(inputId = 'regiao', label = 'Região', choices = regioes,
                     multiple = TRUE, selected = NULL),
  shiny::uiOutput('ui_estado'),
  shiny::uiOutput('ui_variavel'),
  shiny::uiOutput('ui_action')
)

body <- dashboardBody(
  shiny::fluidRow(
    valueBox('Brasil: Casos confirmados', value = sumario$confirmados, icon = icon('exclamation'), width = 3),
    valueBox('Brasil: Casos suspeitos', value = sumario$suspeitos, width = 3, icon = icon('exclamation')),
    valueBox('Brasil: Casos descartados', value = sumario$descartados,icon = icon('exclamation'), width = 3),
    valueBox('Brasil: Mortes', value = sumario$mortes, icon = icon('exclamation'), width = 3)
    ),
  shiny::fluidRow(box(plotly::plotlyOutput('grafico'), width = 12)),
  shiny::fluidRow(box(DT::dataTableOutput('variacao'), width = 12))
  )

dashboardPage(header, sidebar, body)