library(shinydashboard)
library(shinycssloaders)

header <- dashboardHeader(
  title = 'Coronavirus no Brasil',
  tags$li(a(href = 'http://lamfo.unb.br/index.php?lang=pt-br', target = "_blank",
            img(src = 'lamfo-logo.png',
                title = "Lamfo-UnB", height = "40px"),
            style = "padding-top:5px; padding-bottom:5px;"),
          class = "dropdown")
)

fonte_dados = tags$div(
  style='text-align:center;padding-top:25px;',
  tags$p(tags$a(
    href = 'https://coronavirus.saude.gov.br/',
    target = "_blank", 'Fonte: Ministério da Saúde',br(),
    paste('Atualização: ', date_update, sep = '')
  )
  )
)


sidebar <- dashboardSidebar(
  shiny::selectInput(inputId = 'regiao', label = 'Região', choices = regioes,
                     multiple = TRUE, selected = NULL),
  shiny::uiOutput('ui_estado'),
  shiny::uiOutput('ui_variavel'),
  shiny::uiOutput('ui_action'),
  shiny::numericInput("tx_contato", "Taxa de Contato", 10, min = 1, max = 100),
  shiny::numericInput("prob_trans", "Probabilidade de transmisssao", 0.17, min = 0, max = 1, step = 0.01),
  shiny::numericInput("periodo_infec", "Periodo de infeccao", 12, min = 1, max = 20),
  shiny::numericInput("periodo_laten", "Periodo de Incubacao", 6, min = 1, max = 20),
  shiny::uiOutput('action_mod'),
  fonte_dados

)

body <- dashboardBody(
  shiny::fluidRow(
    valueBox('Brasil: Casos confirmados', value = sumario$confirmados, icon = icon('user-md'), color = 'blue', width = 3),
    valueBox('Brasil: Casos suspeitos', value = sumario$suspeitos, width = 3, icon = icon('briefcase-medical'), color = 'light-blue'),
    valueBox('Brasil: Casos descartados', value = sumario$descartados,icon = icon('heart'), color = 'green', width = 3),
    valueBox('Brasil: Mortes', value = sumario$mortes, icon = icon('exclamation'), color = 'red',  width = 3)
    ),
  shiny::fluidRow(box(plotly::plotlyOutput('grafico'), width = 12)),
  shiny::fluidRow(box(plotly::plotlyOutput('plot_seir'), width = 12)),
  shiny::fluidRow(box(DT::dataTableOutput('variacao'), width = 12))
  )

dashboardPage(header, sidebar, body)
