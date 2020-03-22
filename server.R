
shiny::shinyServer(function(input, output, session){
  
  df_regiao <- shiny::reactive({
    if(is.null(input$regiao))return(dados)
    filtrar_regiao(dados = dados, regioes = input$regiao)
  })
  
  output$ui_estado <- shiny::renderUI({
    estados <- unique(sort(df_regiao()[['estado']]))
    shiny::selectInput(inputId = 'estado', label = 'Estado', choices = estados,
                       multiple = TRUE, selected = NULL)
  })
  
  df_estado <- shiny::reactive({
    if(is.null(input$estado)) return(df_regiao())
    filtrar_estado(df_regiao(), input$estado)
  })
  
  
  output$ui_variavel <- shiny::renderUI({
    variaveis <- unique(sort(df_estado()[['variavel']]))
    
    shiny::selectInput(inputId = 'variavel',
                       label = 'Tipo de caso',
                       choices = variaveis, multiple = TRUE,
                       selected = variaveis[1]
                       )
    
  })
    
  df_variavel <- shiny::reactive({
    if(is.null(input$variavel)) return(df_estado())
    filtrar_variavel(df_estado(), input$variavel)
  })
  
  
  df_plot <- shiny::reactive({
    
    shiny::req(input$atualizar)
    shiny::isolate(df_variavel())
    
  })
  
  output$grafico <- plotly::renderPlotly({
    plotar(dados = df_plot())
  })
  
  
  table_variacao <- shiny::reactive({
    
    if(is.null(input$regiao)){
      fr <- variacao_estado
    } else{
      fr <- filtrar_regiao(variacao_estado, input$regiao)
    }
    
    if(is.null(input$estado)){
      fe <- fr
    } else{
      fe <- filtrar_estado(fr, input$estado)
    }
    
    if(is.null(input$variavel)){
      fv <- fe
    } else{
      fv <- filtrar_variavel(fe, input$variavel)
    }
    
    names(fv) <- c('Estado', 'Região', 'Tipo', 'Variação 7 dias', 'Variação 3 dias',
                   'Variação 1 dia', 'Casos atuais')
    return(fv)
  })
  
  table_var <- shiny::reactive({
    
    shiny::req(input$atualizar)
    shiny::isolate(table_variacao())
  })
  
  
  output$variacao <- DT::renderDataTable({
    dt_ <- DT::datatable(
      table_var(),
      width = 12,
      filter = 'top',
      rownames = FALSE,
      options = list(pageLenght = 10, autoWidth = TRUE, scrollX = TRUE)
    )
    
    col_var <- c('Variação 7 dias', 'Variação 3 dias', 'Variação 1 dia')
    dt_ <- DT::formatRound(dt_, columns = col_var, digits = 3)
    DT::formatRound(dt_, columns = 'Casos atuais', digits = 0, mark = '.')
  })
  
  output$ui_action <- shiny::renderUI(
    shiny::actionButton('atualizar', 'Visualizar', icon = icon('play-circle'), width = '200px')
  )
  
})