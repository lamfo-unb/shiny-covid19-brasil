
shiny::shinyServer(function(input, output, session){
  
  df_regiao <- shiny::reactive({
    if(is.null(input$regiao))return(resumo_estado)
    filtrar_regiao(dados = resumo_estado, regioes = input$regiao)
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
  
  mod_param <- reactive({
    
    N <- 211289547 # População do Brasil
    
    # Valores populacao
    X = 1       # infeccioso (infectados)
    Y = 0                    # recuperados
    Z = 2                    # expostos (2 ?)
    W = N - (X + Z)          # suscetiveis
    
    taxa_contato <- input$tx_contato                
    prob_transmisssao <- input$prob_trans           
    periodo_infeccao <- input$periodo_infec         
    periodo_latencia <- input$periodo_laten         
    
    beta_value = taxa_contato * prob_transmisssao
    gamma_value = 1 / periodo_infeccao
    delta_value = 1 / periodo_latencia
    
    Ro = beta_value / gamma_value
    #Ro = 2.8
    
    par_list = c(beta = beta_value, gamma = gamma_value, delta = delta_value)
    init_values = c(S = W/N, E = X/N, I = Y/N, R = Z/N)
    
    return(list(par_list = par_list,
                valor_inicial = init_values))
  })
  
  mod_seir <- reactive({
    
    req(input$atualizar_mod)
    tempo = 1:100
    output = lsoda (mod_param()$valor_inicial, tempo, SEIR, mod_param()$par_list)
    
    output2 <- output %>%
      as.data.frame() %>%
      gather("Estado", "Valor", 2:5)
    
    return(list(output2 = output2))
    
  })
    
   output$plot_seir <- plotly::renderPlotly({
     req(input$atualizar_mod)
     plot_seir(dados = mod_seir()$output2)
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
      options = list(pageLenght = 10, autoWidth = TRUE, scrollX = TRUE, order = list(6,'desc'))
    )
    
    col_var <- c('Variação 7 dias', 'Variação 3 dias', 'Variação 1 dia')
    dt_ <- DT::formatRound(dt_, columns = col_var, digits = 3, dec.mark = ',', mark ='.')
    DT::formatRound(dt_, columns = 'Casos atuais', digits = 0, mark = '.')
  })
  
  output$ui_action <- shiny::renderUI(
    shiny::actionButton('atualizar', 'Visualizar', icon = icon('play-circle'), width = '200px')
  )
  
  output$action_mod <- shiny::renderUI(
    shiny::actionButton('atualizar_mod', 'Plotar SEIR', icon = icon('play-circle'), width = '200px')
  )
  
})