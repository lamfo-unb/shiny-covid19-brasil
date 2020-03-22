
library(dplyr)
library(ggplot2)

dados <- data.table::fread('dados/bruto/brazil_covid19.csv', encoding = 'UTF-8',
                           data.table = FALSE)
estados_bras <- data.table::fread('dados/bruto/estados-brasileiros.csv',
                                  data.table = FALSE
)

as_numeric <- function(string){
  string <- stringr::str_remove(string, ',') %>% 
    stringr::str_remove('\\.')
  as.numeric(string)
}


dados <- dplyr::left_join(
  dados,
  estados_bras[,c('estado', 'uf', 'regiao', 'cod_ibge')],
  by = c('state' = 'estado')
)

dados <- dados %>% 
  mutate(
    date = as.Date(date),
    suspects = as_numeric(suspects),
    refuses = as_numeric(refuses),
    cases = as_numeric(cases),
    deaths = as_numeric(deaths)
  )

resumo_estado <- dados %>% 
  group_by(date, state, regiao) %>% 
  summarise(
    suspeitos = sum(suspects),
    descartados = sum(refuses),
    casos = sum(cases),
    mortes = sum(deaths)
  ) %>%
  ungroup() %>% 
  rename(estado = state)

resumo_estado <- resumo_estado %>% 
  tidyr::pivot_longer(
    cols = suspeitos:mortes,
    names_to = 'variavel',
    values_to = 'quantidade'
    )

resumo_estado$variavel <- factor(
  x =resumo_estado$variavel,
  levels = c("suspeitos","descartados","casos","mortes"),
  labels = c('Suspeitos', 'Descartados', 'Confirmados', 'Mortes')
  )

resumo_estado$variavel <- as.character(resumo_estado$variavel)

calcular_variacao <- function(dados, estado_, var_){
  
  df <- dados %>% 
    filter(estado == estado_) %>% 
    filter(variavel == var_)
  
  n <-  nrow(df)
  n_1 <- n-1
  n_3 <- n-3
  n_7 <- n-7
  
  valor_lag <- function(df, lag_){
   out <-  tryCatch(
      expr = as.numeric(df[lag_, 'quantidade']),
      error = function(cond){return(NA)}
      )
   
   return(out)
  }
  
  variacao <- data.frame(
    'estado' = estado_,
    'variavel' = var_,
    'valor_7d' = valor_lag(df, n_7),
    'valor_3d' = valor_lag(df, n_3),
    'valor_1d' = valor_lag(df, n_1),
    'valor_atual' = valor_lag(df, n),
    stringsAsFactors = FALSE
  )
  return(variacao)
}

vars_ <- function(dados, estado){
  
  variaveis <- unique(dados[['variavel']])
  expand_ <- expand.grid(estado, variaveis, stringsAsFactors = FALSE)
  
  estado_ <- expand_[['Var1']]
  variaveis_ <- expand_[['Var2']]
  
  vari <- mapply(FUN = calcular_variacao, estado_ = estado_, var_ = variaveis,
                 MoreArgs = list(dados = dados), SIMPLIFY = FALSE)
  
  vari <- do.call(base::rbind, vari)
  row.names(vari) <- NULL
  
  return(vari)
}


historico_estado <-  vars_(
  dados = resumo_estado, 
  estado = unique(resumo_estado[['estado']])
)


validar_type <- function(valor){
  if(any(is.na(valor), is.nan(valor), is.infinite(valor))) valor <- NA
  return(valor)
}

variacao_estado <- historico_estado %>% 
  mutate(
    var_7d = (valor_atual/valor_7d)-1,
    var_3d = (valor_atual/valor_3d)-1,
    var_1d = (valor_atual/valor_1d)-1
  ) %>% 
  select(
    -valor_7d, -valor_3d, -valor_1d
  )

variacao_estado <- variacao_estado %>% 
  mutate(var_7d = sapply(var_7d, validar_type),
         var_3d = sapply(var_3d, validar_type),
         var_1d = sapply(var_1d, validar_type)
         )

variacao_estado <- dplyr::left_join(
  variacao_estado,
  estados_bras[,c('estado', 'regiao')],
  by = 'estado'
  )
colunas <- c("estado", "regiao","variavel", "var_7d", "var_3d", "var_1d", "valor_atual")
variacao_estado <- variacao_estado[, colunas]

feather::write_feather(resumo_estado, 'dados/tratado/resumo-estado.feather')
feather::write_feather(historico_estado, 'dados/tratado/historico-estilizado-estado.feather')
feather::write_feather(variacao_estado, 'dados/tratado/variacao-estado.feather')
