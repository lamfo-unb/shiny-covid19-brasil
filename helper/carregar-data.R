
data_ <- new.env()
local({
  URL_RESUMO <- 'https://raw.githubusercontent.com/NeurembergMatos/data/master/covid-brasil/resumo-estado.csv'
  URL_VARIACAO <- 'https://raw.githubusercontent.com/NeurembergMatos/data/master/covid-brasil/variacao-estado.csv'
  
  COL_CLASSES_RESUMO <- c(
    "date" = 'Date', "estado" = 'character', "regiao" = 'character',
    "variavel" = 'character', "quantidade" = 'integer'
  )
  
  COL_CLASSES_VARIACAO <- c(
    "estado" = 'character', "regiao" = 'character',  "variavel"  = 'character',
    "var_7d" = 'numeric', "var_3d" = 'numeric', "var_1d" = 'numeric',
    "valor_atual"  = 'integer'
  )
  
  resumo_estado <- read.table(
    file = URL_RESUMO, header = TRUE,
    sep = '\t', dec  = ',', encoding = 'UTF-8',
    colClasses = COL_CLASSES_RESUMO, stringsAsFactors = FALSE
  )
  
  variacao_estado <- read.table(
    file = URL_VARIACAO, header = TRUE,
    sep = '\t', dec = ',', encoding = 'UTF-8', colClasses = COL_CLASSES_VARIACAO
  )
  
}, envir = data_)

resumo_estado <- data_[['resumo_estado']]
variacao_estado <- data_[['variacao_estado']]
rm(data_)