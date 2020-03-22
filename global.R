library(ggplot2)
library(dplyr)

dados <- feather::read_feather('dados/tratado/resumo-estado.feather')
variacao_estado <- feather::read_feather('dados/tratado/variacao-estado.feather')

sumarizar_brasil <- function(variacao_estado){
  sumario <- variacao_estado %>% 
    group_by(variavel) %>% 
    summarise(casos = sum(valor_atual))
  sumario <- split(sumario, sumario[['variavel']])
  
  sumario <- lapply(sumario, function(x){as.numeric(x[1,2])})
  names(sumario) <- tolower(names(sumario))
  
  sumario <- lapply(sumario, format,  big.mark = '.', decimal.mark = ',')
  
  return(sumario)
}

sumario <- sumarizar_brasil(variacao_estado)


regioes <- unique(sort(dados$regiao))

filtrar_estado <- function(dados, estados){
  dados %>% 
    filter(estado %in% estados)
}

filtrar_regiao <- function(dados, regioes){
  dados %>% 
    filter(regiao %in% regioes)
}
  
filtrar_variavel <- function(dados, variaveis){
  dados %>% 
    filter(variavel %in% variaveis)
}

plotar <- function(dados){

 grafico <- dados %>%
    rename(UF = estado, Casos = quantidade, Data = date) %>%
    ggplot(aes(x = Data, y = Casos, color = UF))+
    geom_line()+
    facet_wrap(~variavel)+
    labs(
      x = NULL,
      y = NULL,
      color = NULL
    )
 plotly::ggplotly(grafico)
}

