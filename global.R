library(ggplot2)
library(dplyr)
library(deSolve)
library(tidyr)

if(!all(sapply(c('resumo_estado', 'variacao_estado'), exists))){
  print('Carregando dados..')
  source('helper/carregar-data.R', encoding = 'UTF-8')
}

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

date_update <- tail(resumo_estado[['date']], 1)
sumario <- sumarizar_brasil(variacao_estado)
regioes <- unique(sort(resumo_estado$regiao))

##### SEIR 

data_br <- resumo_estado %>%
           group_by(date, variavel) %>%
           summarise(quantidade = sum(quantidade))

# Infected <- data_br %>% 
#             filter(variavel == "Confirmados") %>%
#             .$quantidade
#             
# Day <- 1:(length(Infected))
N <- 211289547 # População do Brasil

# Valores populacao

X = 1       # infeccioso (infectados)
Y = 0                    # recuperados
Z = 2                    # expostos (2 ?)
W = N - (X + Z)          # suscetiveis

SEIR = function (current_timepoint, state_values, parameters){ 
  # criando variaveis de estado (local variables)
  S = state_values[1]        # suscetiveis
  E = state_values[2]        # expostos
  I = state_values[3]        # infecciosos
  R = state_values[4]        # recuperados
  
  with ( 
    as.list (parameters),     # parameters recebe variaveis nomeadas 
    {
      # calcula derivadas
      dS = (-beta * S * I)
      dE = (beta * S * I) - (delta * E)
      dI = (delta * E) - (gamma * I)
      dR = (gamma * I)
      
      # combine results
      results = c (dS, dE, dI, dR)
      list (results)
    }
  )
}

taxa_contato = 10                # numero medio de contatos por dia
prob_transmisssao = 0.17               # probabilidade de transmissao
periodo_infeccao = 12                  # infectious period
periodo_latencia = 6                     # periodo de latencia (incubacao)

beta_value = taxa_contato * prob_transmisssao
gamma_value = 1 / periodo_infeccao
delta_value = 1 / periodo_latencia

Ro = beta_value / gamma_value
#Ro = 2.8



par_list = c (beta = beta_value, gamma = gamma_value, delta = delta_value)
init_values = c (S = W/N, E = X/N, I = Y/N, R = Z/N)

tempo = 1:100
output = lsoda (init_values, tempo, SEIR, par_list)

output2 = output %>%
          as.data.frame() %>%
          gather("Estado", "Valor", 2:5)

plot_seir <- function(dados){
  
  grafico <- ggplot(data = dados) +
    geom_line(aes(x = time, y = Valor, color = Estado))  
  
  plotly::ggplotly(grafico)
}

