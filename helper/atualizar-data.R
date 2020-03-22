

parametros <- new.env()
local({
  url_repositorio <- 'https://github.com/NeurembergMatos/data/tree/master/covid-brasil'
  
  url_historico <- paste(
    url_repositorio,
    'historico-estilizado-estado.feather?raw=true', sep = '/'
  )
  url_resumo <- paste(url_repositorio, 'resumo-estado.rds?raw=true', sep = '/')
  url_variacao <- paste(
    url_repositorio, 'variacao-estado.rds?raw=true', sep = '/'
  )
  
  url_status <- 'https://raw.githubusercontent.com/NeurembergMatos/data/master/covid-brasil/atualizacao.txt'
  status_local <- read.csv('dados/atualizacao.txt', header = FALSE, stringsAsFactors = FALSE)
  status_repo <- read.csv(url_status, header = FALSE, stringsAsFactors = FALSE)
  n_status_local <- nrow(status_local)
  n_status_repo <- nrow(status_repo)
  
  
  status <- list(
    local = read.csv('dados/atualizacao.txt', stringsAsFactors = FALSE),
    remoto = read.csv(url_status, stringsAsFactors = FALSE)
  )
  
  files_remotes <- list(
    dest_files = c('dados/tratado/resumo-estado.rds', 'dados/tratado/variacao-estado.rds'),
    urls = c(url_resumo, url_variacao)
  )
}, envir = parametros)

historico <- parametros$status
files_remotos <- parametros$files_remotes

is_remote_historico_update <- function(his_local, his_remoto){
  remoto <- his_remoto[['historico']]
  local <- his_local[['historico']]
  
  tail(remoto, 1) != tail(local, 1)
}

update_historico_local <- function(his_remoto){
  write.csv(x = his_remoto, 'dados/atualizacao.txt', row.names = FALSE)
}

atualizar_data <- function(files_remotos, status){
  if(!status) return(FALSE)
  
  w <- function(url, dest_file){
    download.file(url = url, destfile = dest_file, mode = 'wb')
  }
  
  mapply(w, url = files_remotos[['urls']], dest_file = files_remotos[['dest_files']])
  return(TRUE)
}

atualizar_historico_local <- function(his_remoto, status){
  if(!status) return(FALSE)
  write.csv2(x = his_remoto, file = 'dados/atualizacao.txt', row.names = FALSE)
  return(TRUE)
}

status <- is_remote_historico_update(historico$local, historico$remoto)
atualizar_data(files_remotos, status)
atualizar_historico_local(historico$remoto, status)


