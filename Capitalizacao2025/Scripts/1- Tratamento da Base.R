library(tidyverse)
library(arrow)

# Definindo diretório de trabalho na pasta raiz do script
dirname(rstudioapi::getSourceEditorContext()$path) |> 
  setwd()

# Importando base de dados
base <- read.csv("Persistencia_2025-05-26_dados_persistencia_capitalizacao.csv", header = TRUE,
                 sep = ",", stringsAsFactors = FALSE)

# Salvar a base de dados original
write_parquet(base, "../Bases/base_original.parquet")

##############################################################################################################
# Modificações - Usando dataInicioVigenciaCobertura e dataStatusItemContratado - 143.755 observações

# Lendo a base original em parquet
base <- read_parquet("../Bases/base_original.parquet")

# Criando a coluna dataConsulta, que é quando os dados foram retirados na base da MAG
base$dataConsulta <- as.Date("2025-05-26")

# Tratando os dados
base <- base |>
  # Criando a variável status já com rótulos ajustados
  mutate(
    status = case_when(
      grepl("(?i)remover", flag) ~ "Remover",
      flag %in% c("Ciclo de vida completo") ~ "Remover",
      flag %in% c("Ativo", "Censura") ~ "Censura",  # já ajusta os censurados aqui
      flag == "Cancelamento" ~ "Cancelamento",
      TRUE ~ flag)) |>
  
  # Filtrando os registros válidos
  filter(
    dataInicioVigenciaCobertura >= as.Date("2015-06-01"),
    dataStatusItemContratado >= as.Date("2020-06-01"),
    !(status %in% c("Remover", ""))) |>
  
  # Calculando tempos
  mutate(
    tempo = as.Date(dataStatusItemContratado) - as.Date(dataInicioVigenciaCobertura),
    tempo_mes = as.integer(tempo / 30),
    tempo_ano = as.integer(tempo / 365.25)) |>
  
  # Filtrando casos inválidos
  filter(
    tempo_mes > 0,
    capitalSeguradoPrevisto > 0,
    !(flag == "Ativo" & dataStatusItemContratado != dataConsulta),
    !(produtoNome == "PRIVATE SOLUTIONS WHOLE LIFE 2014" & prazoContribuicao < 0)) |>
  
  # Criando a variável delta
  mutate(delta = ifelse(status == "Censura", 0, 1))

# Lista de colunas a serem tratadas
colunas_para_tratar <- c("sexoCliente", "categoriaCliente")

base[colunas_para_tratar] <- lapply(base[colunas_para_tratar], function(coluna) {
  ifelse(is.na(coluna) | coluna == "", "NÃO INFORMADO", coluna)
})

# Salvar a base de dados modificada
write_parquet(base, "../Bases/base_modificada.parquet")