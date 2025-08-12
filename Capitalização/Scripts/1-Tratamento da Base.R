#categoriaCliente vazia incluída: 1701 observações

library(tidyverse)

# Definindo diretório de trabalho na pasta raiz do script ####
dirname(rstudioapi::getSourceEditorContext()$path) |> 
  setwd()

# Importando base de dados ####
base <- read_rds('../Bases/capitalizacao.rds')

# Tratando dados ####
base <- base |> 
  mutate(status = case_when(
    statusItemContratado  %in% c("REMIDO - D02",
                                 "ATIVA EM DIA - A00",
                                 "ATIVA 2 ATRASOS - A02",
                                 "ATIVA 1 ATRASO - A01",
                                 "ATIVA 3 ATRASOS - A03",
                                 "ATIVA 4 ATRASOS - A04",
                                 "ATIVA 5 ATRASOS - A05") ~ "Ativo",
    statusItemContratado %in% c("INADIMPLÊNCIA - C01",
                                "DESISTÊNCIA - C05",
                                "PROVISÃO PORTADA - C19",
                                "CANCELADO PELA REGULAÇÃO - C22",
                                "RESGATE - B02") ~ "Resgate",
    statusItemContratado %in% c("PECÚLIO - B03",
                                "INVALIDEZ (PAGAMENTO ÚNICO IPD) - B09",
                                "AUXÍLIO FUNERAL - B07",
                                "LIQUIDAÇÃO ESPECIAL BENEFÍCIO - B05","APOSENTADORIA/INVALIDEZ/SOBREVIVÊNCIA (RENDA) - B06",
                                "PRESCRIÇÃO BENEFÍCIO - C11",
                                "BENEFÍCIO NEGADO - C10",
                                "CANCELADO POR DEPENDÊNCIA - C21",
                                "ÓBITO - C03", "SOBREVIVÊNCIA - B08",
                                "PRAZO CUMPRIDO - C06") ~ "Censura",
    statusItemContratado %in% c("CONTRATO ANULADO - C16",
                                "ENCERRAMENTO APÓLICE - C13",
                                "TRANSFERÊNCIA - C15") ~ "Remover",
    statusItemContratado == "SALDAMENTO - D00" ~ "Saldamento",
    TRUE ~ statusItemContratado)) |> 
  filter(as.Date(dataInicioVigenciaCobertura) >= as.Date("2010-01-01"),
         !(status %in% c("Sem status", "Remover", ""))) |> 
  mutate(tempo = case_when(status == "Ativo" ~ 
                             as.Date(dataConsulta) - 
                             as.Date(dataInicioVigenciaCobertura),
                           status != "Ativo" ~ 
                             as.Date(dataStatusItemContratado) -
                             as.Date(dataInicioVigenciaCobertura)),
         status = case_when(status %in% c("Censura", "Ativo") ~ "Censura",
                            TRUE ~ status),
         tempo_mes = as.integer(tempo / 30),
         tempo_ano = as.integer(tempo / 365.25)) |> 
  filter(tempo_mes > 0) |> 
  mutate(delta = case_when(status == "Censura" ~ 0, 
                           TRUE ~ 1),
         delta1 = case_when(status == "Resgate" ~ 1, 
                            TRUE ~ 0),
         delta2 = case_when(status == "Saldamento" ~ 1, 
                            TRUE ~ 0)) |>
  filter(status != "PAGAMENTO ÚNICO - D01")

base$status <- factor(base$status, 
                      levels = c("Censura", "Saldamento", "Resgate"))

base$istate = rep("entry", nrow(base))

write_rds(base, '../Bases/capitalizacao_tratada.rds',
          compress = 'xz')