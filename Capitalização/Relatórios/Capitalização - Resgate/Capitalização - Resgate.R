install.packages(c("tidyverse","data.table","survival","plotly","dplyr","ggpubr",
                   "flextable","lnmixsurv","ggsurvfit","ggridges","survminer","","arrow",
                   "bayesplot","posterior","knitr","DT","kableExtra","lares"), repos = "https://cloud.r-project.org")

library(tidyverse)
library(data.table)
library(survival)
library(plotly)
library(dplyr)
library(ggpubr)
library(flextable)
library(lnmixsurv)
library(ggsurvfit)
library(ggridges)
library(survminer)
library(arrow)
library(bayesplot)
library(posterior)
library(knitr)
library(DT)
library(kableExtra)
library(lares)


base <- read_rds('../Bases/capitalizacao_tratada.rds')

base$categoriaCliente[base$categoriaCliente == ""] <- "sem categoriaCliente"

p = ggplot(base) + theme_classic()

# Função
tx.emp <- function(t, s, cat) {
  seq_time <- 1:length(t)
  
  aux <- NULL
  
  for (i in 2:length(seq_time)) {
    aux <- c(aux, 
             (s[seq_time[i - 1]] - s[seq_time[i]]) /
               ((t[seq_time[i]] - t[seq_time[i - 1]]) * s[seq_time[i - 1]]))
  }
  
  return(bind_rows(tibble(time = 0, hazard = 0, strata = cat),
                   tibble(time = t[-1], hazard = aux,
                          strata = cat)))
}



# Análise Covariáveis {.tabset .tabset-fade .tabset-pills}

## Sexo

## Transformando '' em "NÃO INFORMADO"


base <- base %>%
  mutate(sexoCliente = if_else(sexoCliente == "", "NÃO INFORMADO", sexoCliente))


base_resgate <- base |> filter(status == "Resgate")

(p + geom_bar(aes(x = sexoCliente, fill = sexoCliente), data = base_resgate)) |>
  ggplotly()

# Tabela de frequências (% sobre total de resgates por categoria)
table <- table(base_resgate$sexoCliente) |> as.data.frame()
colnames(table) <- c("Categoria", "Freq")

table$Perc <- round((table$Freq / sum(table$Freq)) * 100, 2)

table |>
  flextable() |>
  width(width = 1.3) |>
  bold(part = "header")


## Categoria

(p + geom_bar(aes(x = categoriaCliente, fill = categoriaCliente), data = base_resgate)) |>
  ggplotly()

# Tabela de frequências (% sobre total de resgates por categoria)
table <- table(base_resgate$categoriaCliente) |> as.data.frame()
colnames(table) <- c("Categoria", "Freq")

table$Perc <- round((table$Freq / sum(table$Freq)) * 100, 2)

table |>
  flextable() |>
  width(width = 1.3) |>
  bold(part = "header")


## Forma de Cobrança

(p + geom_bar(aes(x = formaCobranca, fill = formaCobranca), data = base_resgate)) |>
  ggplotly()

# Tabela de frequências (% sobre total de resgates por categoria)
table <- table(base_resgate$formaCobranca) |> as.data.frame()
colnames(table) <- c("Categoria", "Freq")

table$Perc <- round((table$Freq / sum(table$Freq)) * 100, 2)

table |>
  flextable() |>
  width(width = 1.3) |>
  bold(part = "header")


## Periodicidade de Cobrança


(p + geom_bar(aes(x = periodicidadeCobranca, fill = periodicidadeCobranca), data = base_resgate)) |>
  ggplotly()

# Tabela de frequências (% sobre total de resgates por categoria)
table <- table(base_resgate$periodicidadeCobranca) |> as.data.frame()
colnames(table) <- c("Categoria", "Freq")

table$Perc <- round((table$Freq / sum(table$Freq)) * 100, 2)

table |>
  flextable() |>
  width(width = 1.3) |>
  bold(part = "header")


## Cobertura


(p + geom_bar(aes(x = tipoCobertura, fill = tipoCobertura), data = base_resgate)) |>
  ggplotly()

# Tabela de frequências (% sobre total de resgates por categoria)
table <- table(base_resgate$tipoCobertura) |> as.data.frame()
colnames(table) <- c("Categoria", "Freq")

table$Perc <- round((table$Freq / sum(table$Freq)) * 100, 2)

table |>
  flextable() |>
  width(width = 1.3) |>
  bold(part = "header")


## Agrupamento


(p + geom_bar(aes(x = coberturaAgrupamento, fill = coberturaAgrupamento), data = base_resgate)) |>
  ggplotly()

# Tabela de frequências (% sobre total de resgates por categoria)
table <- table(base_resgate$coberturaAgrupamento) |> as.data.frame()
colnames(table) <- c("Categoria", "Freq")

table$Perc <- round((table$Freq / sum(table$Freq)) * 100, 2)

table |>
  flextable() |>
  width(width = 1.3) |>
  bold(part = "header")

