library(tidyverse)
library(mclust)
library(ggsurvfit)
library(arrow)
library(plotly)
library(dplyr)
library(survival)
library(broom)

# Definindo diretório de trabalho na pasta raiz do script
dirname(rstudioapi::getSourceEditorContext()$path) |> 
  setwd()

# Carregando a base de dados
base <- read_parquet('../Bases/base_modificada.parquet')

# Criar funções necessárias
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

balance_number_obs_km <- function(km) {
  tempos_unicos <- sort(unique(km$time))
  
  maior_tempo <- max(tempos_unicos)
  
  out <- tibble(time = NA,
                estimate = NA,
                strata = NA)
  
  for(c in unique(km$strata)) {
    sub_km <- km |> 
      filter(strata == c)
    
    maior_tempo_strata <- max(sub_km$time)
    ultima_sob_strata <- sub_km |> 
      slice(which.max(time)) |> 
      pull(estimate)
    for(t in tempos_unicos) {
      if(t > maior_tempo_strata) {
        out <- out |> 
          bind_rows(tibble(time = t,
                           estimate = ultima_sob_strata,
                           strata = c))
      } else {
        if(t %in% sub_km$time) {
          out <- out |> 
            bind_rows(sub_km |> 
                        filter(time == t) |> 
                        select(time, estimate, strata))
        } else {
          out <- out |> 
            bind_rows(sub_km |> 
                        filter(time <= t) |> 
                        slice(which.max(time)) |> 
                        select(estimate, strata) |> 
                        mutate(time = t))
        }
      }
    }
  }
  
  return(out[-1, ])
}

# Variáveis

# categoriaCliente
km = survfit2(Surv(tempo_mes, delta) ~ categoriaCliente, data = base) |> 
  tidy_survfit(type = 'survival')

km_balanceada <- balance_number_obs_km(km)

spread(km_balanceada |> 
         filter(time != 0) |> 
         select(time, estimate, strata),
       key = 'strata', value = 'estimate') |>
  select(-time) |> 
  t() |>
  mclustBIC(G = 2:3)

spread(km_balanceada |> 
         filter(time != 0) |> 
         select(time, estimate, strata),
       key = 'strata', value = 'estimate') |>
  select(-time) |> 
  t() |> 
  Mclust(G = 2, modelNames = 'EII') -> mod1

spread(km_balanceada |> 
         filter(time != 0) |> 
         select(time, estimate, strata),
       key = 'strata', value = 'estimate') |>
  select(-time) |> 
  t() |> 
  Mclust(G = 2, modelNames = 'EEI') -> mod2

tb_cluster <- tibble(categoriaCliente = names(mod1$classification),
                     clusterCC = as.numeric(mod1$classification)) |> # só usei o mod1, pois mod1 e mod2 são iguais
  mutate(clusterCC = factor(clusterCC))

base <- left_join(base, tb_cluster, by = 'categoriaCliente')

# ramoDescricao
km = survfit2(Surv(tempo_mes, delta) ~ ramoDescricao, data = base) |> 
  tidy_survfit(type = 'survival')

km_balanceada <- balance_number_obs_km(km)

spread(km_balanceada |> 
         filter(time != 0) |> 
         select(time, estimate, strata),
       key = 'strata', value = 'estimate') |>
  select(-time) |> 
  t() |>
  mclustBIC(G = 2:3)

spread(km_balanceada |> 
         filter(time != 0) |> 
         select(time, estimate, strata),
       key = 'strata', value = 'estimate') |>
  select(-time) |> 
  t() |> 
  Mclust(G = 3, modelNames = 'EII') -> mod1

spread(km_balanceada |> 
         filter(time != 0) |> 
         select(time, estimate, strata),
       key = 'strata', value = 'estimate') |>
  select(-time) |> 
  t() |> 
  Mclust(G = 3, modelNames = 'EEI') -> mod2

spread(km_balanceada |> 
         filter(time != 0) |> 
         select(time, estimate, strata),
       key = 'strata', value = 'estimate') |>
  select(-time) |> 
  t() |> 
  Mclust(G = 2, modelNames = 'VVI') -> mod3

tb_cluster <- tibble(ramoDescricao = names(mod1$classification),
                     clusterRD1 = as.numeric(mod1$classification), # mod1 = mod2
                     clusterRD3 = as.numeric(mod3$classification)) |> 
  mutate(clusterRD1 = factor(clusterRD1),
         clusterRD3 = factor(clusterRD3))

base <- left_join(base, tb_cluster, by = 'ramoDescricao')

# produtoNome
km = survfit2(Surv(tempo_mes, delta) ~ produtoNome, data = base) |> 
  tidy_survfit(type = 'survival')

km_balanceada <- balance_number_obs_km(km)

spread(km_balanceada |> 
         filter(time != 0) |> 
         select(time, estimate, strata),
       key = 'strata', value = 'estimate') |>
  select(-time) |> 
  t() |>
  mclustBIC(G = 2:3)

spread(km_balanceada |> 
         filter(time != 0) |> 
         select(time, estimate, strata),
       key = 'strata', value = 'estimate') |>
  select(-time) |> 
  t() |> 
  Mclust(G = 3, modelNames = 'VEI') -> mod1

spread(km_balanceada |> 
         filter(time != 0) |> 
         select(time, estimate, strata),
       key = 'strata', value = 'estimate') |>
  select(-time) |> 
  t() |> 
  Mclust(G = 3, modelNames = 'VII') -> mod2

spread(km_balanceada |> 
         filter(time != 0) |> 
         select(time, estimate, strata),
       key = 'strata', value = 'estimate') |>
  select(-time) |> 
  t() |> 
  Mclust(G = 2, modelNames = 'VVI') -> mod3

tb_cluster <- tibble(produtoNome = names(mod1$classification),
                     clusterPN1 = as.numeric(mod1$classification),
                     clusterPN2 = as.numeric(mod2$classification),
                     clusterPN3 = as.numeric(mod3$classification)) |> 
  mutate(clusterPN1 = factor(clusterPN1),
         clusterPN2 = factor(clusterPN2),
         clusterPN3 = factor(clusterPN3))

base <- left_join(base, tb_cluster, by = 'produtoNome')

# Salvando a basee com os clusters de cada variável
arrow::write_parquet(base,
                     '../Bases/base_clusters.parquet',
                     compression = 'brotli')