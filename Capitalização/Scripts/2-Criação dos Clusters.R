library(tidyverse)
library(mclust)
library(ggsurvfit)
library(arrow)
library(plotly)
library(dplyr)
library(survival)
library(broom)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Criando bases resgate e base saldamento
base <- read_rds('../Bases/capitalizacao_tratada.rds')

base$categoriaCliente[base$categoriaCliente == ""] <- "sem categoriaCliente"

base_reduzida <- base |>  
  select(dataInicioVigenciaCobertura,
         categoriaCliente,
         formaCobranca,
         tipoCobertura,
         status,
         tempo_mes,
         delta,
         delta1,
         delta2)

base_resgate <- base_reduzida

base_saldamento <- base_reduzida |> 
  filter(tipoCobertura != "SOBREVIVÊNCIA")

# Salvando as bases
arrow::write_parquet(base_resgate,
                     '../Bases/base_resgate.parquet',
                     compression = 'brotli')

arrow::write_parquet(base_saldamento,
                     '../Bases/base_saldamento.parquet',
                     compression = 'brotli')

# Lendo as bases
base_resgate <- arrow::read_parquet('../Bases/base_resgate.parquet')
base_saldamento <- arrow::read_parquet('../Bases/base_saldamento.parquet')

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

########################## Base Resgate ##########################
# categoriaCliente
km_resgate <- survfit2(Surv(tempo_mes, delta1) ~ categoriaCliente,
                       data = base_resgate) |> 
  tidy_survfit(type = 'survival')

km_balanceada <- balance_number_obs_km(km_resgate)

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
  Mclust(G = 2, modelNames = 'EEI') -> mod1

spread(km_balanceada |> 
         filter(time != 0) |> 
         select(time, estimate, strata),
       key = 'strata', value = 'estimate') |>
  select(-time) |> 
  t() |> 
  Mclust(G = 2, modelNames = 'EII') -> mod2

tb_cluster <- tibble(categoriaCliente = names(mod1$classification),
                     clusterCC = as.numeric(mod1$classification)) |> #só usei o mod1 pois mod1 e mod2 são iguais
  mutate(clusterCC = factor(clusterCC))

base_resgate <- left_join(base_resgate,
                          tb_cluster,
                          by = 'categoriaCliente')

# formaCobranca
km_resgate <- survfit2(Surv(tempo_mes, delta1) ~ formaCobranca,
                       data = base_resgate) |> 
  tidy_survfit(type = 'survival')

km_balanceada <- balance_number_obs_km(km_resgate)

spread(km_balanceada |> 
         filter(time != 0) |> 
         select(time, estimate, strata),
       key = 'strata', value = 'estimate') |>
  select(-time) |> 
  t() |>
  mclustBIC(G = 2:4)

spread(km_balanceada |> 
         filter(time != 0) |> 
         select(time, estimate, strata),
       key = 'strata', value = 'estimate') |>
  select(-time) |> 
  t() |> 
  Mclust(G = 4, modelNames = 'EII') -> mod1

spread(km_balanceada |> 
         filter(time != 0) |> 
         select(time, estimate, strata),
       key = 'strata', value = 'estimate') |>
  select(-time) |> 
  t() |> 
  Mclust(G = 4, modelNames = 'EEI') -> mod2

spread(km_balanceada |> 
         filter(time != 0) |> 
         select(time, estimate, strata),
       key = 'strata', value = 'estimate') |>
  select(-time) |> 
  t() |> 
  Mclust(G = 3, modelNames = 'EII') -> mod3

tb_cluster <- tibble(formaCobranca = names(mod1$classification),
                     clusterFC = as.numeric(mod1$classification), # só usei o mod1 pois mod1 e mod2 são iguais
                     clusterFC3 = as.numeric(mod3$classification)) |> 
  mutate(clusterFC = factor(clusterFC),
         clusterFC3 = factor(clusterFC3))

base_resgate <- left_join(base_resgate,
                          tb_cluster,
                          by = 'formaCobranca')

# tipoCobertura
km_resgate <- survfit2(Surv(tempo_mes, delta1) ~ tipoCobertura,
                       data = base_resgate) |> 
  tidy_survfit(type = 'survival')

km_balanceada <- balance_number_obs_km(km_resgate)

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
  Mclust(G = 2, modelNames = 'EEI') -> mod1

spread(km_balanceada |> 
         filter(time != 0) |> 
         select(time, estimate, strata),
       key = 'strata', value = 'estimate') |>
  select(-time) |> 
  t() |> 
  Mclust(G = 2, modelNames = 'EII') -> mod2

tb_cluster <- tibble(tipoCobertura = names(mod1$classification),
                     clusterTC = as.numeric(mod1$classification)) |> # só usei o mod1 pois mod1 e mod2 são iguais
  mutate(clusterTC = factor(clusterTC))

base_resgate <- left_join(base_resgate,
                          tb_cluster,
                          by = 'tipoCobertura')

# Salvando a base resgate com os clusters de cada variável
arrow::write_parquet(base_resgate,
                     '../Bases/base_resgate_clusters_variaveis.parquet',
                     compression = 'brotli')

# Criação da base_resgate_clusters que é a combinação dos clusters + agrupamento para os clusters com poucos dados
# Importando base de dados
base_resgate <- read_parquet("../Bases/base_resgate_clusters_variaveis.parquet")

# Criar permutações dos clusters
base_resgate <- base_resgate |>
  mutate(combinação = paste0(clusterCC, '-', clusterFC, '-', clusterTC))

# Agrupando combinações que possuem funções de risco parecidas:
# 1-4-1 e 2-4-1 viram 5-5-5 (Outros1)
# 1-2-2 e 2-2-2 viram 6-6-6 (Outros2)
# 1-3-1, 1-3-2, 2-1-2, 2-3-1 e 2-3-2 viram 7-7-7 (Outros3)
base_resgate <- base_resgate |>
  mutate(combinação_agrupada = case_when(
    combinação %in% c('1-4-1', '2-4-1') ~ '5-5-5', # Outros1
    combinação %in% c('1-2-2', '2-2-2') ~ '6-6-6', # Outros2
    combinação %in% c('1-3-1', '1-3-2', '2-1-2', '2-3-1', '2-3-2') ~ '7-7-7', # Outros3
    TRUE ~ combinação)) |>
  mutate(combinação_agrupada = factor(combinação_agrupada))

# Unir as combinações agrupadas ao conjunto de dados originais
base_resgate <- base_resgate |>
  mutate(clusterCC = case_when(
    combinação_agrupada == '5-5-5' ~ '5',
    combinação_agrupada == '6-6-6' ~ '6',
    combinação_agrupada == '7-7-7' ~ '7',
    TRUE ~ as.character(clusterCC)),
  clusterFC = case_when(
    combinação_agrupada == '5-5-5' ~ '5',
    combinação_agrupada == '6-6-6' ~ '6',
    combinação_agrupada == '7-7-7' ~ '7',
    TRUE ~ as.character(clusterFC)),
  clusterTC = case_when(
    combinação_agrupada == '5-5-5' ~ '5',
    combinação_agrupada == '6-6-6' ~ '6',
    combinação_agrupada == '7-7-7' ~ '7',
    TRUE ~ as.character(clusterTC))) |>
  mutate(clusterCC = as.factor(clusterCC),
         clusterFC = as.factor(clusterFC),
         clusterTC = as.factor(clusterTC))

km_surv <- survfit2(Surv(tempo_mes, delta1) ~ clusterCC + clusterFC + clusterTC,
                    data = base_resgate) |> 
  tidy_survfit()

pred_risk <- tibble()

for(cat in km_surv |> select(strata) |> unique() |> pull()) {
  pred_risk <- bind_rows(pred_risk, tx.emp(km_surv |> 
                                             filter(strata == cat) |>
                                             pull(time), km_surv |> 
                                             filter(strata == cat) |> 
                                             pull(estimate), cat))
}

p_sob <- ggplot(km_surv) + 
  geom_step(aes(x = time, y = estimate, color = strata)) +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 168, 24)) +
  labs(title = "Resgate")

p_risco <- ggplot(pred_risk) +
  geom_line(aes(x = time, y = hazard, color = strata)) +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 168, 24)) +
  labs(title = "Resgate")

subplot(ggplotly(p_sob), plotly::style(ggplotly(p_risco), showlegend = FALSE))

# Salvando a nova base_resgate
arrow::write_parquet(base_resgate,
                     '../Bases/base_resgate_clusters.parquet',
                     compression = 'brotli')

########################## Base Saldamento ##########################
# categoriaCliente
# Criando uma nova coluna chamada CC baseada na coluna categoriaCliente:
# Se categoriaCliente for igual a Cliente MAG, CC = 1,
# Se categoriaCliente for igual a MAG Blue, CC = 2,
# Se categoriaCliente for igual a sem categoriaCliente, CC = 3

base_saldamento <- base_saldamento |>
  mutate(CC = factor(case_when(categoriaCliente == "Cliente MAG" ~ 1,
                               categoriaCliente == "MAG Blue" ~ 2,
                               categoriaCliente == "sem categoriaCliente" ~ 3)))

# formaCobranca
km_saldamento <- survfit2(Surv(tempo_mes, delta2) ~ formaCobranca,
                          data = base_saldamento) |> 
  tidy_survfit(type = 'survival')

km_balanceada <- balance_number_obs_km(km_saldamento)

spread(km_balanceada |> 
         filter(time != 0) |> 
         select(time, estimate, strata),
       key = 'strata', value = 'estimate') |>
  select(-time) |>
  t() |>
  mclustBIC(G = 2:4)

spread(km_balanceada |> 
         filter(time != 0) |> 
         select(time, estimate, strata),
       key = 'strata', value = 'estimate') |>
  select(-time) |>
  t() |>
  Mclust(G = 4, modelNames = 'EII') -> mod1

spread(km_balanceada |> 
         filter(time != 0) |> 
         select(time, estimate, strata),
       key = 'strata', value = 'estimate') |>
  select(-time) |>
  t() |>
  Mclust(G = 2, modelNames = 'VII') -> mod2

spread(km_balanceada |> 
         filter(time != 0) |> 
         select(time, estimate, strata),
       key = 'strata', value = 'estimate') |>
  select(-time) |>
  t() |>
  Mclust(G = 3, modelNames = 'EII') -> mod3

tb_cluster <- tibble(formaCobranca = names(mod1$classification),
                     clusterFC1 = as.numeric(mod1$classification),
                     clusterFC2 = as.numeric(mod2$classification),
                     clusterFC = as.numeric(mod3$classification)) |>
  mutate(clusterFC1 = factor(clusterFC1),
         clusterFC2 = factor(clusterFC2),
         clusterFC = factor(clusterFC))

base_saldamento <- left_join(base_saldamento,
                             tb_cluster,
                             by = 'formaCobranca')

# tipoCobertura
# Criando uma nova coluna chamada TC baseada na coluna tipoCobertura:
# Se tipoCobertura  for igual a INVALIDEZ, TC = 1
# Se tipoCobertura for igual a MORTE, TC = 2

base_saldamento <- base_saldamento |>
  mutate(TC = factor(case_when(tipoCobertura == "INVALIDEZ" ~ 1,
                                      tipoCobertura == "MORTE" ~ 2)))

# Salvando a base saldamento com os clusters de cada variável
arrow::write_parquet(base_saldamento,
                     '../Bases/base_saldamento_clusters_variaveis.parquet',
                     compression = 'brotli')

## Criação da base_saldamento_clusters que é a combinação dos clusters + agrupamento nos clusters com poucos dados
# Importando base de dados
base_saldamento <- read_parquet("../Bases/base_saldamento_clusters_variaveis.parquet")

# Criar permutações dos clusters
base_saldamento <- base_saldamento |>
  mutate(combinação = paste0(CC, '-', clusterFC, '-', TC))

# Agrupando combinações que possuem funções de risco parecidas:
base_saldamento <- base_saldamento |>
  mutate(combinação_agrupada = case_when(
    combinação %in% c('1-3-1', '1-3-2') ~ '4-4-4', # Outros1
    combinação %in% c('1-1-1', '1-2-1', '2-1-1', '2-2-1', '2-3-1', '2-3-2', 
                      '3-1-1', '3-1-2', '3-2-2', '3-3-1', '3-3-2') ~ '5-5-5', # Outros2
    TRUE ~ combinação)) |>
  mutate(combinação_agrupada = factor(combinação_agrupada))

# Unir as combinações agrupadas ao conjunto de dados originais
base_saldamento <- base_saldamento |>
  mutate(CC = case_when(
    combinação_agrupada == '4-4-4' ~ '4',
    combinação_agrupada == '5-5-5' ~ '5',
    TRUE ~ as.character(CC)),
    clusterFC = case_when(
      combinação_agrupada == '4-4-4' ~ '4',
      combinação_agrupada == '5-5-5' ~ '5',
      TRUE ~ as.character(clusterFC)),
    TC = case_when(
      combinação_agrupada == '4-4-4' ~ '4',
      combinação_agrupada == '5-5-5' ~ '5',
      TRUE ~ as.character(TC))) |>
  mutate(CC = as.factor(CC),
         clusterFC = as.factor(clusterFC),
         TC = as.factor(TC))

km_surv <- survfit2(Surv(tempo_mes, delta2) ~ CC + clusterFC + TC,
                    data = base_saldamento) |> 
  tidy_survfit()

pred_risk <- tibble()

for(cat in km_surv |> select(strata) |> unique() |> pull()) {
  pred_risk <- bind_rows(pred_risk, tx.emp(km_surv |> 
                                             filter(strata == cat) |>
                                             pull(time), km_surv |> 
                                             filter(strata == cat) |> 
                                             pull(estimate), cat))
}

p_sob <- ggplot(km_surv) + 
  geom_step(aes(x = time, y = estimate, color = strata)) +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 168, 24))

p_risco <- ggplot(pred_risk) +
  geom_line(aes(x = time, y = hazard, color = strata)) +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 168, 24))

subplot(ggplotly(p_sob), plotly::style(ggplotly(p_risco), showlegend = FALSE))

# Salvando a nova base_saldamento
arrow::write_parquet(base_saldamento,
                     '../Bases/base_saldamento_clusters.parquet',
                     compression = 'brotli')