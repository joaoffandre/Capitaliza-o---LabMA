# Salvei os modelos nas pastas com _3  (Pacote Atualizado), pois esses modelos são feitos com os agrupamentos baseados nas combinações que possuem riscos parecidos

library(arrow)
library(tidyverse)
library(ggsurvfit)
library(lnmixsurv)
library(plotly)
library(bayesplot)
library(posterior)
library(kableExtra)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

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

############# Resgate ##############
# Importando base de dados
base_resgate <- read_parquet("../Bases/base_resgate_clusters.parquet")

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

############ EM - Resgate ############
# Uma Componente (só rodei 1 semente)
model_1comp_EM1 <- survival_ln_mixture_em(Surv(tempo_mes, delta1) ~ clusterCC + clusterFC + clusterTC,
                                          data = base_resgate, 
                                          mixture_components = 1,
                                          iter = 1000,
                                          number_em_search = 1000,
                                          starting_seed = 1)

plot(model_1comp_EM1)

write_rds(model_1comp_EM1, '../Modelos/EM_3/modelo_1comp_EM1.rds')

# Sobrevivência
preds1EM <- plot_fit_on_data(model_1comp_EM1, data = base_resgate)

plot1 <- preds1EM$ggplot +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 168, 24)) +
  labs(title = "Resgate - 1 Componente EM")

# Risco
preds1EM_risco <- plot_fit_on_data(model_1comp_EM1, type = "hazard", data = base_resgate)

plot2 <- preds1EM_risco$ggplot +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 168, 24)) +
  labs(title = "Resgate - 1 Componente EM")

subplot(ggplotly(plot1), plotly::style(ggplotly(plot2), showlegend = FALSE))

# Duas Componentes

# Primeiro
model_2comp_EM1 <- survival_ln_mixture_em(Surv(tempo_mes, delta1) ~ clusterCC + clusterFC + clusterTC,
                                         data = base_resgate, 
                                         mixture_components = 2,
                                         iter = 1000,
                                         number_em_search = 1000,
                                         starting_seed = 2)

plot(model_2comp_EM1)

write_rds(model_2comp_EM1, '../Modelos/EM_3/modelo_2comp_EM1.rds')
model_2comp_EM1 <- read_rds('../Modelos/EM_3/modelo_2comp_EM1.rds')

# Sobrevivência
preds2EM <- plot_fit_on_data(model_2comp_EM1, data = base_resgate)

plot1 <- preds2EM$ggplot +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 168, 24)) +
  labs(title = "Resgate - 2 Componentes EM")

# Risco
preds2EM_risco <- plot_fit_on_data(model_2comp_EM1, type = "hazard", data = base_resgate)

plot2 <- preds2EM_risco$ggplot +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 168, 24)) +
  labs(title = "Resgate - 2 Componentes EM")

subplot(ggplotly(plot1), plotly::style(ggplotly(plot2), showlegend = FALSE))

# Segundo
model_2comp_EM2 <- survival_ln_mixture_em(Surv(tempo_mes, delta1) ~ clusterCC + clusterFC + clusterTC,
                                          data = base_resgate, 
                                          mixture_components = 2,
                                          iter = 1000,
                                          number_em_search = 1000,
                                          starting_seed = 120)

plot(model_2comp_EM2)

write_rds(model_2comp_EM2, '../Modelos/EM_3/modelo_2comp_EM2.rds')

# Sobrevivência
preds2EM <- plot_fit_on_data(model_2comp_EM2, data = base_resgate)

plot1 <- preds2EM$ggplot +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 168, 24)) +
  labs(title = "Resgate - 2 Componentes EM")

# Risco
preds2EM_risco <- plot_fit_on_data(model_2comp_EM2, type = "hazard", data = base_resgate)

plot2 <- preds2EM_risco$ggplot +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 168, 24)) +
  labs(title = "Resgate - 2 Componentes EM")

subplot(ggplotly(plot1), plotly::style(ggplotly(plot2), showlegend = FALSE))

# Terceiro
model_2comp_EM3 <- survival_ln_mixture_em(Surv(tempo_mes, delta1) ~ clusterCC + clusterFC + clusterTC,
                                          data = base_resgate, 
                                          mixture_components = 2,
                                          iter = 1000,
                                          number_em_search = 1000,
                                          starting_seed = 24)

plot(model_2comp_EM3)

write_rds(model_2comp_EM3, '../Modelos/EM_3/modelo_2comp_EM3.rds')

# Sobrevivência
preds2EM <- plot_fit_on_data(model_2comp_EM3, data = base_resgate)

plot1 <- preds2EM$ggplot +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 168, 24)) +
  labs(title = "Resgate - 2 Componentes EM")

# Risco
preds2EM_risco <- plot_fit_on_data(model_2comp_EM3, type = "hazard", data = base_resgate)

plot2 <- preds2EM_risco$ggplot +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 168, 24)) +
  labs(title = "Resgate - 2 Componentes EM")

subplot(ggplotly(plot1), plotly::style(ggplotly(plot2), showlegend = FALSE))

# Três Componentes
# Primeiro
model_3comp_EM1 <- survival_ln_mixture_em(Surv(tempo_mes, delta1) ~ clusterCC + clusterFC + clusterTC,
                                         data = base_resgate, 
                                         mixture_components = 3,
                                         iter = 1000,
                                         number_em_search = 1000,
                                         starting_seed = 3)

plot(model_3comp_EM1)

write_rds(model_3comp_EM1, '../Modelos/EM_3/modelo_3comp_EM1.rds')

# Sobrevivência
preds3EM <- plot_fit_on_data(model_3comp_EM1, data = base_resgate)

plot1 <- preds3EM$ggplot +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 168, 24)) +
  labs(title = "Resgate - 3 Componentes EM")

# Risco
preds3EM_risco <- plot_fit_on_data(model_3comp_EM1, type = "hazard", data = base_resgate)

plot2 <- preds3EM_risco$ggplot +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 168, 24)) +
  labs(title = "Resgate - 3 Componentes EM")

subplot(ggplotly(plot1), plotly::style(ggplotly(plot2), showlegend = FALSE))

# Segundo
model_3comp_EM2 <- survival_ln_mixture_em(Surv(tempo_mes, delta1) ~ clusterCC + clusterFC + clusterTC,
                                          data = base_resgate, 
                                          mixture_components = 3,
                                          iter = 1000,
                                          number_em_search = 1000,
                                          starting_seed = 18)

plot(model_3comp_EM2)

write_rds(model_3comp_EM2, '../Modelos/EM_3/modelo_3comp_EM2.rds')

# Sobrevivência
preds3EM <- plot_fit_on_data(model_3comp_EM2, data = base_resgate)

plot1 <- preds3EM$ggplot +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 168, 24)) +
  labs(title = "Resgate - 3 Componentes EM")

# Risco
preds3EM_risco <- plot_fit_on_data(model_3comp_EM2, type = "hazard", data = base_resgate)

plot2 <- preds3EM_risco$ggplot +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 168, 24)) +
  labs(title = "Resgate - 3 Componentes EM")

subplot(ggplotly(plot1), plotly::style(ggplotly(plot2), showlegend = FALSE))

# Terceiro
model_3comp_EM3 <- survival_ln_mixture_em(Surv(tempo_mes, delta1) ~ clusterCC + clusterFC + clusterTC,
                                          data = base_resgate, 
                                          mixture_components = 3,
                                          iter = 1000,
                                          number_em_search = 1000,
                                          starting_seed = 360)

plot(model_3comp_EM3)

write_rds(model_3comp_EM3, '../Modelos/EM_3/modelo_3comp_EM3.rds')

# Sobrevivência
preds3EM <- plot_fit_on_data(model_3comp_EM3, data = base_resgate)

plot1 <- preds3EM$ggplot +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 168, 24)) +
  labs(title = "Resgate - 3 Componentes EM")

# Risco
preds3EM_risco <- plot_fit_on_data(model_3comp_EM3, type = "hazard", data = base_resgate)

plot2 <- preds3EM_risco$ggplot +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 168, 24)) +
  labs(title = "Resgate - 3 Componentes EM")

subplot(ggplotly(plot1), plotly::style(ggplotly(plot2), showlegend = FALSE))

# Quatro Componentes
# Primeiro
model_4comp_EM1 <- survival_ln_mixture_em(Surv(tempo_mes, delta1) ~ clusterCC + clusterFC + clusterTC,
                                         data = base_resgate, 
                                         mixture_components = 4,
                                         iter = 1000,
                                         number_em_search = 1000,
                                         starting_seed = 4)

plot(model_4comp_EM1)

write_rds(model_4comp_EM1, '../Modelos/EM_3/modelo_4comp_EM1.rds')

# Sobrevivência
preds4EM <- plot_fit_on_data(model_4comp_EM1, data = base_resgate)

plot1 <- preds4EM$ggplot +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 168, 24)) +
  labs(title = "Resgate - 4 Componentes EM")

# Risco
preds4EM_risco <- plot_fit_on_data(model_4comp_EM1, type = "hazard", data = base_resgate)

plot2 <- preds4EM_risco$ggplot +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 168, 24)) +
  labs(title = "Resgate - 4 Componentes EM")

subplot(ggplotly(plot1), plotly::style(ggplotly(plot2), showlegend = FALSE))

# Segundo
model_4comp_EM2 <- survival_ln_mixture_em(Surv(tempo_mes, delta1) ~ clusterCC + clusterFC + clusterTC,
                                          data = base_resgate, 
                                          mixture_components = 4,
                                          iter = 1000,
                                          number_em_search = 1000,
                                          starting_seed = 440)

plot(model_4comp_EM2)

write_rds(model_4comp_EM2, '../Modelos/EM_3/modelo_4comp_EM2.rds')

# Sobrevivência
preds4EM <- plot_fit_on_data(model_4comp_EM2, data = base_resgate)

plot1 <- preds4EM$ggplot +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 168, 24)) +
  labs(title = "Resgate - 4 Componentes EM")

# Risco
preds4EM_risco <- plot_fit_on_data(model_4comp_EM2, type = "hazard", data = base_resgate)

plot2 <- preds4EM_risco$ggplot +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 168, 24)) +
  labs(title = "Resgate - 4 Componentes EM")

subplot(ggplotly(plot1), plotly::style(ggplotly(plot2), showlegend = FALSE))

# Terceiro
model_4comp_EM3 <- survival_ln_mixture_em(Surv(tempo_mes, delta1) ~ clusterCC + clusterFC + clusterTC,
                                          data = base_resgate, 
                                          mixture_components = 4,
                                          iter = 1000,
                                          number_em_search = 1000,
                                          starting_seed = 8)

plot(model_4comp_EM3)

write_rds(model_4comp_EM3, '../Modelos/EM_3/modelo_4comp_EM3.rds')

# Sobrevivência
preds4EM <- plot_fit_on_data(model_4comp_EM3, data = base_resgate)

plot1 <- preds4EM$ggplot +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 168, 24)) +
  labs(title = "Resgate - 4 Componentes EM")

# Risco
preds4EM_risco <- plot_fit_on_data(model_4comp_EM3, type = "hazard", data = base_resgate)

plot2 <- preds4EM_risco$ggplot +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 168, 24)) +
  labs(title = "Resgate - 4 Componentes EM")

subplot(ggplotly(plot1), plotly::style(ggplotly(plot2), showlegend = FALSE))

############ Modelagem Bayesiana - Resgate ############
# Uma componente
model_1comp <- survival_ln_mixture(Surv(tempo_mes, delta1) ~ clusterCC + clusterFC + clusterTC,
                                   data = base_resgate, 
                                   mixture_components = 1,
                                   iter = 16000,
                                   chains = 4,
                                   cores = 4,
                                   warmup = 0,
                                   em_iter = 1000,
                                   number_em_search = 1000,
                                   show_progress = TRUE,
                                   starting_seed = 1)

mcmc_trace(model_1comp$posterior)

write_rds(model_1comp, '../Modelos/Bay_3/modelo_1comp.rds')
model_1comp <- read_rds('../Modelos/Bay_3/modelo_1comp.rds')

# Retirando o warmup
model_1comp$posterior <- posterior::subset_draws(model_1comp$posterior, iter = 14001:16000)

# Olhando o comportamento das cadeias
mcmc_trace(model_1comp$posterior)

# Sobrevivência
preds1Bayesian <- plot_fit_on_data(model_1comp, interval = "credible", data = base_resgate)

(preds1Bayesian$ggplot +
    theme_bw() +
    scale_x_continuous(breaks = seq(0, 168, 24)) +
    labs(title = "Resgate - 1 Componente Bayesiano")) |>
  ggplotly()

# Risco
preds1Bayesian_risco <- plot_fit_on_data(model_1comp, type = "hazard", interval = "credible", data = base_resgate)

(preds1Bayesian_risco$ggplot +
    theme_bw() +
    scale_x_continuous(breaks = seq(0, 168, 24)) +
    labs(title = "Resgate - 1 Componente Bayesiano")) |>
  ggplotly()

# Duas componentes
model_2comp <- survival_ln_mixture(Surv(tempo_mes, delta1) ~ clusterCC + clusterFC + clusterTC,
                                   data = base_resgate, 
                                   mixture_components = 2,
                                   iter = 16000,
                                   chains = 4,
                                   cores = 4,
                                   warmup = 0,
                                   em_iter = 1000,
                                   number_em_search = 1000,
                                   show_progress = TRUE,
                                   starting_seed = 2)

mcmc_trace(model_2comp$posterior)

write_rds(model_2comp, '../Modelos/Bay_3/modelo_2comp.rds')
model_2comp <- read_rds('../Modelos/Bay_3/modelo_2comp.rds')

# Retirando o warmup
model_2comp$posterior <- posterior::subset_draws(model_2comp$posterior, iter = 14001:16000)

# Olhando o comportamento das cadeias
mcmc_trace(model_2comp$posterior)

# Sobrevivência
preds2Bayesian <- plot_fit_on_data(model_2comp, interval = "credible", data = base_resgate)

(preds2Bayesian$ggplot +
    theme_bw() +
    scale_x_continuous(breaks = seq(0, 168, 24)) +
    labs(title = "Resgate - 2 Componentes Bayesiano")) |>
  ggplotly()

# Risco
preds2Bayesian_risco <- plot_fit_on_data(model_2comp, type = "hazard", interval = "credible", data = base_resgate)

(preds2Bayesian_risco$ggplot +
    theme_bw() +
    scale_x_continuous(breaks = seq(0, 168, 24)) +
    labs(title = "Resgate - 2 Componentes Bayesiano")) |>
  ggplotly()

# Três componentes
model_3comp <- survival_ln_mixture(Surv(tempo_mes, delta1) ~ clusterCC + clusterFC + clusterTC,
                                   data = base_resgate, 
                                   mixture_components = 3,
                                   iter = 16000,
                                   chains = 4,
                                   cores = 4,
                                   warmup = 0,
                                   em_iter = 1000,
                                   number_em_search = 1000,
                                   show_progress = TRUE,
                                   starting_seed = 3)

mcmc_trace(model_3comp$posterior)

write_rds(model_3comp, '../Modelos/Bay_3/modelo_3comp.rds')
model_3comp <- read_rds('../Modelos/Bay_3/modelo_3comp.rds')

# Retirando o warmup
model_3comp$posterior <- posterior::subset_draws(model_3comp$posterior, iter = 14001:16000)

# Olhando o comportamento das cadeias
mcmc_trace(model_3comp$posterior)

# Sobrevivência
preds3Bayesian <- plot_fit_on_data(model_3comp, interval = "credible", data = base_resgate)

(preds3Bayesian$ggplot +
    theme_bw() +
    scale_x_continuous(breaks = seq(0, 168, 24)) +
    labs(title = "Resgate - 3 Componentes Bayesiano")) |>
  ggplotly()

# Risco
preds3Bayesian_risco <- plot_fit_on_data(model_3comp, type = "hazard", interval = "credible", data = base_resgate)

(preds3Bayesian_risco$ggplot +
    theme_bw() +
    scale_x_continuous(breaks = seq(0, 168, 24)) +
    labs(title = "Resgate - 3 Componentes Bayesiano")) |>
  ggplotly()

# Quatro componentes
model_4comp <- survival_ln_mixture(Surv(tempo_mes, delta1) ~ clusterCC + clusterFC + clusterTC,
                                   data = base_resgate, 
                                   mixture_components = 4,
                                   iter = 16000,
                                   chains = 4,
                                   cores = 4,
                                   warmup = 0,
                                   em_iter = 1000,
                                   number_em_search = 1000,
                                   show_progress = TRUE,
                                   starting_seed = 440)

mcmc_trace(model_4comp$posterior)

write_rds(model_4comp, '../Modelos/Bay_3/modelo_4comp.rds') #oq ta salvo modelo_4comp é seed 440
model_4comp <- read_rds('../Modelos/Bay_3/modelo_4comp.rds')

# Retirando o warmup
model_4comp$posterior <- posterior::subset_draws(model_4comp$posterior, iter = 14001:16000)

# Olhando o comportamento das cadeias
mcmc_trace(model_4comp$posterior)

# Sobrevivência
preds4Bayesian <- plot_fit_on_data(model_4comp, interval = "credible", data = base_resgate)

(preds4Bayesian$ggplot +
    theme_bw() +
    scale_x_continuous(breaks = seq(0, 168, 24)) +
    labs(title = "Resgate - 4 Componentes Bayesiano")) |>
  ggplotly()

# Risco
preds4Bayesian_risco <- plot_fit_on_data(model_4comp, type = "hazard", interval = "credible", data = base_resgate)

(preds4Bayesian_risco$ggplot +
    theme_bw() +
    scale_x_continuous(breaks = seq(0, 168, 24)) +
    labs(title = "Resgate - 4 Componentes Bayesiano")) |>
  ggplotly()

#### Métricas - Bayesiano Resgate ####
metricas_1comp <- fit_metrics(preds1Bayesian$preds, nobs = nobs(model_1comp)) |> # métricas
  mutate(weighted_value = (n_strata/nobs(model_1comp)) * value) |> # adicionar peso
  group_by(chain, metric) |> # agrupar por cadeia e métrica
  summarise(value = round(sum(weighted_value), 5)) |> # somar para obter a média ponderada
  mutate(mixture_components = factor(1)) # adicionar número de componentes utilizado

metricas_2comp <- fit_metrics(preds2Bayesian$preds, nobs = nobs(model_2comp)) |> # métricas
  mutate(weighted_value = (n_strata/nobs(model_2comp)) * value) |> # adicionar peso
  group_by(chain, metric) |> # agrupar por cadeia e métrica
  summarise(value = round(sum(weighted_value), 5)) |> # somar para obter a média ponderada
  mutate(mixture_components = factor(2)) # adicionar número de componentes utilizado

metricas_3comp <- fit_metrics(preds3Bayesian$preds, nobs = nobs(model_3comp)) |> # métricas
  mutate(weighted_value = (n_strata/nobs(model_3comp)) * value) |> # adicionar peso
  group_by(chain, metric) |> # agrupar por cadeia e métrica
  summarise(value = round(sum(weighted_value), 5)) |> # somar para obter a média ponderada
  mutate(mixture_components = factor(3)) # adicionar número de componentes utilizado

metricas_4comp <- fit_metrics(preds4Bayesian$preds, nobs = nobs(model_4comp)) |> # métricas
  mutate(weighted_value = (n_strata/nobs(model_4comp)) * value) |> # adicionar peso
  group_by(chain, metric) |> # agrupar por cadeia e métrica
  summarise(value = round(sum(weighted_value), 5)) |> # somar para obter a média ponderada
  mutate(mixture_components = factor(4)) # adicionar número de componentes utilizado

# Agrupar métricas
metricas <- bind_rows(metricas_1comp, metricas_2comp, metricas_3comp, metricas_4comp)

# Salvar métricas
write_rds(metricas, '../Modelos/Bay_3/metricas_resgate.rds')

#################################################################################################

############# Saldamento ##############
# Importando base de dados
base_saldamento <- read_parquet("../Bases/base_saldamento_clusters.parquet")

km_surv_sald <- survfit2(Surv(tempo_mes, delta2) ~ CC + clusterFC + TC,
                         data = base_saldamento) |>
  tidy_survfit()

pred_risk_sald <- tibble()

for(cat in km_surv_sald |> select(strata) |> unique() |> pull()) {
  pred_risk_sald <- bind_rows(pred_risk_sald, tx.emp(km_surv_sald |> 
                                                       filter(strata == cat) |>
                                                       pull(time), km_surv_sald |> 
                                                       filter(strata == cat) |> 
                                                       pull(estimate), cat))
}

p_sob_sald <- ggplot(km_surv_sald) + 
  geom_step(aes(x = time, y = estimate, color = strata)) +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 168, 24)) +
  labs(title = "Saldamento")

p_risco_sald <- ggplot(pred_risk_sald) +
  geom_line(aes(x = time, y = hazard, color = strata)) +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 168, 24)) +
  labs(title = "Saldamento")

subplot(ggplotly(p_sob_sald), plotly::style(ggplotly(p_risco_sald), showlegend = FALSE))

### Rodar isso antes de rodar os modelos
# Preciso truncar antes de modelar (truncar é definir como censura (delta2 = 0))
# Truncar 1-2-2 no tempo 100, o n.risk no tempo 100 é de 23;  
# Truncar 4-4-4 no tempo 84, o n.risk no tempo 84 é de 211;  
# Truncar 5-5-5 no tempo 96, o n.risk no tempo 96 é de 317.

base_saldamento <- base_saldamento |>
  mutate(delta2 = case_when((combinação_agrupada == "1-2-2" & tempo_mes > 100) ~ 0,
                            (combinação_agrupada == "4-4-4" & tempo_mes > 84) ~ 0,
                            (combinação_agrupada == "5-5-5" & tempo_mes > 96) ~ 0,
                            TRUE ~ delta2),
         tempo_mes = case_when((combinação_agrupada == "1-2-2" & tempo_mes > 100) ~ 100,
                               (combinação_agrupada == "4-4-4" & tempo_mes > 84) ~ 84,
                               (combinação_agrupada == "5-5-5" & tempo_mes > 96) ~ 96,
                               TRUE ~ tempo_mes))

km_surv_sald <- survfit2(Surv(tempo_mes, delta2) ~ CC + clusterFC + TC,
                         data = base_saldamento) |>
  tidy_survfit()

pred_risk_sald <- tibble()

for(cat in km_surv_sald |> select(strata) |> unique() |> pull()) {
  pred_risk_sald <- bind_rows(pred_risk_sald, tx.emp(km_surv_sald |> 
                                                       filter(strata == cat) |>
                                                       pull(time), km_surv_sald |> 
                                                       filter(strata == cat) |> 
                                                       pull(estimate), cat))
}

p_sob_sald <- ggplot(km_surv_sald) + 
  geom_step(aes(x = time, y = estimate, color = strata)) +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 168, 24)) +
  labs(title = "Saldamento")

p_risco_sald <- ggplot(pred_risk_sald) +
  geom_line(aes(x = time, y = hazard, color = strata)) +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 168, 24)) +
  labs(title = "Saldamento")

subplot(ggplotly(p_sob_sald), plotly::style(ggplotly(p_risco_sald), showlegend = FALSE))

############ EM - Saldamento ############
# Uma Componente
model_1comp_EM_sald <- survival_ln_mixture_em(Surv(tempo_mes, delta2) ~ CC + clusterFC + TC,
                                               data = base_saldamento, 
                                               mixture_components = 1,
                                               iter = 1000,
                                               number_em_search = 1000,
                                               starting_seed = 1)

plot(model_1comp_EM_sald)

write_rds(model_1comp_EM_sald, '../Modelos/EM_3/modelo_1comp_EM_sald.rds')

# Sobrevivência
preds1EM_sald <- plot_fit_on_data(model_1comp_EM_sald, data = base_saldamento)

plot1 <- preds1EM_sald$ggplot +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 168, 24)) +
  labs(title = "Saldamento - 1 Componente EM")

# Risco
preds1EM_sald_risco <- plot_fit_on_data(model_1comp_EM_sald, type = "hazard", data = base_saldamento)

plot2 <- preds1EM_sald_risco$ggplot +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 168, 24)) +
  labs(title = "Saldamento - 1 Componente EM")

subplot(ggplotly(plot1), plotly::style(ggplotly(plot2), showlegend = FALSE))

# Duas Componentes
model_2comp_EM_sald <- survival_ln_mixture_em(Surv(tempo_mes, delta2) ~ CC + clusterFC + TC,
                                              data = base_saldamento, 
                                              mixture_components = 2,
                                              iter = 1000,
                                              number_em_search = 1000,
                                              starting_seed = 2)

plot(model_2comp_EM_sald)

write_rds(model_2comp_EM_sald, '../Modelos/EM_3/modelo_2comp_EM_sald.rds')

# Sobrevivência
preds2EM_sald <- plot_fit_on_data(model_2comp_EM_sald1, data = base_saldamento)

plot1 <- preds2EM_sald$ggplot +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 168, 24)) +
  labs(title = "Saldamento - 2 Componentes EM")

# Risco
preds2EM_sald_risco <- plot_fit_on_data(model_2comp_EM_sald, type = "hazard", data = base_saldamento)

plot2 <- preds2EM_sald_risco$ggplot +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 168, 24)) +
  labs(title = "Saldamento - 2 Componentes EM")

subplot(ggplotly(plot1), plotly::style(ggplotly(plot2), showlegend = FALSE))

# Três Componentes
model_3comp_EM_sald <- survival_ln_mixture_em(Surv(tempo_mes, delta2) ~ CC + clusterFC + TC,
                                              data = base_saldamento, 
                                              mixture_components = 3,
                                              iter = 1000,
                                              number_em_search = 1000,
                                              starting_seed = 3)

plot(model_3comp_EM_sald)

write_rds(model_3comp_EM_sald, '../Modelos/EM_3/modelo_3comp_EM_sald.rds')

# Sobrevivência
preds3EM_sald <- plot_fit_on_data(model_3comp_EM_sald, data = base_saldamento)

plot1 <- preds3EM_sald$ggplot +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 168, 24)) +
  labs(title = "Saldamento - 3 Componentes EM")

# Risco
preds3EM_sald_risco <- plot_fit_on_data(model_3comp_EM_sald, type = "hazard", data = base_saldamento)

plot2 <- preds3EM_sald_risco$ggplot +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 168, 24)) +
  labs(title = "Saldamento - 3 Componentes EM")

subplot(ggplotly(plot1), plotly::style(ggplotly(plot2), showlegend = FALSE))

# Quatro Componentes
model_4comp_EM_sald <- survival_ln_mixture_em(Surv(tempo_mes, delta2) ~ CC + clusterFC + TC,
                                              data = base_saldamento, 
                                              mixture_components = 4,
                                              iter = 1000,
                                              number_em_search = 1000,
                                              starting_seed = 400)

plot(model_4comp_EM_sald)

write_rds(model_4comp_EM_sald, '../Modelos/EM_3/modelo_4comp_EM_sald.rds')

# Sobrevivência
preds4EM_sald <- plot_fit_on_data(model_4comp_EM_sald, data = base_saldamento)

plot1 <- preds4EM_sald$ggplot +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 168, 24)) +
  labs(title = "Saldamento - 4 Componentes EM")

# Risco
preds4EM_sald_risco <- plot_fit_on_data(model_4comp_EM_sald, type = "hazard", data = base_saldamento)

plot2 <- preds4EM_sald_risco$ggplot +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 168, 24)) +
  labs(title = "Saldamento - 4 Componentes EM")

subplot(ggplotly(plot1), plotly::style(ggplotly(plot2), showlegend = FALSE))

############ Modelagem Bayesiana - Saldamento ############
# Uma componente
model_1comp_sald <- survival_ln_mixture(Surv(tempo_mes, delta2) ~ CC + clusterFC + TC,
                                        data = base_saldamento, 
                                        mixture_components = 1,
                                        iter = 16000,
                                        chains = 4,
                                        cores = 4,
                                        warmup = 0,
                                        em_iter = 1000,
                                        number_em_search = 1000,
                                        show_progress = TRUE,
                                        starting_seed = 1)

mcmc_trace(model_1comp_sald$posterior)

write_rds(model_1comp_sald, '../Modelos/Bay_3/modelo_1comp_sald.rds')
model_1comp_sald <- read_rds('../Modelos/Bay_3/modelo_1comp_sald.rds')

# Retirando o warmup
model_1comp_sald$posterior <- posterior::subset_draws(model_1comp_sald$posterior, iter = 14001:16000)

# Olhando o comportamento das cadeias
mcmc_trace(model_1comp_sald$posterior)

# Sobrevivência
preds1Bayesian_sald <- plot_fit_on_data(model_1comp_sald, interval = "credible", data = base_saldamento)

(preds1Bayesian_sald$ggplot +
    theme_bw() +
    scale_x_continuous(breaks = seq(0, 168, 24)) +
    labs(title = "Saldamento - 1 Componente Bayesiano")) |>
  ggplotly()

# Risco
preds1Bayesian_sald_risco <- plot_fit_on_data(model_1comp_sald, type = "hazard", interval = "credible", data = base_saldamento)

(preds1Bayesian_sald_risco$ggplot +
    theme_bw() +
    scale_x_continuous(breaks = seq(0, 168, 24)) +
    labs(title = "Saldamento - 1 Componente Bayesiano")) |>
  ggplotly()

# Duas componentes
model_2comp_sald <- survival_ln_mixture(Surv(tempo_mes, delta2) ~ CC + clusterFC + TC,
                                        data = base_saldamento, 
                                        mixture_components = 2,
                                        iter = 16000,
                                        chains = 4,
                                        cores = 4,
                                        warmup = 0,
                                        em_iter = 1000,
                                        number_em_search = 1000,
                                        show_progress = TRUE,
                                        starting_seed = 2)

mcmc_trace(model_2comp_sald$posterior)

write_rds(model_2comp_sald, '../Modelos/Bay_3/modelo_2comp_sald.rds')
model_2comp_sald <- read_rds('../Modelos/Bay_3/modelo_2comp_sald.rds')

# Retirando o warmup
model_2comp_sald$posterior <- posterior::subset_draws(model_2comp_sald$posterior, iter = 14001:16000)

# Olhando o comportamento das cadeias
mcmc_trace(model_2comp_sald$posterior)

# Sobrevivência
preds2Bayesian_sald <- plot_fit_on_data(model_2comp_sald, interval = "credible", data = base_saldamento)

(preds2Bayesian_sald$ggplot +
    theme_bw() +
    scale_x_continuous(breaks = seq(0, 168, 24)) +
    labs(title = "Saldamento - 2 Componentes Bayesiano")) |>
  ggplotly()

# Risco
preds2Bayesian_sald_risco <- plot_fit_on_data(model_2comp_sald, type = "hazard", interval = "credible", data = base_saldamento)

(preds2Bayesian_sald_risco$ggplot +
    theme_bw() +
    scale_x_continuous(breaks = seq(0, 168, 24)) +
    labs(title = "Saldamento - 2 Componentes Bayesiano")) |>
  ggplotly()

# Três componentes
model_3comp_sald <- survival_ln_mixture(Surv(tempo_mes, delta2) ~ CC + clusterFC + TC,
                                        data = base_saldamento, 
                                        mixture_components = 3,
                                        iter = 16000,
                                        chains = 4,
                                        cores = 4,
                                        warmup = 0,
                                        em_iter = 1000,
                                        number_em_search = 1000,
                                        show_progress = TRUE,
                                        starting_seed = 3)

mcmc_trace(model_3comp_sald$posterior)

write_rds(model_3comp_sald, '../Modelos/Bay_3/modelo_3comp_sald.rds')
model_3comp_sald <- read_rds('../Modelos/Bay_3/modelo_3comp_sald.rds')

# Retirando o warmup
model_3comp_sald$posterior <- posterior::subset_draws(model_3comp_sald$posterior, iter = 14001:16000)

# Olhando o comportamento das cadeias
mcmc_trace(model_3comp_sald$posterior)

# Sobrevivência
preds3Bayesian_sald <- plot_fit_on_data(model_3comp_sald, interval = "credible", data = base_saldamento)

(preds3Bayesian_sald$ggplot +
    theme_bw() +
    scale_x_continuous(breaks = seq(0, 168, 24)) +
    labs(title = "Saldamento - 3 Componentes Bayesiano")) |>
  ggplotly()

# Risco
preds3Bayesian_sald_risco <- plot_fit_on_data(model_3comp_sald, type = "hazard", interval = "credible", data = base_saldamento)

(preds3Bayesian_sald_risco$ggplot +
    theme_bw() +
    scale_x_continuous(breaks = seq(0, 168, 24)) +
    labs(title = "Saldamento - 3 Componentes Bayesiano")) |>
  ggplotly()

# Quatro componentes
model_4comp_sald <- survival_ln_mixture(Surv(tempo_mes, delta2) ~ CC + clusterFC + TC,
                                        data = base_saldamento, 
                                        mixture_components = 4,
                                        iter = 16000,
                                        chains = 4,
                                        cores = 4,
                                        warmup = 0,
                                        em_iter = 1000,
                                        number_em_search = 1000,
                                        show_progress = TRUE,
                                        starting_seed = 400)

mcmc_trace(model_4comp_sald$posterior)

write_rds(model_4comp_sald, '../Modelos/Bay_3/modelo_4comp_sald.rds')
model_4comp_sald <- read_rds('../Modelos/Bay_3/modelo_4comp_sald.rds')

# Retirando o warmup
model_4comp_sald$posterior <- posterior::subset_draws(model_4comp_sald$posterior, iter = 14001:16000)

# Olhando o comportamento das cadeias
mcmc_trace(model_4comp_sald$posterior)

# Sobrevivência
preds4Bayesian_sald <- plot_fit_on_data(model_4comp_sald, interval = "credible", data = base_saldamento)

(preds4Bayesian_sald$ggplot +
    theme_bw() +
    scale_x_continuous(breaks = seq(0, 168, 24)) +
    labs(title = "Saldamento - 4 Componentes Bayesiano")) |>
  ggplotly()

# Risco
preds4Bayesian_sald_risco <- plot_fit_on_data(model_4comp_sald, type = "hazard", interval = "credible", data = base_saldamento)

(preds4Bayesian_sald_risco$ggplot +
    theme_bw() +
    scale_x_continuous(breaks = seq(0, 168, 24)) +
    labs(title = "Saldamento - 4 Componentes Bayesiano")) |>
  ggplotly()

#### Métricas - Bayesiano Saldamento ####
metricas_1comp <- fit_metrics(preds1Bayesian_sald$preds, nobs = nobs(model_1comp_sald)) |> # métricas
  mutate(weighted_value = (n_strata/nobs(model_1comp_sald)) * value) |> # adicionar peso
  group_by(chain, metric) |> # agrupar por cadeia e métrica
  summarise(value = round(sum(weighted_value), 5)) |> # somar para obter a média ponderada
  mutate(mixture_components = factor(1)) # adicionar número de componentes utilizado

metricas_2comp <- fit_metrics(preds2Bayesian_sald$preds, nobs = nobs(model_2comp_sald)) |> # métricas
  mutate(weighted_value = (n_strata/nobs(model_2comp_sald)) * value) |> # adicionar peso
  group_by(chain, metric) |> # agrupar por cadeia e métrica
  summarise(value = round(sum(weighted_value), 5)) |> # somar para obter a média ponderada
  mutate(mixture_components = factor(2)) # adicionar número de componentes utilizado

metricas_3comp <- fit_metrics(preds3Bayesian_sald$preds, nobs = nobs(model_3comp_sald)) |> # métricas
  mutate(weighted_value = (n_strata/nobs(model_3comp_sald)) * value) |> # adicionar peso
  group_by(chain, metric) |> # agrupar por cadeia e métrica
  summarise(value = round(sum(weighted_value), 5)) |> # somar para obter a média ponderada
  mutate(mixture_components = factor(3)) # adicionar número de componentes utilizado

metricas_4comp <- fit_metrics(preds4Bayesian_sald$preds, nobs = nobs(model_4comp_sald)) |> # métricas
  mutate(weighted_value = (n_strata/nobs(model_4comp_sald)) * value) |> # adicionar peso
  group_by(chain, metric) |> # agrupar por cadeia e métrica
  summarise(value = round(sum(weighted_value), 5)) |> # somar para obter a média ponderada
  mutate(mixture_components = factor(4)) # adicionar número de componentes utilizado

# Agrupar métricas
metricas <- bind_rows(metricas_1comp, metricas_2comp, metricas_3comp, metricas_4comp)

# Salvar métricas
write_rds(metricas, '../Modelos/Bay_3/metricas_saldamento.rds')