# Pacotes utilizados
library(tidyverse)
library(arrow)
library(ggsurvfit)
library(plotly)
library(gridExtra)
library(posterior)
library(bayesplot)
library(lnmixsurv)
library(ggpubr)

# Definindo diretório de trabalho na pasta raiz do script ####
dirname(rstudioapi::getSourceEditorContext()$path) |> 
  setwd()

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

##########################################################
####          Base Original (sem filtros)             ####
##########################################################

# Importando base de dados
base <- read_rds('../Bases/capitalizacao.rds')

# Status Item Contratado - Status que foram removidos
table(base$statusItemContratado)

#### Acessando o CONTRATO ANULADO - C16

# Valor Absoluto
length(which(base$statusItemContratado == "CONTRATO ANULADO - C16"))

# Frequência
length(which(base$statusItemContratado == "CONTRATO ANULADO - C16"))/length(base$statusItemContratado) *100

#### Acessando o ENCERRAMENTO APÓLICE - C13

# Valor Absoluto
length(which(base$statusItemContratado == "ENCERRAMENTO APÓLICE - C13"))

# Frequência
length(which(base$statusItemContratado == "ENCERRAMENTO APÓLICE - C13"))/length(base$statusItemContratado) *100

#### Acessando a TRANSFERÊNCIA - C15

# Valor Absoluto
length(which(base$statusItemContratado == "TRANSFERÊNCIA - C15"))

# Frequência
length(which(base$statusItemContratado == "TRANSFERÊNCIA - C15"))/length(base$statusItemContratado) *100

# Status Item Contratado - Status que foram removidos pois eram anteriores a 2010-01-01

### Calculando o valor absoluto de todos os status - são 33 no total - 12 status removidos
table(base$statusItemContratado)

### Calculando quantos contratos tem a data de início da vigência anterior a 2010-01-01
a = as.Date(base$dataInicioVigenciaCobertura) < as.Date("2010-01-01")
length(which(a))

### Contando as remoções dos status e contratos que tem a data de início da vigência anterior a 2010-01-01
### temos um total de 207.485 observações (que foram removidas da base original)

##########################################################
####            Base Tratada - Completa               ####
##########################################################

# Importando base de dados
base <- read_rds('../Bases/capitalizacao_tratada.rds')

# Criando um resumo para ver como cada statusItemContratado foi mapeado para o novo status
resumo_status <- base %>%
  group_by(statusItemContratado, status) %>%
  summarise(contagem = n(), .groups = 'drop') %>%
  arrange(statusItemContratado, status)

################################# Resgate #################################

##########################################################
####           Base Resgate - sem clusterização       ####
##########################################################

#### Importando base de dados ####
base_resgate <- read_parquet('../Bases/base_resgate.parquet')

#### Proporções de censura e cancelamento (resgate e saldamento) - essa informação está na coluna status
ggplot(base_resgate, aes(x = status, y = prop.table(after_stat(count))*100, label = scales::percent(prop.table(after_stat(count)), 0.01))) +
  geom_bar(position = "dodge", fill = "skyblue2", color = "black") +
  geom_text(stat = "count", position = position_dodge(.9), vjust = -0.5, size = 4) +
  scale_y_continuous(n.breaks = 10, limits = c(0, 70)) +
  labs(x = "", y = "") +
  theme_bw()

# Conferindo as porcentagens
(length(which(base_resgate$status == "Saldamento"))/length(base_resgate$status))*100
(length(which(base_resgate$status == "Resgate"))/length(base_resgate$status))*100
(length(which(base_resgate$status == "Censura"))/length(base_resgate$status))*100

#### Proporção por categoria do cliente
ggplot(base_resgate, aes(x = categoriaCliente, y = prop.table(after_stat(count))*100, label = scales::percent(prop.table(after_stat(count)), 0.01))) +
  geom_bar(position = "dodge", fill = "skyblue2", color = "black") +
  geom_text(stat = "count", position = position_dodge(.9), vjust = -0.5, size = 4) +
  scale_y_continuous(n.breaks = 10, limits = c(0, 80)) +
  labs(x = "", y = "") +
  theme_bw()

# Conferindo as porcentagens
(length(which(base_resgate$categoriaCliente == ""))/length(base_resgate$categoriaCliente))*100
(length(which(base_resgate$categoriaCliente == "Cliente MAG"))/length(base_resgate$categoriaCliente))*100
(length(which(base_resgate$categoriaCliente == "MAG Blue"))/length(base_resgate$categoriaCliente))*100

#### Proporção por forma de cobrança
ggplot(base_resgate, aes(x = formaCobranca, y = prop.table(after_stat(count))*100, label = scales::percent(prop.table(after_stat(count)), 0.01))) +
  geom_bar(position = "dodge", fill = "skyblue2", color = "black") +
  geom_text(stat = "count", position = position_dodge(.9), vjust = -0.5, size = 4) +
  scale_y_continuous(n.breaks = 10, limits = c(0, 60)) +
  labs(x = "", y = "") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#### Proporção por tipo de cobertura
ggplot(base_resgate, aes(x = tipoCobertura, y = prop.table(after_stat(count))*100, label = scales::percent(prop.table(after_stat(count)), 0.01))) +
  geom_bar(position = "dodge", fill = "skyblue2", color = "black") +
  geom_text(stat = "count", position = position_dodge(.9), vjust = -0.5, size = 4) +
  scale_y_continuous(n.breaks = 10, limits = c(0, 90)) +
  labs(x = "", y = "") +
  theme_bw()

#### Categoria do Cliente
km_surv <- survfit2(Surv(tempo_mes, delta1) ~ categoriaCliente,
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

plot1 <- ggplot(km_surv) + 
  geom_step(aes(x = time, y = estimate, color = strata), linewidth = 1) +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 168, 24)) +
  ylab("S(t)") +
  xlab("Tempo de casa (em meses)") +
  theme(legend.title = element_blank())

plot2 <- ggplot(pred_risk) +
  geom_line(aes(x = time, y = hazard, color = strata), linewidth = 1) +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 168, 24)) +
  ylab("h(t)") +
  xlab("Tempo de casa (em meses)") +
  theme(legend.title = element_blank())

ggarrange(plot1, plot2, ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom")

#### Forma de Cobrança
km_surv <- survfit2(Surv(tempo_mes, delta1) ~ formaCobranca,
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

plot1 <- ggplot(km_surv) + 
  geom_step(aes(x = time, y = estimate, color = strata), linewidth = 1) +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 168, 24)) +
  theme_bw() +
  ylab("S(t)") +
  xlab("Tempo de casa (em meses)") +
  theme(legend.title = element_blank())

plot2 <- ggplot(pred_risk) +
  geom_line(aes(x = time, y = hazard, color = strata), linewidth = 1) +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 168, 24)) +
  theme_bw() +
  ylab("h(t)") +
  xlab("Tempo de casa (em meses)") +
  theme(legend.title = element_blank())

ggarrange(plot1, plot2, ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom")

#### Tipo de Cobertura
km_surv <- survfit2(Surv(tempo_mes, delta1) ~ tipoCobertura,
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

plot1 <- ggplot(km_surv) + 
  geom_step(aes(x = time, y = estimate, color = strata), linewidth = 1) +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 168, 24)) +
  theme_bw() +
  ylab("S(t)") +
  xlab("Tempo de casa (em meses)") +
  theme(legend.title = element_blank())

plot2 <- ggplot(pred_risk) +
  geom_line(aes(x = time, y = hazard, color = strata), linewidth = 1) +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 168, 24)) +
  theme_bw() +
  ylab("h(t)") +
  xlab("Tempo de casa (em meses)") +
  theme(legend.title = element_blank())

ggarrange(plot1, plot2, ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom")

##########################################################
####    Base Resgate - estudo dos cenários criados    ####
##########################################################

#### Importando base de dados ####
base_resgate <- read_parquet('../Bases/base_resgate_clusters_variaveis.parquet')

# Obter todas as combinações possíveis de clusterCC, clusterFC e clusterTC
todas_combinacoes <- expand.grid(clusterCC = unique(base_resgate$clusterCC),
                                 clusterFC = unique(base_resgate$clusterFC),
                                 clusterTC = unique(base_resgate$clusterTC))

# Contando as combinações presentes na base
combinacoes_presentes <- base_resgate |>
  group_by(clusterCC, clusterFC, clusterTC) |>
  summarise(Contagem = n(), .groups = 'drop')

# Fazer um join para incluir todas as combinações possíveis, preenchendo com zero onde não há combinações
combinacoes_completas <- todas_combinacoes |>
  left_join(combinacoes_presentes, by = c("clusterCC", "clusterFC", "clusterTC")) |>
  mutate(Contagem = replace_na(Contagem, 0)) |>
  arrange(desc(Contagem))

combinacoes_completas

##########################################################
####  Base Resgate - clusterização das covariáveis    ####
##########################################################

#### Importando base de dados ####
base_resgate <- read_parquet('../Bases/base_resgate_clusters.parquet')

# Renomeando os nomes das combinações para por exemplo: 1-1-1 vira CC1-FC1-TC1 para ser mais fácil identificar
base_resgate$combinação <- gsub(
  "(\\d+)-(\\d+)-(\\d+)",  # Padrão a ser encontrado: números separados por hífens
  "CC\\1-FC\\2-TC\\3",     # Novo formato: adiciona CC, FC e TC antes dos números
  base_resgate$combinação
)

km_surv <- survfit2(Surv(tempo_mes, delta1) ~ combinação,
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

ggplot(km_surv) + 
  geom_step(aes(x = time, y = estimate, color = strata), linewidth = 1) +
  scale_x_continuous(breaks = seq(0, 168, 24)) +
  theme_bw() +
  ylab("S(t)") +
  xlab("Tempo de casa (em meses)") +
  theme(legend.position = "bottom", legend.title = element_blank())

ggplot(pred_risk) +
  geom_line(aes(x = time, y = hazard, color = strata), linewidth = 1) +
  scale_x_continuous(breaks = seq(0, 168, 24)) +
  scale_y_continuous(limits = c(0, 0.5)) +
  theme_bw() +
  ylab("h(t)") +
  xlab("Tempo de casa (em meses)") +
  theme(legend.position = "bottom", legend.title = element_blank())

##########################################################
####          Base Resgate - agrupamentos             ####
##########################################################

#### Importando base de dados ####
base_resgate <- read_parquet('../Bases/base_resgate_clusters.parquet')

# Renomeando os nomes das combinações agrupadas para por exemplo: 1-1-1 vira CC1-FC1-TC1 para ser mais fácil identificar
base_resgate$combinação_agrupada <- gsub(
  "(\\d+)-(\\d+)-(\\d+)",  # Padrão a ser encontrado: números separados por hífens
  "CC\\1-FC\\2-TC\\3",     # Novo formato: adiciona CC, FC e TC antes dos números
  base_resgate$combinação_agrupada
)

km_surv <- survfit2(Surv(tempo_mes, delta1) ~ combinação_agrupada,
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

ggplot(km_surv) + 
  geom_step(aes(x = time, y = estimate, color = strata), linewidth = 1) +
  scale_x_continuous(breaks = seq(0, 168, 24)) +
  theme_bw() +
  ylab("S(t)") +
  xlab("Tempo de casa (em meses)") +
  theme(legend.position = "bottom", legend.title = element_blank())

ggplot(pred_risk) +
  geom_line(aes(x = time, y = hazard, color = strata), linewidth = 1) +
  scale_x_continuous(breaks = seq(0, 168, 24)) +
  scale_y_continuous(limits = c(0, 0.2)) +
  theme_bw() +
  ylab("h(t)") +
  xlab("Tempo de casa (em meses)") +
  theme(legend.position = "bottom", legend.title = element_blank())

#### Proporção por combinação agrupada
ggplot(base_resgate, aes(x = combinação_agrupada, y = prop.table(after_stat(count))*100, label = scales::percent(prop.table(after_stat(count)), 0.01))) +
  geom_bar(position = "dodge", fill = "skyblue2", color = "black") +
  geom_text(stat = "count", position = position_dodge(.9), vjust = -0.5, size = 4) +
  scale_y_continuous(n.breaks = 10, limits = c(0, 80)) +
  labs(x = "", y = "") +
  theme_bw()

##########################################################
####               Base Resgate - modelagem           ####
##########################################################

# Modelagem escolhida com base na visualização. 
# As métricas sugerem uma componente/cadeia que visualmente não fica boa,
# então usando o senso crítico escolhemos: 3 componentes, cadeia 1

#### Importando base de dados ####
base_resgate <- read_parquet('../Bases/base_resgate_clusters.parquet')

# Carregando o modelo
model_3comp <- read_rds('../Modelos/Bay_3/modelo_3comp.rds')

# Irei escolher 3 Componentes, cadeia 1
model_3comp$posterior <- posterior::subset_draws(model_3comp$posterior, chain = 1, iter = 14001:16000)

# Olhando o comportamento
mcmc_trace(model_3comp$posterior)

# Sobrevivência
preds3Bayesian <- plot_fit_on_data(model_3comp, data = base_resgate, interval = "credible")

preds3Bayesian$ggplot +
  scale_x_continuous(breaks = seq(0, 168, 24)) +
  theme_bw() +
  ylab("S(t)") +
  xlab("Tempo de casa (em meses)") +
  theme(legend.position = "bottom", legend.title = element_blank())

# Risco
preds3Bayesian_risco <- plot_fit_on_data(model_3comp, data = base_resgate, type = "hazard", interval = "credible")

preds3Bayesian_risco$ggplot +
    scale_x_continuous(breaks = seq(0, 168, 24)) +
    theme_bw() +
    ylab("h(t)") +
    xlab("Tempo de casa (em meses)") +
    theme(legend.position = "bottom", legend.title = element_blank())

#####################################################################
####             Estudando os Cenários - Resgate                 ####
#####################################################################

# Olhando a sobrevivência de cada agrupamento separadamente:
data0 <- preds3Bayesian$preds

# Olhando o risco de cada agrupamento separadamente:
data <- preds3Bayesian_risco$preds

## Cenário 1: clusterCC=1, clusterFC=1, clusterTC=1

# Sobrevivência
filtro0 <- data0 |>
  filter(strata == "clusterCC=1, clusterFC=1, clusterTC=1")

plot1 <- ggplot(filtro0, aes(x = time)) +
  geom_step(aes(y = estimate, linetype = "KM Cenário 1"), linewidth = 1) +
  geom_line(aes(y = .pred_survival, color = "MSMLN Cenário 1"), linewidth = 1) +
  geom_ribbon(aes(ymin = .pred_lower, ymax = .pred_upper, fill = "IC 95%"), alpha = 0.5) +
  scale_linetype_manual(values = c("KM Cenário 1" = "solid"), guide = guide_legend(order = 1)) +
  scale_color_manual(values = c("MSMLN Cenário 1" = "blue"), guide = guide_legend(order = 2)) +
  scale_fill_manual(values = c("IC 95%" = "skyblue2"), guide = guide_legend(order = 3)) +
  scale_x_continuous(breaks = seq(0, 168, 24), limits = c(0, 168)) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_bw() +
  ylab("S(t)") +
  xlab("Tempo de casa (em meses)") +
  theme(legend.position = "bottom", legend.title = element_blank())

# Risco
filtro1 <- data |>
  filter(strata == "clusterCC=1, clusterFC=1, clusterTC=1")

plot2 <- ggplot(filtro1, aes(x = time)) +
  geom_line(aes(y = hazard_estimate, linetype = "Empírica Cenário 1"), linewidth = 1) +
  geom_line(aes(y = .pred_hazard, color = "Ajustada Cenário 1"), linewidth = 1) +
  geom_ribbon(aes(ymin = .pred_lower, ymax = .pred_upper, fill = "IC 95%"), alpha = 0.5) +
  scale_linetype_manual(values = c("Empírica Cenário 1" = "solid"), guide = guide_legend(order = 1)) +
  scale_color_manual(values = c("Ajustada Cenário 1" = "blue"), guide = guide_legend(order = 2)) +
  scale_fill_manual(values = c("IC 95%" = "skyblue2"), guide = guide_legend(order = 3)) +
  scale_x_continuous(breaks = seq(0, 168, 24), limits = c(0, 168)) +
  scale_y_continuous(limits = c(0, 0.25)) +
  theme_bw() +
  ylab("h(t)") +
  xlab("Tempo de casa (em meses)") +
  theme(legend.position = "bottom", legend.title = element_blank())

ggarrange(plot1, plot2, ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom")

## Cenário 2: clusterCC=1, clusterFC=1, clusterTC=2

# Sobrevivência
filtro0 <- data0 |>
  filter(strata == "clusterCC=1, clusterFC=1, clusterTC=2")

plot1 <- ggplot(filtro0, aes(x = time)) +
  geom_step(aes(y = estimate, linetype = "KM Cenário 2"), linewidth = 1) +
  geom_line(aes(y = .pred_survival, color = "MSMLN Cenário 2"), linewidth = 1) +
  geom_ribbon(aes(ymin = .pred_lower, ymax = .pred_upper, fill = "IC 95%"), alpha = 0.5) +
  scale_linetype_manual(values = c("KM Cenário 2" = "solid"), guide = guide_legend(order = 1)) +
  scale_color_manual(values = c("MSMLN Cenário 2" = "blue"), guide = guide_legend(order = 2)) +
  scale_fill_manual(values = c("IC 95%" = "skyblue2"), guide = guide_legend(order = 3)) +
  scale_x_continuous(breaks = seq(0, 168, 24), limits = c(0, 168)) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_bw() +
  ylab("S(t)") +
  xlab("Tempo de casa (em meses)") +
  theme(legend.position = "bottom", legend.title = element_blank())

# Risco
filtro2 <- data |>
  filter(strata == "clusterCC=1, clusterFC=1, clusterTC=2")

plot2 <- ggplot(filtro2, aes(x = time)) +
  geom_line(aes(y = hazard_estimate, linetype = "Empírica Cenário 2"), linewidth = 1) +
  geom_line(aes(y = .pred_hazard, color = "Ajustada Cenário 2"), linewidth = 1) +
  geom_ribbon(aes(ymin = .pred_lower, ymax = .pred_upper, fill = "IC 95%"), alpha = 0.5) +
  scale_linetype_manual(values = c("Empírica Cenário 2" = "solid"), guide = guide_legend(order = 1)) +
  scale_color_manual(values = c("Ajustada Cenário 2" = "blue"), guide = guide_legend(order = 2)) +
  scale_fill_manual(values = c("IC 95%" = "skyblue2"), guide = guide_legend(order = 3)) +
  scale_x_continuous(breaks = seq(0, 168, 24), limits = c(0, 168)) +
  scale_y_continuous(limits = c(0, 0.25)) +
  theme_bw() +
  ylab("h(t)") +
  xlab("Tempo de casa (em meses)") +
  theme(legend.position = "bottom", legend.title = element_blank())

ggarrange(plot1, plot2, ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom")

## Cenário 3: clusterCC=1, clusterFC=2, clusterTC=1

# Sobrevivência
filtro0 <- data0 |>
  filter(strata == "clusterCC=1, clusterFC=2, clusterTC=1")

plot1<- ggplot(filtro0, aes(x = time)) +
  geom_step(aes(y = estimate, linetype = "KM Cenário 3"), linewidth = 1) +
  geom_line(aes(y = .pred_survival, color = "MSMLN Cenário 3"), linewidth = 1) +
  geom_ribbon(aes(ymin = .pred_lower, ymax = .pred_upper, fill = "IC 95%"), alpha = 0.5) +
  scale_linetype_manual(values = c("KM Cenário 3" = "solid"), guide = guide_legend(order = 1)) +
  scale_color_manual(values = c("MSMLN Cenário 3" = "blue"), guide = guide_legend(order = 2)) +
  scale_fill_manual(values = c("IC 95%" = "skyblue2"), guide = guide_legend(order = 3)) +
  scale_x_continuous(breaks = seq(0, 168, 24), limits = c(0, 168)) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_bw() +
  ylab("S(t)") +
  xlab("Tempo de casa (em meses)") +
  theme(legend.position = "bottom", legend.title = element_blank())

# Risco
filtro3 <- data |>
  filter(strata == "clusterCC=1, clusterFC=2, clusterTC=1")

plot2 <- ggplot(filtro3, aes(x = time)) +
  geom_line(aes(y = hazard_estimate, linetype = "Empírica Cenário 3"), linewidth = 1) +
  geom_line(aes(y = .pred_hazard, color = "Ajustada Cenário 3"), linewidth = 1) +
  geom_ribbon(aes(ymin = .pred_lower, ymax = .pred_upper, fill = "IC 95%"), alpha = 0.5) +
  scale_linetype_manual(values = c("Empírica Cenário 3" = "solid"), guide = guide_legend(order = 1)) +
  scale_color_manual(values = c("Ajustada Cenário 3" = "blue"), guide = guide_legend(order = 2)) +
  scale_fill_manual(values = c("IC 95%" = "skyblue2"), guide = guide_legend(order = 3)) +
  scale_x_continuous(breaks = seq(0, 168, 24), limits = c(0, 168)) +
  scale_y_continuous(limits = c(0, 0.25)) +
  theme_bw() +
  ylab("h(t)") +
  xlab("Tempo de casa (em meses)") +
  theme(legend.position = "bottom", legend.title = element_blank())

ggarrange(plot1, plot2, ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom")

## Cenário 4: clusterCC=2, clusterFC=1, clusterTC=1

# Sobrevivência
filtro0 <- data0 |>
  filter(strata == "clusterCC=2, clusterFC=1, clusterTC=1")

plot1 <- ggplot(filtro0, aes(x = time)) +
  geom_step(aes(y = estimate, linetype = "KM Cenário 4"), linewidth = 1) +
  geom_line(aes(y = .pred_survival, color = "MSMLN Cenário 4"), linewidth = 1) +
  geom_ribbon(aes(ymin = .pred_lower, ymax = .pred_upper, fill = "IC 95%"), alpha = 0.5) +
  scale_linetype_manual(values = c("KM Cenário 4" = "solid"), guide = guide_legend(order = 1)) +
  scale_color_manual(values = c("MSMLN Cenário 4" = "blue"), guide = guide_legend(order = 2)) +
  scale_fill_manual(values = c("IC 95%" = "skyblue2"), guide = guide_legend(order = 3)) +
  scale_x_continuous(breaks = seq(0, 168, 24), limits = c(0, 168)) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_bw() +
  ylab("S(t)") +
  xlab("Tempo de casa (em meses)") +
  theme(legend.position = "bottom", legend.title = element_blank())

# Risco
filtro4 <- data |>
  filter(strata == "clusterCC=2, clusterFC=1, clusterTC=1")

plot2 <- ggplot(filtro4, aes(x = time)) +
  geom_line(aes(y = hazard_estimate, linetype = "Empírica Cenário 4"), linewidth = 1) +
  geom_line(aes(y = .pred_hazard, color = "Ajustada Cenário 4"), linewidth = 1) +
  geom_ribbon(aes(ymin = .pred_lower, ymax = .pred_upper, fill = "IC 95%"), alpha = 0.5) +
  scale_linetype_manual(values = c("Empírica Cenário 4" = "solid"), guide = guide_legend(order = 1)) +
  scale_color_manual(values = c("Ajustada Cenário 4" = "blue"), guide = guide_legend(order = 2)) +
  scale_fill_manual(values = c("IC 95%" = "skyblue2"), guide = guide_legend(order = 3)) +
  scale_x_continuous(breaks = seq(0, 168, 24), limits = c(0, 168)) +
  scale_y_continuous(limits = c(0, 0.25)) +
  theme_bw() +
  ylab("h(t)") +
  xlab("Tempo de casa (em meses)") +
  theme(legend.position = "bottom", legend.title = element_blank())

ggarrange(plot1, plot2, ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom")

## Cenário 5: clusterCC=2, clusterFC=2, clusterTC=1

# Sobrevivência
filtro0 <- data0 |>
  filter(strata == "clusterCC=2, clusterFC=2, clusterTC=1")

plot1 <- ggplot(filtro0, aes(x = time)) +
  geom_step(aes(y = estimate, linetype = "KM Cenário 5"), linewidth = 1) +
  geom_line(aes(y = .pred_survival, color = "MSMLN Cenário 5"), linewidth = 1) +
  geom_ribbon(aes(ymin = .pred_lower, ymax = .pred_upper, fill = "IC 95%"), alpha = 0.5) +
  scale_linetype_manual(values = c("KM Cenário 5" = "solid"), guide = guide_legend(order = 1)) +
  scale_color_manual(values = c("MSMLN Cenário 5" = "blue"), guide = guide_legend(order = 2)) +
  scale_fill_manual(values = c("IC 95%" = "skyblue2"), guide = guide_legend(order = 3)) +
  scale_x_continuous(breaks = seq(0, 168, 24), limits = c(0, 168)) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_bw() +
  ylab("S(t)") +
  xlab("Tempo de casa (em meses)") +
  theme(legend.position = "bottom", legend.title = element_blank())

# Risco
filtro5 <- data |>
  filter(strata == "clusterCC=2, clusterFC=2, clusterTC=1")

plot2 <- ggplot(filtro5, aes(x = time)) +
  geom_line(aes(y = hazard_estimate, linetype = "Empírica Cenário 5"), linewidth = 1) +
  geom_line(aes(y = .pred_hazard, color = "Ajustada Cenário 5"), linewidth = 1) +
  geom_ribbon(aes(ymin = .pred_lower, ymax = .pred_upper, fill = "IC 95%"), alpha = 0.5) +
  scale_linetype_manual(values = c("Empírica Cenário 5" = "solid"), guide = guide_legend(order = 1)) +
  scale_color_manual(values = c("Ajustada Cenário 5" = "blue"), guide = guide_legend(order = 2)) +
  scale_fill_manual(values = c("IC 95%" = "skyblue2"), guide = guide_legend(order = 3)) +
  scale_x_continuous(breaks = seq(0, 168, 24), limits = c(0, 168)) +
  scale_y_continuous(limits = c(0, 0.25)) +
  theme_bw() +
  ylab("h(t)") +
  xlab("Tempo de casa (em meses)") +
  theme(legend.position = "bottom", legend.title = element_blank())

ggarrange(plot1, plot2, ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom")

## Cenário 6: clusterCC=5, clusterFC=5, clusterTC=5

# Sobrevivência
filtro0 <- data0 |>
  filter(strata == "clusterCC=5, clusterFC=5, clusterTC=5")

plot1 <- ggplot(filtro0, aes(x = time)) +
  geom_step(aes(y = estimate, linetype = "KM Cenário 6"), linewidth = 1) +
  geom_line(aes(y = .pred_survival, color = "MSMLN Cenário 6"), linewidth = 1) +
  geom_ribbon(aes(ymin = .pred_lower, ymax = .pred_upper, fill = "IC 95%"), alpha = 0.5) +
  scale_linetype_manual(values = c("KM Cenário 6" = "solid"), guide = guide_legend(order = 1)) +
  scale_color_manual(values = c("MSMLN Cenário 6" = "blue"), guide = guide_legend(order = 2)) +
  scale_fill_manual(values = c("IC 95%" = "skyblue2"), guide = guide_legend(order = 3)) +
  scale_x_continuous(breaks = seq(0, 168, 24), limits = c(0, 168)) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_bw() +
  ylab("S(t)") +
  xlab("Tempo de casa (em meses)") +
  theme(legend.position = "bottom", legend.title = element_blank())

# Risco
filtro6 <- data |>
  filter(strata == "clusterCC=5, clusterFC=5, clusterTC=5")

plot2 <- ggplot(filtro6, aes(x = time)) +
  geom_line(aes(y = hazard_estimate, linetype = "Empírica Cenário 6"), linewidth = 1) +
  geom_line(aes(y = .pred_hazard, color = "Ajustada Cenário 6"), linewidth = 1) +
  geom_ribbon(aes(ymin = .pred_lower, ymax = .pred_upper, fill = "IC 95%"), alpha = 0.5) +
  scale_linetype_manual(values = c("Empírica Cenário 6" = "solid"), guide = guide_legend(order = 1)) +
  scale_color_manual(values = c("Ajustada Cenário 6" = "blue"), guide = guide_legend(order = 2)) +
  scale_fill_manual(values = c("IC 95%" = "skyblue2"), guide = guide_legend(order = 3)) +
  scale_x_continuous(breaks = seq(0, 168, 24), limits = c(0, 168)) +
  scale_y_continuous(limits = c(0, 0.25)) +
  theme_bw() +
  ylab("h(t)") +
  xlab("Tempo de casa (em meses)") +
  theme(legend.position = "bottom", legend.title = element_blank())

ggarrange(plot1, plot2, ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom")

## Cenário 7: clusterCC=6, clusterFC=6, clusterTC=6

# Sobrevivência
filtro0 <- data0 |>
  filter(strata == "clusterCC=6, clusterFC=6, clusterTC=6")

plot1 <- ggplot(filtro0, aes(x = time)) +
  geom_step(aes(y = estimate, linetype = "KM Cenário 7"), linewidth = 1) +
  geom_line(aes(y = .pred_survival, color = "MSMLN Cenário 7"), linewidth = 1) +
  geom_ribbon(aes(ymin = .pred_lower, ymax = .pred_upper, fill = "IC 95%"), alpha = 0.5) +
  scale_linetype_manual(values = c("KM Cenário 7" = "solid"), guide = guide_legend(order = 1)) +
  scale_color_manual(values = c("MSMLN Cenário 7" = "blue"), guide = guide_legend(order = 2)) +
  scale_fill_manual(values = c("IC 95%" = "skyblue2"), guide = guide_legend(order = 3)) +
  scale_x_continuous(breaks = seq(0, 168, 24), limits = c(0, 168)) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_bw() +
  ylab("S(t)") +
  xlab("Tempo de casa (em meses)") +
  theme(legend.position = "bottom", legend.title = element_blank())

# Risco
filtro7 <- data |>
  filter(strata == "clusterCC=6, clusterFC=6, clusterTC=6")

plot2 <- ggplot(filtro7, aes(x = time)) +
  geom_line(aes(y = hazard_estimate, linetype = "Empírica Cenário 7"), linewidth = 1) +
  geom_line(aes(y = .pred_hazard, color = "Ajustada Cenário 7"), linewidth = 1) +
  geom_ribbon(aes(ymin = .pred_lower, ymax = .pred_upper, fill = "IC 95%"), alpha = 0.5) +
  scale_linetype_manual(values = c("Empírica Cenário 7" = "solid"), guide = guide_legend(order = 1)) +
  scale_color_manual(values = c("Ajustada Cenário 7" = "blue"), guide = guide_legend(order = 2)) +
  scale_fill_manual(values = c("IC 95%" = "skyblue2"), guide = guide_legend(order = 3)) +
  scale_x_continuous(breaks = seq(0, 168, 24), limits = c(0, 168)) +
  scale_y_continuous(limits = c(0, 0.25)) +
  theme_bw() +
  ylab("h(t)") +
  xlab("Tempo de casa (em meses)") +
  theme(legend.position = "bottom", legend.title = element_blank())

ggarrange(plot1, plot2, ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom")

## Cenário 8: clusterCC=7, clusterFC=7, clusterTC=7

# Sobrevivência
filtro0 <- data0 |>
  filter(strata == "clusterCC=7, clusterFC=7, clusterTC=7")

plot1 <- ggplot(filtro0, aes(x = time)) +
  geom_step(aes(y = estimate, linetype = "KM Cenário 8"), linewidth = 1) +
  geom_line(aes(y = .pred_survival, color = "MSMLN Cenário 8"), linewidth = 1) +
  geom_ribbon(aes(ymin = .pred_lower, ymax = .pred_upper, fill = "IC 95%"), alpha = 0.5) +
  scale_linetype_manual(values = c("KM Cenário 8" = "solid"), guide = guide_legend(order = 1)) +
  scale_color_manual(values = c("MSMLN Cenário 8" = "blue"), guide = guide_legend(order = 2)) +
  scale_fill_manual(values = c("IC 95%" = "skyblue2"), guide = guide_legend(order = 3)) +
  scale_x_continuous(breaks = seq(0, 168, 24), limits = c(0, 168)) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_bw() +
  ylab("S(t)") +
  xlab("Tempo de casa (em meses)") +
  theme(legend.position = "bottom", legend.title = element_blank())

# Risco
filtro8 <- data |>
  filter(strata == "clusterCC=7, clusterFC=7, clusterTC=7")

plot2 <- ggplot(filtro8, aes(x = time)) +
  geom_line(aes(y = hazard_estimate, linetype = "Empírica Cenário 8"), linewidth = 1) +
  geom_line(aes(y = .pred_hazard, color = "Ajustada Cenário 8"), linewidth = 1) +
  geom_ribbon(aes(ymin = .pred_lower, ymax = .pred_upper, fill = "IC 95%"), alpha = 0.5) +
  scale_linetype_manual(values = c("Empírica Cenário 8" = "solid"), guide = guide_legend(order = 1)) +
  scale_color_manual(values = c("Ajustada Cenário 8" = "blue"), guide = guide_legend(order = 2)) +
  scale_fill_manual(values = c("IC 95%" = "skyblue2"), guide = guide_legend(order = 3)) +
  scale_x_continuous(breaks = seq(0, 168, 24), limits = c(0, 168)) +
  scale_y_continuous(limits = c(0, 0.25)) +
  theme_bw() +
  ylab("h(t)") +
  xlab("Tempo de casa (em meses)") +
  theme(legend.position = "bottom", legend.title = element_blank())

ggarrange(plot1, plot2, ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom")

#################################### Só os cenários (sem as KM) ################################

# Sobrevivência
plot1 <- ggplot(preds3Bayesian$preds) +
  geom_line(aes(x = time, y = .pred_survival, color = strata), linewidth = 1.2) +
  scale_x_continuous(breaks = seq(0, 168, 24), limits = c(0, 168)) +
  scale_y_continuous(limits = c(0, 1))  +
  scale_color_manual(name = "Cenário",
    values = c("clusterCC=1, clusterFC=1, clusterTC=1" = "red",
               "clusterCC=1, clusterFC=1, clusterTC=2" = "blue",
               "clusterCC=1, clusterFC=2, clusterTC=1" = "green",
               "clusterCC=2, clusterFC=1, clusterTC=1" = "purple",
               "clusterCC=2, clusterFC=2, clusterTC=1" = "orange",
               "clusterCC=5, clusterFC=5, clusterTC=5" = "brown",
               "clusterCC=6, clusterFC=6, clusterTC=6" = "pink",
               "clusterCC=7, clusterFC=7, clusterTC=7" = "cyan"),
    labels = c("clusterCC=1, clusterFC=1, clusterTC=1" = "Cenário 1",
               "clusterCC=1, clusterFC=1, clusterTC=2" = "Cenário 2",
               "clusterCC=1, clusterFC=2, clusterTC=1" = "Cenário 3",
               "clusterCC=2, clusterFC=1, clusterTC=1" = "Cenário 4",
               "clusterCC=2, clusterFC=2, clusterTC=1" = "Cenário 5",
               "clusterCC=5, clusterFC=5, clusterTC=5" = "Cenário 6",
               "clusterCC=6, clusterFC=6, clusterTC=6" = "Cenário 7",
               "clusterCC=7, clusterFC=7, clusterTC=7" = "Cenário 8")) +
  theme_bw() +
  ylab("S(t)") +
  xlab("Tempo de casa (em meses)") +
  theme(legend.position = "bottom", legend.title = element_blank())

# Risco
plot2 <- ggplot(preds3Bayesian_risco$preds) +
  geom_line(aes(x = time, y = .pred_hazard, color = strata), linewidth = 1.2) +
  scale_x_continuous(breaks = seq(0, 168, 24), limits = c(0, 168)) +
  scale_y_continuous(limits = c(0, 0.25))  +
  scale_color_manual(name = "Cenário",
                     values = c("clusterCC=1, clusterFC=1, clusterTC=1" = "red",
                                "clusterCC=1, clusterFC=1, clusterTC=2" = "blue",
                                "clusterCC=1, clusterFC=2, clusterTC=1" = "green",
                                "clusterCC=2, clusterFC=1, clusterTC=1" = "purple",
                                "clusterCC=2, clusterFC=2, clusterTC=1" = "orange",
                                "clusterCC=5, clusterFC=5, clusterTC=5" = "brown",
                                "clusterCC=6, clusterFC=6, clusterTC=6" = "pink",
                                "clusterCC=7, clusterFC=7, clusterTC=7" = "cyan"),
                     labels = c("clusterCC=1, clusterFC=1, clusterTC=1" = "Cenário 1",
                                "clusterCC=1, clusterFC=1, clusterTC=2" = "Cenário 2",
                                "clusterCC=1, clusterFC=2, clusterTC=1" = "Cenário 3",
                                "clusterCC=2, clusterFC=1, clusterTC=1" = "Cenário 4",
                                "clusterCC=2, clusterFC=2, clusterTC=1" = "Cenário 5",
                                "clusterCC=5, clusterFC=5, clusterTC=5" = "Cenário 6",
                                "clusterCC=6, clusterFC=6, clusterTC=6" = "Cenário 7",
                                "clusterCC=7, clusterFC=7, clusterTC=7" = "Cenário 8")) +
  theme_bw() +
  ylab("h(t)") +
  xlab("Tempo de casa (em meses)") +
  theme(legend.position = "bottom", legend.title = element_blank())

ggarrange(plot1, plot2, ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom")

################################ Comparando Cenário 1 x Cenário 3 ###############################

## Cenário 1: clusterCC=1, clusterFC=1, clusterTC=1
## Cenário 3: clusterCC=1, clusterFC=2, clusterTC=1

# Sobrevivência
filtroc13 <- data0 |>
  filter(strata %in% c("clusterCC=1, clusterFC=1, clusterTC=1",
                       "clusterCC=1, clusterFC=2, clusterTC=1"))

plot1 <- ggplot(filtroc13) +
  geom_line(aes(x = time, y = .pred_survival, color = strata), linewidth = 1.2) +
  scale_x_continuous(breaks = seq(0, 168, 24), limits = c(0, 168)) +
  scale_y_continuous(limits = c(0, 1))  +
  scale_color_manual(name = "Cenário",
                     values = c("clusterCC=1, clusterFC=1, clusterTC=1" = "red",
                                "clusterCC=1, clusterFC=2, clusterTC=1" = "green"),
                     labels = c("clusterCC=1, clusterFC=1, clusterTC=1" = "Cenário 1",
                                "clusterCC=1, clusterFC=2, clusterTC=1" = "Cenário 3")) +
  theme_bw() +
  ylab("S(t)") +
  xlab("Tempo de casa (em meses)") +
  theme(legend.position = "bottom", legend.title = element_blank())

# Risco
filtroc13_risco <- data |>
  filter(strata %in% c("clusterCC=1, clusterFC=1, clusterTC=1",
                       "clusterCC=1, clusterFC=2, clusterTC=1"))

plot2 <- ggplot(filtroc13_risco) +
  geom_line(aes(x = time, y = .pred_hazard, color = strata), linewidth = 1.2) +
  scale_x_continuous(breaks = seq(0, 168, 24), limits = c(0, 168)) +
  scale_y_continuous(limits = c(0, 0.25))  +
  scale_color_manual(name = "Cenário",
                     values = c("clusterCC=1, clusterFC=1, clusterTC=1" = "red",
                                "clusterCC=1, clusterFC=2, clusterTC=1" = "green"),
                     labels = c("clusterCC=1, clusterFC=1, clusterTC=1" = "Cenário 1",
                                "clusterCC=1, clusterFC=2, clusterTC=1" = "Cenário 3")) +
  theme_bw() +
  ylab("h(t)") +
  xlab("Tempo de casa (em meses)") +
  theme(legend.position = "bottom", legend.title = element_blank())

ggarrange(plot1, plot2, ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom")

################################ Comparando Cenário 1 x Cenário 2 ###############################

## Cenário 1: clusterCC=1, clusterFC=1, clusterTC=1
## Cenário 2: clusterCC=1, clusterFC=1, clusterTC=2

# Sobrevivência
filtroc12 <- data0 |>
  filter(strata %in% c("clusterCC=1, clusterFC=1, clusterTC=1",
                       "clusterCC=1, clusterFC=1, clusterTC=2"))

plot1 <- ggplot(filtroc12) +
  geom_line(aes(x = time, y = .pred_survival, color = strata), linewidth = 1.2) +
  scale_x_continuous(breaks = seq(0, 168, 24), limits = c(0, 168)) +
  scale_y_continuous(limits = c(0, 1))  +
  scale_color_manual(name = "Cenário",
                     values = c("clusterCC=1, clusterFC=1, clusterTC=1" = "red",
                                "clusterCC=1, clusterFC=1, clusterTC=2" = "blue"),
                     labels = c("clusterCC=1, clusterFC=1, clusterTC=1" = "Cenário 1",
                                "clusterCC=1, clusterFC=1, clusterTC=2" = "Cenário 2")) +
  theme_bw() +
  ylab("S(t)") +
  xlab("Tempo de casa (em meses)") +
  theme(legend.position = "bottom", legend.title = element_blank())

# Risco
filtroc12_risco <- data |>
  filter(strata %in% c("clusterCC=1, clusterFC=1, clusterTC=1",
                       "clusterCC=1, clusterFC=1, clusterTC=2"))

plot2 <- ggplot(filtroc12_risco) +
  geom_line(aes(x = time, y = .pred_hazard, color = strata), linewidth = 1.2) +
  scale_x_continuous(breaks = seq(0, 168, 24), limits = c(0, 168)) +
  scale_y_continuous(limits = c(0, 0.25))  +
  scale_color_manual(name = "Cenário",
                     values = c("clusterCC=1, clusterFC=1, clusterTC=1" = "red",
                                "clusterCC=1, clusterFC=1, clusterTC=2" = "blue"),
                     labels = c("clusterCC=1, clusterFC=1, clusterTC=1" = "Cenário 1",
                                "clusterCC=1, clusterFC=1, clusterTC=2" = "Cenário 2")) +
  theme_bw() +
  ylab("h(t)") +
  xlab("Tempo de casa (em meses)") +
  theme(legend.position = "bottom", legend.title = element_blank())

ggarrange(plot1, plot2, ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom")

##########################################################
####    Base Resgate - Parâmetros para o Relatório    ####
##########################################################

# Carregando o modelo
model_3comp <- read_rds('../Modelos/Bay_3/modelo_3comp.rds')

# Irei escolher 3 Componentes, cadeia 1
model_3comp$posterior <- posterior::subset_draws(model_3comp$posterior, chain = 1, iter = 14001:16000)

# Olhando os parâmetros
model_3comp

# Dado que o pacote lnmixsurv não traz a estimativa e IC para a 
# última componente de mistura, irei calcular

model_3comp$posterior <- model_3comp$posterior |> 
  as_tibble() |> 
  mutate(eta_3 = 1 - (eta_1 + eta_2)) |> 
  posterior::as_draws()

# Intervalo para os etas

model_3comp$posterior |> 
  posterior::subset_draws(variable = 'eta', regex = TRUE) |> 
  as_tibble() |> 
  pivot_longer(cols = starts_with('eta'), names_to = 'eta', values_to = 'value') |>
  group_by(eta) |> 
  summarise(mediana = median(value),
            ic_025 = quantile(value, 0.025),
            ic_975 = quantile(value, 0.975)) |> 
  ungroup() |> 
  group_by(eta) |> 
  summarise(across(where(is.numeric), ~ round(., 3)))

##########################################################
####            Base Resgate - Previsões              ####
##########################################################

#### Importando base de dados ####
base_resgate <- read_parquet('../Bases/base_resgate_clusters.parquet')

# Carregando o modelo
model_3comp <- read_rds('../Modelos/Bay_3/modelo_3comp.rds')

# Irei escolher 3 Componentes, cadeia 1
model_3comp$posterior <- posterior::subset_draws(model_3comp$posterior, chain = 1, iter = 14001:16000)

# Sobrevivência
preds3Bayesian <- plot_fit_on_data(model_3comp, data = base_resgate, interval = "credible")

previsoes_sobrevivencia_resgate <- preds3Bayesian$preds |>
  select(time, strata, .pred_survival)

# Salvar o dataframe 'previsoes_sobrevivencia_resgate' como um arquivo CSV
write.csv(previsoes_sobrevivencia_resgate, "previsoes_sobrevivencia_resgate.csv", row.names = FALSE)

# Risco
preds3Bayesian_risco <- plot_fit_on_data(model_3comp, data = base_resgate, type = "hazard", interval = "credible")

previsoes_risco_resgate <- preds3Bayesian_risco$preds |>
  select(time, strata, .pred_hazard)

# Salvar o dataframe 'previsoes_risco_resgate' como um arquivo CSV
write.csv(previsoes_risco_resgate, "previsoes_risco_resgate.csv", row.names = FALSE)

##########################################################
####     Base Resgate - Mapa da Clusterização         ####
##########################################################

#### Importando base de dados ####
base_resgate <- read_parquet('../Bases/base_resgate_clusters.parquet')

# Criando o mapa, removendo duplicatas e separando a coluna Cluster em três novas colunas
mapa_resgate <- base_resgate |>
  select(cluster = combinação_agrupada, categoriaCliente, formaCobranca, tipoCobertura) |>  # Seleciona as colunas
  distinct() |> # Remove duplicatas
  separate(cluster, into = c("clusterCC", "clusterFC", "clusterTC"), sep = "-", remove = FALSE) |> # Separa a coluna Cluster
  select(cluster, clusterCC, categoriaCliente, clusterFC, formaCobranca, clusterTC, tipoCobertura) |> # Reorganiza as colunas
  select(-cluster)

# Salvar o dataframe 'mapa_resgate' como um arquivo CSV
write.csv(mapa_resgate, "mapa_resgate.csv", row.names = FALSE)

################################# Saldamento #################################

##########################################################
####        Base Saldamento - sem clusterização       ####
##########################################################

#### Importando base de dados ####
base_saldamento <- read_parquet('../Bases/base_saldamento.parquet')

#### Proporções de censura e cancelamento (resgate e saldamento) - essa informação está na coluna status
ggplot(base_saldamento, aes(x = status, y = prop.table(after_stat(count))*100, label = scales::percent(prop.table(after_stat(count)), 0.01))) +
  geom_bar(position = "dodge", fill = "skyblue2", color = "black") +
  geom_text(stat = "count", position = position_dodge(.9), vjust = -0.5, size = 4) +
  scale_y_continuous(n.breaks = 10, limits = c(0, 60)) +
  labs(x = "", y = "") +
  theme_bw()

#### Proporção por categoria do cliente
ggplot(base_saldamento, aes(x = categoriaCliente, y = prop.table(after_stat(count))*100, label = scales::percent(prop.table(after_stat(count)), 0.01))) +
  geom_bar(position = "dodge", fill = "skyblue2", color = "black") +
  geom_text(stat = "count", position = position_dodge(.9), vjust = -0.5, size = 4) +
  scale_y_continuous(n.breaks = 10, limits = c(0, 70)) +
  labs(x = "", y = "") +
  theme_bw()

#### Proporção por forma de cobrança
ggplot(base_saldamento, aes(x = formaCobranca, y = prop.table(after_stat(count))*100, label = scales::percent(prop.table(after_stat(count)), 0.01))) +
  geom_bar(position = "dodge", fill = "skyblue2", color = "black") +
  geom_text(stat = "count", position = position_dodge(.9), vjust = -0.5, size = 4) +
  scale_y_continuous(n.breaks = 10, limits = c(0, 60)) +
  labs(x = "", y = "") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#### Proporção por tipo de cobertura
ggplot(base_saldamento, aes(x = tipoCobertura, y = prop.table(after_stat(count))*100, label = scales::percent(prop.table(after_stat(count)), 0.01))) +
  geom_bar(position = "dodge", fill = "skyblue2", color = "black") +
  geom_text(stat = "count", position = position_dodge(.9), vjust = -0.5, size = 4) +
  scale_y_continuous(n.breaks = 10, limits = c(0, 100)) +
  labs(x = "", y = "") +
  theme_bw()

#### Categoria do Cliente
km_surv <- survfit2(Surv(tempo_mes, delta2) ~ categoriaCliente,
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

plot1 <- ggplot(km_surv) + 
  geom_step(aes(x = time, y = estimate, color = strata), linewidth = 1) +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 168, 24)) +
  ylab("S(t)") +
  xlab("Tempo de casa (em meses)") +
  theme(legend.title = element_blank())

plot2 <- ggplot(pred_risk) +
  geom_line(aes(x = time, y = hazard, color = strata), linewidth = 1) +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 168, 24)) +
  ylab("h(t)") +
  xlab("Tempo de casa (em meses)") +
  theme(legend.title = element_blank())

ggarrange(plot1, plot2, ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom")

#### Forma de Cobrança
km_surv <- survfit2(Surv(tempo_mes, delta2) ~ formaCobranca,
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

plot1 <- ggplot(km_surv) + 
  geom_step(aes(x = time, y = estimate, color = strata), linewidth = 1) +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 168, 24)) +
  theme_bw() +
  ylab("S(t)") +
  xlab("Tempo de casa (em meses)") +
  theme(legend.title = element_blank())

plot2 <- ggplot(pred_risk) +
  geom_line(aes(x = time, y = hazard, color = strata), linewidth = 1) +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 168, 24)) +
  theme_bw() +
  ylab("h(t)") +
  xlab("Tempo de casa (em meses)") +
  theme(legend.title = element_blank())

ggarrange(plot1, plot2, ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom")

#### Tipo de Cobertura
km_surv <- survfit2(Surv(tempo_mes, delta2) ~ tipoCobertura,
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

plot1 <- ggplot(km_surv) + 
  geom_step(aes(x = time, y = estimate, color = strata), linewidth = 1) +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 168, 24)) +
  theme_bw() +
  ylab("S(t)") +
  xlab("Tempo de casa (em meses)") +
  theme(legend.title = element_blank())

plot2 <- ggplot(pred_risk) +
  geom_line(aes(x = time, y = hazard, color = strata), linewidth = 1) +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 168, 24)) +
  theme_bw() +
  ylab("h(t)") +
  xlab("Tempo de casa (em meses)") +
  theme(legend.title = element_blank())

ggarrange(plot1, plot2, ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom")

##########################################################
####   Base Saldamento - estudo dos cenários criados  ####
##########################################################

#### Importando base de dados ####
base_saldamento <- read_parquet('../Bases/base_saldamento_clusters_variaveis.parquet')

# Obter todas as combinações possíveis de CC, clusterFC e TC
todas_combinacoes <- expand.grid(CC = unique(base_saldamento$CC),
                                 clusterFC = unique(base_saldamento$clusterFC),
                                 TC = unique(base_saldamento$TC))

# Contando as combinações presentes na base
combinacoes_presentes <- base_saldamento |>
  group_by(CC, clusterFC, TC) |>
  summarise(Contagem = n(), .groups = 'drop')

# Fazer um join para incluir todas as combinações possíveis, preenchendo com zero onde não há combinações
combinacoes_completas <- todas_combinacoes |>
  left_join(combinacoes_presentes, by = c("CC", "clusterFC", "TC")) |>
  mutate(Contagem = replace_na(Contagem, 0)) |>
  arrange(desc(Contagem))

combinacoes_completas

##########################################################
#### Base Saldamento - clusterização das covariáveis  ####
##########################################################

#### Importando base de dados ####
base_saldamento <- read_parquet('../Bases/base_saldamento_clusters.parquet')

# Para ser mais fácil de entender e dado que não houve clusterização nas covariáveis
# categoriaCliente e tipoCobertura, irei relacionar os nomes com os valores

cc_map <- c("1" = "Cliente MAG", "2" = "MAG Blue", "3" = "sem categoriaCliente")
tc_map <- c("1" = "INVALIDEZ", "2" = "MORTE")

base_saldamento <- base_saldamento %>%
  mutate(
    # Separando as partes da coluna 'combinação'
    cc = cc_map[substr(combinação, 1, 1)],
    fc = substr(combinação, 3, 3),
    tc = tc_map[substr(combinação, 5, 5)],
    # Combinando as partes no novo formato
    combinação = paste(cc, paste0("FC", fc), tc, sep = "-")
  ) %>%
  select(-cc, -fc, -tc) # Removendo as colunas temporárias

km_surv <- survfit2(Surv(tempo_mes, delta2) ~ combinação,
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

ggplot(km_surv) + 
  geom_step(aes(x = time, y = estimate, color = strata), linewidth = 1) +
  scale_x_continuous(breaks = seq(0, 168, 24)) +
  theme_bw() +
  ylab("S(t)") +
  xlab("Tempo de casa (em meses)") +
  theme(legend.position = "bottom", legend.title = element_blank())

ggplot(pred_risk) +
  geom_line(aes(x = time, y = hazard, color = strata), linewidth = 1) +
  scale_x_continuous(breaks = seq(0, 168, 24)) +
  scale_y_continuous(limits = c(0, 0.25)) +
  theme_bw() +
  ylab("h(t)") +
  xlab("Tempo de casa (em meses)") +
  theme(legend.position = "bottom", legend.title = element_blank())

##########################################################
####         Base Saldamento - agrupamentos           ####
##########################################################

#### Importando base de dados ####
base_saldamento <- read_parquet('../Bases/base_saldamento_clusters.parquet')

# Vetores de mapeamento para CC e TC
cc_map <- c("1" = "Cliente MAG", "2" = "MAG Blue", "3" = "sem categoriaCliente", "4" = "CC4", "5" = "CC5")
tc_map <- c("1" = "INVALIDEZ", "2" = "MORTE", "4" = "TC4", "5" = "TC5")

base_saldamento <- base_saldamento %>%
  mutate(
    # Separando as partes da coluna 'combinação_agrupada'
    cc = cc_map[substr(combinação_agrupada, 1, 1)],
    fc = substr(combinação_agrupada, 3, 3),
    tc = tc_map[substr(combinação_agrupada, 5, 5)],
    # Combinando as partes no novo formato
    combinação_agrupada = paste(cc, paste0("FC", fc), tc, sep = "-")
  ) %>%
  mutate(
    # Transformando 'combinação_agrupada' em fator para controlar a ordem
    combinação_agrupada = factor(combinação_agrupada, 
                                 levels = c(
                                   # Incluindo primeiro os valores desejados
                                   "Cliente MAG-FC1-MORTE",
                                   "Cliente MAG-FC2-MORTE",
                                   "MAG Blue-FC1-MORTE",
                                   "MAG Blue-FC2-MORTE",
                                   # Outros valores adicionados depois
                                   "CC4-FC4-TC4", "CC5-FC5-TC5"))
  )

strata_levels <- c(
  "Cliente MAG-FC1-MORTE",
  "Cliente MAG-FC2-MORTE",
  "MAG Blue-FC1-MORTE",
  "MAG Blue-FC2-MORTE",
  "CC4-FC4-TC4", 
  "CC5-FC5-TC5")

km_surv <- survfit2(Surv(tempo_mes, delta2) ~ combinação_agrupada,
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


pred_risk <- pred_risk %>%
  mutate(strata = factor(strata, levels = strata_levels))

ggplot(km_surv) + 
  geom_step(aes(x = time, y = estimate, color = strata), linewidth = 1) +
  scale_x_continuous(breaks = seq(0, 168, 24)) +
  theme_bw() +
  ylab("S(t)") +
  xlab("Tempo de casa (em meses)") +
  theme(legend.position = "bottom", legend.title = element_blank())

ggplot(pred_risk) +
  geom_line(aes(x = time, y = hazard, color = strata), linewidth = 1) +
  scale_x_continuous(breaks = seq(0, 168, 24)) +
  scale_y_continuous(limits = c(0, 0.25)) +
  theme_bw() +
  ylab("h(t)") +
  xlab("Tempo de casa (em meses)") +
  theme(legend.position = "bottom", legend.title = element_blank())

#### Proporção por combinação agrupada
ggplot(base_saldamento, aes(x = combinação_agrupada, y = prop.table(after_stat(count))*100, label = scales::percent(prop.table(after_stat(count)), 0.01))) +
  geom_bar(position = "dodge", fill = "skyblue2", color = "black") +
  geom_text(stat = "count", position = position_dodge(.9), vjust = -0.5, size = 4) +
  scale_y_continuous(n.breaks = 10, limits = c(0, 80)) +
  labs(x = "", y = "") +
  theme_bw()

### Quantidade por combinação agrupada
base_saldamento |> 
  count(combinação_agrupada) |> 
  ggplot(aes(x = combinação_agrupada, y = n, label = n)) +
  geom_bar(stat = "identity", fill = "skyblue2", color = "black") +
  geom_text(vjust = -0.5, size = 4) +
  theme_bw() +
  labs(x = "", y = "")

##########################################################
####             Base Saldamento - modelagem          ####
##########################################################

# Carregando a base
base_saldamento <- read_parquet('../Bases/base_saldamento_clusters.parquet')

# Realizando truncamento na base saldamento
base_saldamento <- base_saldamento |>
  mutate(delta2 = case_when((combinação_agrupada == "1-2-2" & tempo_mes > 100) ~ 0,
                            (combinação_agrupada == "4-4-4" & tempo_mes > 84) ~ 0,
                            (combinação_agrupada == "5-5-5" & tempo_mes > 96) ~ 0,
                            TRUE ~ delta2),
         tempo_mes = case_when((combinação_agrupada == "1-2-2" & tempo_mes > 100) ~ 100,
                               (combinação_agrupada == "4-4-4" & tempo_mes > 84) ~ 84,
                               (combinação_agrupada == "5-5-5" & tempo_mes > 96) ~ 96,
                               TRUE ~ tempo_mes))

## Modelo escolhido com base nas métricas

# Carregando o modelo
model_4comp_sald <- read_rds('../Modelos/Bay_3/modelo_4comp_sald.rds')

# Irei escolher 4 Componentes, cadeia 2
model_4comp_sald$posterior <- posterior::subset_draws(model_4comp_sald$posterior, chain = 2, iter = 14001:16000)

# Olhando o comportamento das cadeias
mcmc_trace(model_4comp_sald$posterior)

# Sobrevivência
preds4Bayesian_sald <- plot_fit_on_data(model_4comp_sald,  data = base_saldamento, interval = "credible")

preds4Bayesian_sald$ggplot +
  scale_x_continuous(breaks = seq(0, 168, 24)) +
  theme_bw() +
  ylab("S(t)") +
  xlab("Tempo de casa (em meses)") +
  theme(legend.position = "bottom", legend.title = element_blank())

# Risco
preds4Bayesian_risco_sald <- plot_fit_on_data(model_4comp_sald, data = base_saldamento, type = "hazard", interval = "credible")

preds4Bayesian_risco_sald$ggplot +
  scale_x_continuous(breaks = seq(0, 168, 24)) +
  theme_bw() +
  ylab("h(t)") +
  xlab("Tempo de casa (em meses)") +
  theme(legend.position = "bottom", legend.title = element_blank())

#####################################################################
####           Estudando os Cenários - Saldamento                ####
#####################################################################

# Olhando a sobrevivência de cada agrupamento separadamente:
data1 <- preds4Bayesian_sald$preds

# Olhando a sobrevivência de cada agrupamento separadamente:
data <- preds4Bayesian_risco_sald$preds

## Cenário 1: CC=1, clusterFC=1, TC=2

# Sobrevivência
filtro1 <- data1 |>
  filter(strata == "CC=1, clusterFC=1, TC=2")

plot1 <- ggplot(filtro1, aes(x = time)) +
  geom_step(aes(y = estimate, linetype = "KM Cenário 1"), linewidth = 1) +
  geom_line(aes(y = .pred_survival, color = "MSMLN Cenário 1"), linewidth = 1) +
  geom_ribbon(aes(ymin = .pred_lower, ymax = .pred_upper, fill = "IC 95%"), alpha = 0.5) +
  scale_linetype_manual(values = c("KM Cenário 1" = "solid"), guide = guide_legend(order = 1)) +
  scale_color_manual(values = c("MSMLN Cenário 1" = "blue"), guide = guide_legend(order = 2)) +
  scale_fill_manual(values = c("IC 95%" = "skyblue2"), guide = guide_legend(order = 3)) +
  scale_x_continuous(breaks = seq(0, 168, 24), limits = c(0, 168)) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_bw() +
  ylab("S(t)") +
  xlab("Tempo de casa (em meses)") +
  theme(legend.position = "bottom", legend.title = element_blank())

# Risco
filtro1 <- data |>
  filter(strata == "CC=1, clusterFC=1, TC=2")

plot2 <- ggplot(filtro1, aes(x = time)) +
  geom_line(aes(y = hazard_estimate, linetype = "Empírica Cenário 1"), linewidth = 1) +
  geom_line(aes(y = .pred_hazard, color = "Ajustada Cenário 1"), linewidth = 1) +
  geom_ribbon(aes(ymin = .pred_lower, ymax = .pred_upper, fill = "IC 95%"), alpha = 0.5) +
  scale_linetype_manual(values = c("Empírica Cenário 1" = "solid"), guide = guide_legend(order = 1)) +
  scale_color_manual(values = c("Ajustada Cenário 1" = "blue"), guide = guide_legend(order = 2)) +
  scale_fill_manual(values = c("IC 95%" = "skyblue2"), guide = guide_legend(order = 3)) +
  scale_x_continuous(breaks = seq(0, 168, 24), limits = c(0, 168)) +
  scale_y_continuous(limits = c(0, 0.25)) +
  theme_bw() +
  ylab("h(t)") +
  xlab("Tempo de casa (em meses)") +
  theme(legend.position = "bottom", legend.title = element_blank())

ggarrange(plot1, plot2, ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom")

## Cenário 2: CC=1, clusterFC=2, TC=2

# Sobrevivência
filtro2 <- data1 |>
  filter(strata == "CC=1, clusterFC=2, TC=2")

plot1 <- ggplot(filtro2, aes(x = time)) +
  geom_step(aes(y = estimate, linetype = "KM Cenário 2"), linewidth = 1) +
  geom_line(aes(y = .pred_survival, color = "MSMLN Cenário 2"), linewidth = 1) +
  geom_ribbon(aes(ymin = .pred_lower, ymax = .pred_upper, fill = "IC 95%"), alpha = 0.5) +
  scale_linetype_manual(values = c("KM Cenário 2" = "solid"), guide = guide_legend(order = 1)) +
  scale_color_manual(values = c("MSMLN Cenário 2" = "blue"), guide = guide_legend(order = 2)) +
  scale_fill_manual(values = c("IC 95%" = "skyblue2"), guide = guide_legend(order = 3)) +
  scale_x_continuous(breaks = seq(0, 168, 24), limits = c(0, 168)) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_bw() +
  ylab("S(t)") +
  xlab("Tempo de casa (em meses)") +
  theme(legend.position = "bottom", legend.title = element_blank())

# Risco
filtro2 <- data |>
  filter(strata == "CC=1, clusterFC=2, TC=2")

plot2 <- ggplot(filtro2, aes(x = time)) +
  geom_line(aes(y = hazard_estimate, linetype = "Empírica Cenário 2"), linewidth = 1) +
  geom_line(aes(y = .pred_hazard, color = "Ajustada Cenário 2"), linewidth = 1) +
  geom_ribbon(aes(ymin = .pred_lower, ymax = .pred_upper, fill = "IC 95%"), alpha = 0.5) +
  scale_linetype_manual(values = c("Empírica Cenário 2" = "solid"), guide = guide_legend(order = 1)) +
  scale_color_manual(values = c("Ajustada Cenário 2" = "blue"), guide = guide_legend(order = 2)) +
  scale_fill_manual(values = c("IC 95%" = "skyblue2"), guide = guide_legend(order = 3)) +
  scale_x_continuous(breaks = seq(0, 168, 24), limits = c(0, 168)) +
  scale_y_continuous(limits = c(0, 0.25)) +
  theme_bw() +
  ylab("h(t)") +
  xlab("Tempo de casa (em meses)") +
  theme(legend.position = "bottom", legend.title = element_blank())

ggarrange(plot1, plot2, ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom")

## Cenário 3: CC=2, clusterFC=1, TC=2

# Sobrevivência
filtro3 <- data1 |>
  filter(strata == "CC=2, clusterFC=1, TC=2")

plot1 <- ggplot(filtro3, aes(x = time)) +
  geom_step(aes(y = estimate, linetype = "KM Cenário 3"), linewidth = 1) +
  geom_line(aes(y = .pred_survival, color = "MSMLN Cenário 3"), linewidth = 1) +
  geom_ribbon(aes(ymin = .pred_lower, ymax = .pred_upper, fill = "IC 95%"), alpha = 0.5) +
  scale_linetype_manual(values = c("KM Cenário 3" = "solid"), guide = guide_legend(order = 1)) +
  scale_color_manual(values = c("MSMLN Cenário 3" = "blue"), guide = guide_legend(order = 2)) +
  scale_fill_manual(values = c("IC 95%" = "skyblue2"), guide = guide_legend(order = 3)) +
  scale_x_continuous(breaks = seq(0, 168, 24), limits = c(0, 168)) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_bw() +
  ylab("S(t)") +
  xlab("Tempo de casa (em meses)") +
  theme(legend.position = "bottom", legend.title = element_blank())

# Risco
filtro3 <- data|>
  filter(strata == "CC=2, clusterFC=1, TC=2")

plot2 <- ggplot(filtro3, aes(x = time)) +
  geom_line(aes(y = hazard_estimate, linetype = "Empírica Cenário 3"), linewidth = 1) +
  geom_line(aes(y = .pred_hazard, color = "Ajustada Cenário 3"), linewidth = 1) +
  geom_ribbon(aes(ymin = .pred_lower, ymax = .pred_upper, fill = "IC 95%"), alpha = 0.5) +
  scale_linetype_manual(values = c("Empírica Cenário 3" = "solid"), guide = guide_legend(order = 1)) +
  scale_color_manual(values = c("Ajustada Cenário 3" = "blue"), guide = guide_legend(order = 2)) +
  scale_fill_manual(values = c("IC 95%" = "skyblue2"), guide = guide_legend(order = 3)) +
  scale_x_continuous(breaks = seq(0, 168, 24), limits = c(0, 168)) +
  scale_y_continuous(limits = c(0, 0.25)) +
  theme_bw() +
  ylab("h(t)") +
  xlab("Tempo de casa (em meses)") +
  theme(legend.position = "bottom", legend.title = element_blank())

ggarrange(plot1, plot2, ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom")

## Cenário 4: CC=2, clusterFC=2, TC=2

# Sobrevivência
filtro4 <- data1 |>
  filter(strata == "CC=2, clusterFC=2, TC=2")

plot1 <- ggplot(filtro4, aes(x = time)) +
  geom_step(aes(y = estimate, linetype = "KM Cenário 4"), linewidth = 1) +
  geom_line(aes(y = .pred_survival, color = "MSMLN Cenário 4"), linewidth = 1) +
  geom_ribbon(aes(ymin = .pred_lower, ymax = .pred_upper, fill = "IC 95%"), alpha = 0.5) +
  scale_linetype_manual(values = c("KM Cenário 4" = "solid"), guide = guide_legend(order = 1)) +
  scale_color_manual(values = c("MSMLN Cenário 4" = "blue"), guide = guide_legend(order = 2)) +
  scale_fill_manual(values = c("IC 95%" = "skyblue2"), guide = guide_legend(order = 3)) +
  scale_x_continuous(breaks = seq(0, 168, 24), limits = c(0, 168)) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_bw() +
  ylab("S(t)") +
  xlab("Tempo de casa (em meses)") +
  theme(legend.position = "bottom", legend.title = element_blank())

# Risco
filtro4 <- data |>
  filter(strata == "CC=2, clusterFC=2, TC=2")

plot2 <- ggplot(filtro4, aes(x = time)) +
  geom_line(aes(y = hazard_estimate, linetype = "Empírica Cenário 4"), linewidth = 1) +
  geom_line(aes(y = .pred_hazard, color = "Ajustada Cenário 4"), linewidth = 1) +
  geom_ribbon(aes(ymin = .pred_lower, ymax = .pred_upper, fill = "IC 95%"), alpha = 0.5) +
  scale_linetype_manual(values = c("Empírica Cenário 4" = "solid"), guide = guide_legend(order = 1)) +
  scale_color_manual(values = c("Ajustada Cenário 4" = "blue"), guide = guide_legend(order = 2)) +
  scale_fill_manual(values = c("IC 95%" = "skyblue2"), guide = guide_legend(order = 3)) +
  scale_x_continuous(breaks = seq(0, 168, 24), limits = c(0, 168)) +
  scale_y_continuous(limits = c(0, 0.25)) +
  theme_bw() +
  ylab("h(t)") +
  xlab("Tempo de casa (em meses)") +
  theme(legend.position = "bottom", legend.title = element_blank())

ggarrange(plot1, plot2, ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom")

## Cenário 5: CC=4, clusterFC=4, TC=4

# Sobrevivência
filtro5 <- data1 |>
  filter(strata == "CC=4, clusterFC=4, TC=4")

plot1 <- ggplot(filtro5, aes(x = time)) +
  geom_step(aes(y = estimate, linetype = "KM Cenário 5"), linewidth = 1) +
  geom_line(aes(y = .pred_survival, color = "MSMLN Cenário 5"), linewidth = 1) +
  geom_ribbon(aes(ymin = .pred_lower, ymax = .pred_upper, fill = "IC 95%"), alpha = 0.5) +
  scale_linetype_manual(values = c("KM Cenário 5" = "solid"), guide = guide_legend(order = 1)) +
  scale_color_manual(values = c("MSMLN Cenário 5" = "blue"), guide = guide_legend(order = 2)) +
  scale_fill_manual(values = c("IC 95%" = "skyblue2"), guide = guide_legend(order = 3)) +
  scale_x_continuous(breaks = seq(0, 168, 24), limits = c(0, 168)) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_bw() +
  ylab("S(t)") +
  xlab("Tempo de casa (em meses)") +
  theme(legend.position = "bottom", legend.title = element_blank())

# Risco
filtro5 <- data |>
  filter(strata == "CC=4, clusterFC=4, TC=4")

plot2 <- ggplot(filtro5, aes(x = time)) +
  geom_line(aes(y = hazard_estimate, linetype = "Empírica Cenário 5"), linewidth = 1) +
  geom_line(aes(y = .pred_hazard, color = "Ajustada Cenário 5"), linewidth = 1) +
  geom_ribbon(aes(ymin = .pred_lower, ymax = .pred_upper, fill = "IC 95%"), alpha = 0.5) +
  scale_linetype_manual(values = c("Empírica Cenário 5" = "solid"), guide = guide_legend(order = 1)) +
  scale_color_manual(values = c("Ajustada Cenário 5" = "blue"), guide = guide_legend(order = 2)) +
  scale_fill_manual(values = c("IC 95%" = "skyblue2"), guide = guide_legend(order = 3)) +
  scale_x_continuous(breaks = seq(0, 168, 24), limits = c(0, 168)) +
  scale_y_continuous(limits = c(0, 0.25)) +
  theme_bw() +
  ylab("h(t)") +
  xlab("Tempo de casa (em meses)") +
  theme(legend.position = "bottom", legend.title = element_blank())

ggarrange(plot1, plot2, ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom")

## Cenário 6: CC=5, clusterFC=5, TC=5

# Sobrevivência
filtro6 <- data1 |>
  filter(strata == "CC=5, clusterFC=5, TC=5")

plot1 <- ggplot(filtro6, aes(x = time)) +
  geom_step(aes(y = estimate, linetype = "KM Cenário 6"), linewidth = 1) +
  geom_line(aes(y = .pred_survival, color = "MSMLN Cenário 6"), linewidth = 1) +
  geom_ribbon(aes(ymin = .pred_lower, ymax = .pred_upper, fill = "IC 95%"), alpha = 0.5) +
  scale_linetype_manual(values = c("KM Cenário 6" = "solid"), guide = guide_legend(order = 1)) +
  scale_color_manual(values = c("MSMLN Cenário 6" = "blue"), guide = guide_legend(order = 2)) +
  scale_fill_manual(values = c("IC 95%" = "skyblue2"), guide = guide_legend(order = 3)) +
  scale_x_continuous(breaks = seq(0, 168, 24), limits = c(0, 168)) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_bw() +
  ylab("S(t)") +
  xlab("Tempo de casa (em meses)") +
  theme(legend.position = "bottom", legend.title = element_blank())

# Risco
filtro6 <- data |>
  filter(strata == "CC=5, clusterFC=5, TC=5")

plot2 <- ggplot(filtro6, aes(x = time)) +
  geom_line(aes(y = hazard_estimate, linetype = "Empírica Cenário 6"), linewidth = 1) +
  geom_line(aes(y = .pred_hazard, color = "Ajustada Cenário 6"), linewidth = 1) +
  geom_ribbon(aes(ymin = .pred_lower, ymax = .pred_upper, fill = "IC 95%"), alpha = 0.5) +
  scale_linetype_manual(values = c("Empírica Cenário 6" = "solid"), guide = guide_legend(order = 1)) +
  scale_color_manual(values = c("Ajustada Cenário 6" = "blue"), guide = guide_legend(order = 2)) +
  scale_fill_manual(values = c("IC 95%" = "skyblue2"), guide = guide_legend(order = 3)) +
  scale_x_continuous(breaks = seq(0, 168, 24), limits = c(0, 168)) +
  scale_y_continuous(limits = c(0, 0.25)) +
  theme_bw() +
  ylab("h(t)") +
  xlab("Tempo de casa (em meses)") +
  theme(legend.position = "bottom", legend.title = element_blank())

ggarrange(plot1, plot2, ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom")

#################################### Só os cenários (sem as KM) ################################

# Sobrevivência
plot1 <- ggplot(preds4Bayesian_sald$preds) +
  geom_line(aes(x = time, y = .pred_survival, color = strata), linewidth = 1.2) +
  scale_x_continuous(breaks = seq(0, 168, 24), limits = c(0, 168)) +
  scale_y_continuous(limits = c(0, 1))  +
  scale_color_manual(name = "Cenário",
                     values = c("CC=1, clusterFC=1, TC=2" = "red",
                                "CC=1, clusterFC=2, TC=2" = "blue",
                                "CC=2, clusterFC=1, TC=2" = "green",
                                "CC=2, clusterFC=2, TC=2" = "purple",
                                "CC=4, clusterFC=4, TC=4" = "orange",
                                "CC=5, clusterFC=5, TC=5" = "brown"),
                     labels = c("CC=1, clusterFC=1, TC=2" = "Cenário 1",
                                "CC=1, clusterFC=2, TC=2" = "Cenário 2",
                                "CC=2, clusterFC=1, TC=2" = "Cenário 3",
                                "CC=2, clusterFC=2, TC=2" = "Cenário 4",
                                "CC=4, clusterFC=4, TC=4" = "Cenário 5",
                                "CC=5, clusterFC=5, TC=5" = "Cenário 6")) +
  theme_bw() +
  ylab("S(t)") +
  xlab("Tempo de casa (em meses)") +
  theme(legend.position = "bottom", legend.title = element_blank())

# Risco
plot2 <- ggplot(preds4Bayesian_risco_sald$preds) +
  geom_line(aes(x = time, y = .pred_hazard, color = strata), linewidth = 1.2) +
  scale_x_continuous(breaks = seq(0, 168, 24), limits = c(0, 168)) +
  scale_y_continuous(limits = c(0, 0.25))  +
  scale_color_manual(name = "Cenário",
                     values = c("CC=1, clusterFC=1, TC=2" = "red",
                                "CC=1, clusterFC=2, TC=2" = "blue",
                                "CC=2, clusterFC=1, TC=2" = "green",
                                "CC=2, clusterFC=2, TC=2" = "purple",
                                "CC=4, clusterFC=4, TC=4" = "orange",
                                "CC=5, clusterFC=5, TC=5" = "brown"),
                     labels = c("CC=1, clusterFC=1, TC=2" = "Cenário 1",
                                "CC=1, clusterFC=2, TC=2" = "Cenário 2",
                                "CC=2, clusterFC=1, TC=2" = "Cenário 3",
                                "CC=2, clusterFC=2, TC=2" = "Cenário 4",
                                "CC=4, clusterFC=4, TC=4" = "Cenário 5",
                                "CC=5, clusterFC=5, TC=5" = "Cenário 6")) +
  theme_bw() +
  ylab("h(t)") +
  xlab("Tempo de casa (em meses)") +
  theme(legend.position = "bottom", legend.title = element_blank())

ggarrange(plot1, plot2, ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom")

#################################### Comparando Cenário 1 x Cenário 2 ################################

## Cenário 1: CC=1, clusterFC=1, TC=2
## Cenário 2: CC=1, clusterFC=2, TC=2

# Sobrevivência
filtroc12 <- data1 |>
  filter(strata %in% c("CC=1, clusterFC=1, TC=2",
                       "CC=1, clusterFC=2, TC=2"))

plot1 <- ggplot(filtroc12) +
  geom_line(aes(x = time, y = .pred_survival, color = strata), linewidth = 1.2) +
  scale_x_continuous(breaks = seq(0, 168, 24), limits = c(0, 168)) +
  scale_y_continuous(limits = c(0, 1))  +
  scale_color_manual(name = "Cenário",
                     values = c("CC=1, clusterFC=1, TC=2" = "red",
                                "CC=1, clusterFC=2, TC=2" = "blue"),
                     labels = c("CC=1, clusterFC=1, TC=2" = "Cenário 1",
                                "CC=1, clusterFC=2, TC=2" = "Cenário 2")) +
  theme_bw() +
  ylab("S(t)") +
  xlab("Tempo de casa (em meses)") +
  theme(legend.position = "bottom", legend.title = element_blank())

# Risco
filtroc12_risco <- data |>
  filter(strata %in% c("CC=1, clusterFC=1, TC=2",
                       "CC=1, clusterFC=2, TC=2"))

plot2 <- ggplot(filtroc12_risco) +
  geom_line(aes(x = time, y = .pred_hazard, color = strata), linewidth = 1.2) +
  scale_x_continuous(breaks = seq(0, 168, 24), limits = c(0, 168)) +
  scale_y_continuous(limits = c(0, 0.25))  +
  scale_color_manual(name = "Cenário",
                     values = c("CC=1, clusterFC=1, TC=2" = "red",
                                "CC=1, clusterFC=2, TC=2" = "blue"),
                     labels = c("CC=1, clusterFC=1, TC=2" = "Cenário 1",
                                "CC=1, clusterFC=2, TC=2" = "Cenário 2")) +
  theme_bw() +
  ylab("h(t)") +
  xlab("Tempo de casa (em meses)") +
  theme(legend.position = "bottom", legend.title = element_blank())

ggarrange(plot1, plot2, ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom")

#################################### Comparando Cenário 1 x Cenário 3 ################################

## Cenário 1: CC=1, clusterFC=1, TC=2
## Cenário 3: CC=2, clusterFC=1, TC=2

# Sobrevivência
filtroc13 <- data1 |>
  filter(strata %in% c("CC=1, clusterFC=1, TC=2",
                       "CC=2, clusterFC=1, TC=2"))

plot1 <- ggplot(filtroc13) +
  geom_line(aes(x = time, y = .pred_survival, color = strata), linewidth = 1.2) +
  scale_x_continuous(breaks = seq(0, 168, 24), limits = c(0, 168)) +
  scale_y_continuous(limits = c(0, 1))  +
  scale_color_manual(name = "Cenário",
                     values = c("CC=1, clusterFC=1, TC=2" = "red",
                                "CC=2, clusterFC=1, TC=2" = "green"),
                     labels = c("CC=1, clusterFC=1, TC=2" = "Cenário 1",
                                "CC=2, clusterFC=1, TC=2" = "Cenário 3")) +
  theme_bw() +
  ylab("S(t)") +
  xlab("Tempo de casa (em meses)") +
  theme(legend.position = "bottom", legend.title = element_blank())

# Risco
filtroc13_risco <- data |>
  filter(strata %in% c("CC=1, clusterFC=1, TC=2",
                       "CC=2, clusterFC=1, TC=2"))

plot2 <- ggplot(filtroc13_risco) +
  geom_line(aes(x = time, y = .pred_hazard, color = strata), linewidth = 1.2) +
  scale_x_continuous(breaks = seq(0, 168, 24), limits = c(0, 168)) +
  scale_y_continuous(limits = c(0, 0.25))  +
  scale_color_manual(name = "Cenário",
                     values = c("CC=1, clusterFC=1, TC=2" = "red",
                                "CC=2, clusterFC=1, TC=2" = "green"),
                     labels = c("CC=1, clusterFC=1, TC=2" = "Cenário 1",
                                "CC=2, clusterFC=1, TC=2" = "Cenário 3")) +
  theme_bw() +
  ylab("h(t)") +
  xlab("Tempo de casa (em meses)") +
  theme(legend.position = "bottom", legend.title = element_blank())

ggarrange(plot1, plot2, ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom")

##########################################################
####   Base Saldamento - Parâmetros para o Relatório  ####
##########################################################

# Carregando o modelo
model_4comp_sald <- read_rds('../Modelos/Bay_3/modelo_4comp_sald.rds')

# Irei escolher 4 Componentes, cadeia 2
model_4comp_sald$posterior <- posterior::subset_draws(model_4comp_sald$posterior, chain = 2, iter = 14001:16000)

# Parâmetros
model_4comp_sald

# Dado que o pacote lnmixsurv não traz a estimativa e IC para a 
# última componente de mistura, irei calcular

model_4comp_sald$posterior <- model_4comp_sald$posterior |> 
  as_tibble() |> 
  mutate(eta_4 = 1 - (eta_1 + eta_2 + eta_3)) |> 
  posterior::as_draws()

# Intervalo para os etas

model_4comp_sald$posterior |> 
  posterior::subset_draws(variable = 'eta', regex = TRUE) |> 
  as_tibble() |> 
  pivot_longer(cols = starts_with('eta'), names_to = 'eta', values_to = 'value') |>
  group_by(eta) |> 
  summarise(mediana = median(value),
            ic_025 = quantile(value, 0.025),
            ic_975 = quantile(value, 0.975)) |> 
  ungroup() |> 
  group_by(eta) |> 
  summarise(across(where(is.numeric), ~ round(., 3)))

##########################################################
####          Base Saldamento - Previsões             ####
##########################################################

#### Importando base de dados ####
base_saldamento <- read_parquet('../Bases/base_saldamento_clusters.parquet')

# Realizando truncamento na base saldamento
base_saldamento <- base_saldamento |>
  mutate(delta2 = case_when((combinação_agrupada == "1-2-2" & tempo_mes > 100) ~ 0,
                            (combinação_agrupada == "4-4-4" & tempo_mes > 84) ~ 0,
                            (combinação_agrupada == "5-5-5" & tempo_mes > 96) ~ 0,
                            TRUE ~ delta2),
         tempo_mes = case_when((combinação_agrupada == "1-2-2" & tempo_mes > 100) ~ 100,
                               (combinação_agrupada == "4-4-4" & tempo_mes > 84) ~ 84,
                               (combinação_agrupada == "5-5-5" & tempo_mes > 96) ~ 96,
                               TRUE ~ tempo_mes))

# Carregando o modelo
model_4comp_sald <- read_rds('../Modelos/Bay_3/modelo_4comp_sald.rds')

# Irei escolher 4 Componentes, cadeia 2
model_4comp_sald$posterior <- posterior::subset_draws(model_4comp_sald$posterior, chain = 2, iter = 14001:16000)

# Sobrevivência
preds4Bayesian_sald <- plot_fit_on_data(model_4comp_sald,  data = base_saldamento, interval = "credible")

previsoes_sobrevivencia_saldamento <- preds4Bayesian_sald$preds |>
  select(time, strata, .pred_survival)

# Salvar o dataframe 'previsoes_sobrevivencia_saldamento' como um arquivo CSV
write.csv(previsoes_sobrevivencia_saldamento, "previsoes_sobrevivencia_saldamento.csv", row.names = FALSE)

# Risco
preds4Bayesian_risco_sald <- plot_fit_on_data(model_4comp_sald, data = base_saldamento, type = "hazard", interval = "credible")

previsoes_risco_saldamento <- preds4Bayesian_risco_sald$preds |>
  select(time, strata, .pred_hazard)

# Salvar o dataframe 'previsoes_risco_saldamento' como um arquivo CSV
write.csv(previsoes_risco_saldamento, "previsoes_risco_saldamento.csv", row.names = FALSE)

##########################################################
####    Base Saldamento - Mapa da Clusterização       ####
##########################################################

#### Importando base de dados ####
base_saldamento <- read_parquet('../Bases/base_saldamento_clusters.parquet')

# Criando o mapa, removendo duplicatas e separando a coluna Cluster em três novas colunas
mapa_saldamento <- base_saldamento |>
  select(cluster = combinação_agrupada, categoriaCliente, formaCobranca, tipoCobertura) |>  # Seleciona as colunas
  distinct() |> # Remove duplicatas
  separate(cluster, into = c("CC", "clusterFC", "TC"), sep = "-", remove = FALSE) |> # Separa a coluna Cluster
  select(cluster, CC, categoriaCliente, clusterFC, formaCobranca, TC, tipoCobertura) |> # Reorganiza as colunas
  select(-cluster)

# Salvar o dataframe 'mapa_saldamento' como um arquivo CSV
write.csv(mapa_saldamento, "mapa_saldamento.csv", row.names = FALSE)