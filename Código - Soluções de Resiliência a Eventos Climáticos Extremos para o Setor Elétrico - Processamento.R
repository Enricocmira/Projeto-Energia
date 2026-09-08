############################################################
# Soluções de Resiliência a Eventos Climáticos             #
# Extremos para o Setor Elétrico                           #
# Nomes: Enrico C. Mira                                    #
#        Leandro S. Maciel                                 #
# Data: 13/05/2026                                         #
############################################################


# Limpeza de ambiente 
rm(list=ls()); graphics.off()

# ==========================================================
# MODELAGEM E TABELAS CONDICIONAIS
# ==========================================================
library(readxl)
library(dplyr)
library(lmtest)
library(sandwich)
library(stargazer)
library(FinTS)

# Carregar os dados salvos na Parte 1
intradiario = read_excel("df_intermediario.xlsx")

# Filtrando
df_intradiario <- intradiario %>%
  mutate(eficiencia = eficiencia_horaria,
         eficiencia_lag1 = lag(eficiencia)) %>%
  na.omit() # Remove a primeira linha que ficou com NA devido ao lag

# Padroniza as variáveis explicativas e cria a interação correta 
# para mitigar a multicolinearidade estrutural.
df_intradiario_scaled <- df_intradiario %>%
  arrange(DateTime) %>%
  mutate(
    eficiencia = eficiencia_horaria,
    
    # Padronização (média 0, desvio padrão 1)
    temp_z         = as.numeric(scale(temp)),
    umid_z         = as.numeric(scale(umid)),
    vento_z        = as.numeric(scale(vento))
  ) %>%
  mutate(
    # Interação calculada a partir das variáveis já padronizadas
    interacao_temp_umid_z = temp_z * umid_z
  ) %>%
  na.omit()

# Funções auxiliares seguras para capturar P-Valor
safe_bg_pval   <- function(m) tryCatch(bgtest(m)$p.value, error = function(e) NA)
safe_bp_pval   <- function(m) tryCatch(bptest(m)$p.value, error = function(e) NA)
safe_arch_pval <- function(m) tryCatch(ArchTest(resid(m), lags = 1)$p.value, error = function(e) NA)

# Função para verificar se existe problema em QUALQUER modelo da lista
verificar_problemas <- function(lista_modelos) {
  p_bg   <- sapply(lista_modelos, safe_bg_pval)
  p_bp   <- sapply(lista_modelos, safe_bp_pval)
  p_arch <- sapply(lista_modelos, safe_arch_pval)
  
  # Retorna TRUE se houver pelo menos um p-valor < 0.05 (rejeita H0 de ausência de problema)
  problema <- any(p_bg < 0.05, na.rm = TRUE) | 
    any(p_bp < 0.05, na.rm = TRUE) | 
    any(p_arch < 0.05, na.rm = TRUE)
  return(isTRUE(problema))
}

nomes_modelos <- c("M1", "M2", "M3", "M4", "M5", "M6", "M7")

# ==========================================================
# REGRESSÕES MÚLTIPLAS (VARIÁVEIS ORIGINAIS SEM DUMMIES)
# ==========================================================
m1_orig_base <- lm(eficiencia ~ eficiencia_lag1, data = df_intradiario_scaled)
m2_orig_base <- lm(eficiencia ~ eficiencia_lag1 + temp, data = df_intradiario_scaled)
m3_orig_base <- lm(eficiencia ~ eficiencia_lag1 + umid, data = df_intradiario_scaled)
m4_orig_base <- lm(eficiencia ~ eficiencia_lag1 + temp + umid, data = df_intradiario_scaled)
m5_orig_base <- lm(eficiencia ~ eficiencia_lag1 + temp + umid + interacao_temp_umid, data = df_intradiario_scaled)
m6_orig_base <- lm(eficiencia ~ eficiencia_lag1 + temp + umid + interacao_temp_umid + log(precip + 1), data = df_intradiario_scaled)
m7_orig_base <- lm(eficiencia ~ eficiencia_lag1 + temp + umid + interacao_temp_umid + log(precip + 1) + vento, data = df_intradiario_scaled)

modelos_orig_base <- list(m1_orig_base, m2_orig_base, m3_orig_base, m4_orig_base, m5_orig_base, m6_orig_base, m7_orig_base)

if(verificar_problemas(modelos_orig_base)) {
  erros_hac_orig_base <- lapply(modelos_orig_base, function(m) sqrt(diag(vcovHAC(m))))
  stargazer(modelos_orig_base, type = "text", se = erros_hac_orig_base,
            title = "Regressões (Originais s/ Dummies): Erros-Padrão Robustos (HAC)",
            column.labels = nomes_modelos, dep.var.labels = "Eficiência", omit.stat = c("f", "ser"), digits = 4)
} else {
  stargazer(modelos_orig_base, type = "text",
            title = "Resultados das Regressões (Originais s/ Dummies)",
            column.labels = nomes_modelos, dep.var.labels = "Eficiência", omit.stat = c("f", "ser"), digits = 4)
}

# ==========================================================
# REGRESSÕES MÚLTIPLAS (VARIÁVEIS PADRONIZADAS SEM DUMMIES)
# ==========================================================
m1_base <- lm(eficiencia ~ eficiencia_lag1, data = df_intradiario_scaled)
m2_base <- lm(eficiencia ~ eficiencia_lag1 + temp_z, data = df_intradiario_scaled)
m3_base <- lm(eficiencia ~ eficiencia_lag1 + umid_z, data = df_intradiario_scaled)
m4_base <- lm(eficiencia ~ eficiencia_lag1 + temp_z + umid_z , data = df_intradiario_scaled)
m5_base <- lm(eficiencia ~ eficiencia_lag1 + temp_z + umid_z  + interacao_temp_umid_z, data = df_intradiario_scaled)
m6_base <- lm(eficiencia ~ eficiencia_lag1 + temp_z + umid_z  + interacao_temp_umid_z + log(precip + 1), data = df_intradiario_scaled)
m7_base <- lm(eficiencia ~ eficiencia_lag1 + temp_z + umid_z  + interacao_temp_umid_z + log(precip + 1) + vento_z, data = df_intradiario_scaled)

modelos_base <- list(m1_base, m2_base, m3_base, m4_base, m5_base, m6_base, m7_base)

if(verificar_problemas(modelos_base)) {
  erros_hac_base <- lapply(modelos_base, function(m) sqrt(diag(vcovHAC(m))))
  stargazer(modelos_base, type = "text", se = erros_hac_base,
            title = "Regressões (Padronizadas s/ Dummies): Erros-Padrão Robustos (HAC)",
            column.labels = nomes_modelos, dep.var.labels = "Eficiência", omit.stat = c("f", "ser"), digits = 4)
} else {
  stargazer(modelos_base, type = "text",
            title = "Resultados das Regressões (Padronizadas s/ Dummies)",
            column.labels = nomes_modelos, dep.var.labels = "Eficiência", omit.stat = c("f", "ser"), digits = 4)
}

# ==========================================================
# DUMMIES E MODELOS COM EXTREMOS
# ==========================================================
p90 <- quantile(df_intradiario_scaled$temp, 0.90)
df_intradiario_scaled$dummy_extremo <- ifelse(df_intradiario_scaled$temp >= p90, 1, 0)

# 15. Regressões Originais com Dummies
m1_orig <- lm(eficiencia ~ eficiencia_lag1 + dummy_extremo, data = df_intradiario_scaled)
m2_orig <- lm(eficiencia ~ eficiencia_lag1 + dummy_extremo * temp, data = df_intradiario_scaled)
m3_orig <- lm(eficiencia ~ eficiencia_lag1 + dummy_extremo * umid, data = df_intradiario_scaled)
m4_orig <- lm(eficiencia ~ eficiencia_lag1 + dummy_extremo * (temp + umid), data = df_intradiario_scaled)
m5_orig <- lm(eficiencia ~ eficiencia_lag1 + dummy_extremo * (temp + umid + interacao_temp_umid), data = df_intradiario_scaled)
m6_orig <- lm(eficiencia ~ eficiencia_lag1 + dummy_extremo * (temp + umid + interacao_temp_umid + log(precip + 1)), data = df_intradiario_scaled)
m7_orig <- lm(eficiencia ~ eficiencia_lag1 + dummy_extremo * (temp + umid + interacao_temp_umid + log(precip + 1) + vento), data = df_intradiario_scaled)

modelos_lista_orig <- list(m1_orig, m2_orig, m3_orig, m4_orig, m5_orig, m6_orig, m7_orig)

if(verificar_problemas(modelos_lista_orig)) {
  erros_hac_orig <- lapply(modelos_lista_orig, function(m) sqrt(diag(vcovHAC(m))))
  stargazer(modelos_lista_orig, type = "text", se = erros_hac_orig,
            title = "Regressões Originais c/ Extremos: Erros-Padrão Robustos (HAC)",
            column.labels = nomes_modelos, dep.var.labels = "Eficiência", omit.stat = c("f", "ser"), digits = 4)
}

# 16. Regressões Padronizadas com Dummies
m1 <- lm(eficiencia ~ eficiencia_lag1 + dummy_extremo, data = df_intradiario_scaled)
m2 <- lm(eficiencia ~ eficiencia_lag1 + dummy_extremo * temp_z, data = df_intradiario_scaled)
m3 <- lm(eficiencia ~ eficiencia_lag1 + dummy_extremo * umid_z, data = df_intradiario_scaled)
m4 <- lm(eficiencia ~ eficiencia_lag1 + dummy_extremo * (temp_z + umid_z) , data = df_intradiario_scaled)
m5 <- lm(eficiencia ~ eficiencia_lag1 + dummy_extremo * (temp_z + umid_z  + interacao_temp_umid_z), data = df_intradiario_scaled)
m6 <- lm(eficiencia ~ eficiencia_lag1 + dummy_extremo * (temp_z + umid_z  + interacao_temp_umid_z + log(precip + 1)), data = df_intradiario_scaled)
m7 <- lm(eficiencia ~ eficiencia_lag1 + dummy_extremo * (temp_z + umid_z  + interacao_temp_umid_z + log(precip + 1) + vento_z), data = df_intradiario_scaled)

modelos_lista <- list(m1, m2, m3, m4, m5, m6, m7)

if(verificar_problemas(modelos_lista)) {
  erros_hac <- lapply(modelos_lista, function(m) sqrt(diag(vcovHAC(m))))
  stargazer(modelos_lista, type = "text", se = erros_hac,
            title = "Regressões Padronizadas c/ Extremos: Erros-Padrão Robustos (HAC)",
            column.labels = nomes_modelos, dep.var.labels = "Eficiência", omit.stat = c("f", "ser"), digits = 4)
}


# ==========================================================
# MODELO TAR: LIMIARES MANUAIS DE TEMPERATURA (25, 30, 35°C)
# ==========================================================
# Usando a base padronizada onde a defasagem (eficiencia_lag1) já foi tratada
df_tar_scaled <- df_intradiario_scaled %>%
  mutate(
    log_precip = log(precip + 1)
  )

# Os limiares operacionais que você quer testar (em Celsius)
limiares_celsius <- c(25, 30, 35)

# Listas para armazenar os modelos, os erros robustos e os nomes das colunas
modelos_tar   <- list()
erros_tar     <- list()
nomes_colunas <- c()

cat("====================================================\n")
cat(" TAMANHO DAS AMOSTRAS POR REGIME TÉRMICO\n")
cat("====================================================\n")

# Loop para rodar os dois regimes para cada limiar
for (tau in limiares_celsius) {
  
  # 1. Filtra a base pela temperatura original (Celsius)
  df_baixo <- df_tar_scaled %>% filter(temp <= tau)
  df_alto  <- df_tar_scaled %>% filter(temp > tau)
  
  # Imprime o tamanho da amostra para conferência
  cat(sprintf("Limiar %d°C -> Regime Frio (<= %d°C): %d horas | Regime Quente (> %d°C): %d horas\n", 
              tau, tau, nrow(df_baixo), tau, nrow(df_alto)))
  
  # 2. Estima os modelos usando as variáveis padronizadas (Z-score) e a lag pré-calculada
  mod_baixo <- lm(eficiencia ~ eficiencia_lag1 + temp_z + umid_z + vento_z + 
                    log_precip + interacao_temp_umid_z, data = df_baixo)
  
  mod_alto  <- lm(eficiencia ~ eficiencia_lag1 + temp_z + umid_z + vento_z + 
                    log_precip + interacao_temp_umid_z, data = df_alto)
  
  # 3. Salva os modelos na lista com nomes identificadores
  nome_baixo <- paste0("T", tau, "_Baixo")
  nome_alto  <- paste0("T", tau, "_Alto")
  
  modelos_tar[[nome_baixo]] <- mod_baixo
  modelos_tar[[nome_alto]]  <- mod_alto
  
  # 4. Calcula e salva os Erros-Padrão Robustos (HAC)
  erros_tar[[nome_baixo]] <- sqrt(diag(vcovHAC(mod_baixo)))
  erros_tar[[nome_alto]]  <- sqrt(diag(vcovHAC(mod_alto)))
  
  # 5. Prepara os rótulos para a tabela final
  nomes_colunas <- c(nomes_colunas, paste("<=", tau, "°C"), paste(">", tau, "°C"))
}

cat("\nGerando a tabela comparativa...\n\n")


# ==========================================================
# TABELA CONSOLIDADA (6 COLUNAS)
# ==========================================================
stargazer(modelos_tar,
          type = "text",
          title = "Modelos TAR (Z-score): Limiares Operacionais de Temperatura (Correção HAC)",
          se = erros_tar,
          column.labels = nomes_colunas,
          dep.var.labels = "Eficiência Horária",
          covariate.labels = c("Eficiência (t-1)", "Temp (Z)", "Umidade (Z)", "Vento (Z)", 
                               "Log(Precip + 1)", "Interação Temp*Umid (Z)"),
          omit.stat = c("f", "ser"),
          notes = "Nota: Erros-padrão robustos (HAC / Newey-West) entre parênteses.",
          digits = 5)


# ==========================================================
# PREPARAÇÃO DA BASE PADRONIZADA (Z-SCORE)
# ==========================================================

# 1. Base Padronizada (Conforme seu código) + Criação de Lag e Log
df_tar_scaled <- intradiario %>%
  arrange(DateTime) %>%
  mutate(
    eficiencia = eficiencia_horaria,
    # Padronização (média 0, desvio padrão 1)
    temp_z         = as.numeric(scale(temp)),
    umid_z         = as.numeric(scale(umid)),
    vento_z        = as.numeric(scale(vento)),
    # Variáveis que mantêm a escala ou sofrem outra transformação
    log_precip     = log(precip + 1)
  ) %>%
  mutate(
    # Interação calculada a partir das variáveis padronizadas
    interacao_temp_umid_z = temp_z * umid_z,
    eficiencia_lag1 = lag(eficiencia)
  ) %>%
  na.omit()

# ==========================================================
# MODELO TAR (Threshold na Temperatura Z-Score) COM HAC
# ==========================================================

# Definir o Grid de Busca para a Temperatura Padronizada
limite_inf <- quantile(df_tar_scaled$temp_z, 0.05)
limite_sup <- quantile(df_tar_scaled$temp_z, 0.95)

# O "passo" cai de 0.2 para 0.05 porque estamos em desvios-padrões
grid_threshold <- seq(limite_inf, limite_sup, by = 0.05)

# Loop de Busca (Minimização do SSR)
melhor_ssr <- Inf
melhor_tau <- NA
melhor_modelo_baixo <- NULL
melhor_modelo_alto  <- NULL

cat("Calculando modelo TAR... Buscando threshold ideal em temp_z...\n")

for (tau in grid_threshold) {
  
  # Divide a amostra baseada no Z-score da Temperatura
  df_baixo <- df_tar_scaled %>% filter(temp_z <= tau)
  df_alto  <- df_tar_scaled %>% filter(temp_z > tau)
  
  # Estima os modelos dinâmicos com as variáveis padronizadas
  mod_baixo <- lm(eficiencia ~ eficiencia_lag1 + temp_z + umid_z + vento_z + 
                    log_precip + interacao_temp_umid_z, data = df_baixo)
  
  mod_alto  <- lm(eficiencia ~ eficiencia_lag1 + temp_z + umid_z + vento_z + 
                    log_precip + interacao_temp_umid_z, data = df_alto)
  
  # Soma dos Quadrados dos Resíduos
  ssr_total <- sum(mod_baixo$residuals^2) + sum(mod_alto$residuals^2)
  
  # Atualiza o melhor modelo
  if (ssr_total < melhor_ssr) {
    melhor_ssr <- ssr_total
    melhor_tau <- tau
    melhor_modelo_baixo <- mod_baixo
    melhor_modelo_alto  <- mod_alto
  }
}

# ==========================================================
# RESULTADOS DO MODELO TAR (Z-SCORE)
# ==========================================================

# Calcula os Erros-Padrão Robustos (Newey-West) para os regimes finais
erros_hac_baixo <- sqrt(diag(vcovHAC(melhor_modelo_baixo)))
erros_hac_alto  <- sqrt(diag(vcovHAC(melhor_modelo_alto)))

cat("\n====================================================\n")
cat(" RESULTADO TAR: THRESHOLD EM TEMPERATURA PADRONIZADA\n")
cat("====================================================\n")
cat("Threshold Encontrado (Tau em Z-score):", round(melhor_tau, 3), "desvios padrões\n")
cat("Tamanho da Amostra Regime 1 (temp_z <=", round(melhor_tau, 3), "):", nrow(melhor_modelo_baixo$model), "horas\n")
cat("Tamanho da Amostra Regime 2 (temp_z >", round(melhor_tau, 3), "):", nrow(melhor_modelo_alto$model), "horas\n\n")

# Tabela Comparativa Stargazer injetando os erros HAC
stargazer(melhor_modelo_baixo, melhor_modelo_alto,
          type = "text",
          title = "Modelo TAR (Padronizado): Comparação de Regimes (Correção HAC)",
          se = list(erros_hac_baixo, erros_hac_alto),
          column.labels = c(paste("Regime 1 (temp_z <=", round(melhor_tau, 2), ")"),
                            paste("Regime 2 (temp_z >", round(melhor_tau, 2), ")")),
          dep.var.labels = "Eficiência Horária",
          covariate.labels = c("Eficiência (t-1)", "Temp (Z)", "Umidade (Z)", "Vento (Z)", 
                               "Log(Precip + 1)", "Interação Temp*Umid (Z)"),
          omit.stat = c("f", "ser"),
          notes = "Nota: Erros-padrão robustos (HAC / Newey-West) entre parênteses.",
          digits = 6)

