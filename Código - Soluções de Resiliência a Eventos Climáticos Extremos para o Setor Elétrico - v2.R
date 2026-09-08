############################################################
# Soluções de Resiliência a Eventos Climáticos             #
# Extremos para o Setor Elétrico                           #
# Nomes: Enrico C. Mira                                    #
#        Leandro S. Maciel                                 #
# Data: 31/03/2026                                         #
############################################################

# Limpeza de ambiente 
rm(list=ls()); graphics.off()

# ==========================================================
# 1. PACOTES E AMBIENTE
# ==========================================================
pacotes <- c("readxl", "dplyr", "lubridate", "ggplot2", "GGally", "tidyr", "stargazer", "FinTS","lmtest", "sandwich", "tseries")

for (p in pacotes) {
  if (!require(p, character.only = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}

# ==========================================================
# 2. IMPORTAÇÃO DOS DADOS
# ==========================================================
#setwd("~/Desktop/Produto #4 Acende/Analises R")
#arquivo <- "~/Desktop/Produto #4 Acende/Analises R/dados_v2.xlsx"
arquivo <- "C:/Users/enric/Downloads/dados_v2.xlsx"

df_usina      <- read_excel(arquivo, sheet = "dados usina (geracao e clima)")
df_clima      <- read_excel(arquivo, sheet = "dados clima adicionais")
df_gas        <- read_excel(arquivo, sheet = "dados consumo gas")
df_gas_MMBTU<- read_excel("C:/Users/enric/Downloads/Dados (1).xlsx", 
              sheet = "dados do gas natural consumido", 
              range = "A1:G745") %>% select("Data", `Energia (MMBTU)`) %>% 
              filter(`Energia (MMBTU)`>0)


# ==========================================================
# 3. TRATAMENTO PARA BASE INTRADIÁRIA (HORÁRIA)
# ==========================================================

df_gas <- df_gas %>%
  mutate(Data = as.Date(DataHora)) %>%   # 1. extrai a data
  group_by(Data) %>%
  mutate(soma_dia = sum(Valor, na.rm = TRUE)) %>%  # 2. soma diária
  ungroup() %>%
  left_join(
    df_gas_MMBTU %>% mutate(Data = as.Date(Data)),
    by = "Data"
  ) %>%  # 3. junta com energia diária
  mutate(
    Valor = `Energia (MMBTU)` * Valor / soma_dia  # 4. redistribui
  ) %>%
  select(DataHora, Valor)  # 5. formato final

# 3.1. Usina: Variáveis Climáticas (Horárias)
df_usina_clima <- df_usina %>%
  mutate(
    DateTime = as.POSIXct(DateTime, format="%m/%d/%y %H:%M:%S"),
    temp     = as.numeric(`Temperatura ambiente (°C)`),
    umid     = as.numeric(`Umidade Relativa do ar (%)`),
    geracao_energia = as.numeric(`Geração (MW)`)
  ) %>%
  select(DateTime, temp, umid,geracao_energia)

# 3.3. Clima Adicional (Horário)
df_clima_horario <- df_clima %>%
  mutate(
    DateTime   = dmy_hm(Data),
    precip_num = coalesce(as.numeric(`Precipitação [mm]`), as.numeric(`...5`)),
    vento_num  = as.numeric(`Velocidade do Vento [m/s]`)
  ) %>%
  select(DateTime, vento = vento_num, precip = precip_num)

# 3.4. Gás Natural (Interpolação Diária para Horária)
colnames(df_gas) <- c("DateTime", "gas_consumido")

# ==========================================================
# 4. JUNÇÃO E ENGENHARIA (DF INTRADIÁRIO)
# ==========================================================

df_intradiario <- df_usina_clima %>%
  full_join(df_gas, by = "DateTime") %>%
  full_join(df_clima_horario, by = "DateTime") %>%
  filter(gas_consumido  > 0) %>%
  filter(geracao_energia > 0) %>% # Coluna onde eliminamos dias sem produção
  mutate(
    # Eficiência horária (fração) e interações para análise de alta frequência
    eficiencia_horaria  = (geracao_energia * 3.6) / (gas_consumido * 1.05506),
    interacao_temp_umid = temp * umid
  ) %>%
  filter(geracao_energia > 441)#%>%
  #filter(hour(DateTime) >= 9 & hour(DateTime) <= 16)


# ==========================================================
# 6. ESTATÍSTICAS DESCRITIVAS DOS DADOS DIÁRIOS
# ==========================================================

# Preparar os dados para o stargazer (removendo a Data e a precip bruta para focar no prep)
df_descritiva <- df_intradiario %>%
  select(-DateTime) %>% 
  as.data.frame()

# Gerar a tabela diretamente no Console
stargazer(df_descritiva, 
          type = "text", 
          summary = TRUE, 
          title = "Estatísticas Descritivas - Base Diária Agregada",
          digits = 4, 
          covariate.labels = c("Temperatura (°C)", "Umidade (%)",
                               "Geração Bruta (MW)",
                               "Quantidade do Gás Consumido (MMBTU)", "Vento (m/s)","Precipitação ", "Eficiência", 
                               "Interação (Temp x Umid)"))

# ==========================================================
# 7. VISUALIZAÇÃO DAS SÉRIES TEMPORAIS (HORÁRIAS)
# ==========================================================

# 1. Preparar os dados para um gráfico multi-painel (Long Format)
# Usando 'prep' ao invés de 'precip'
df_long_viz <- df_intradiario %>%
  pivot_longer(
    cols = c(temp, umid, geracao_energia, gas_consumido , vento, precip, eficiencia_horaria, interacao_temp_umid),
    names_to = "Variavel",
    values_to = "Valor"
  ) %>%
  mutate(
    Variavel_Rótulo = factor(Variavel, 
                             levels = c("temp", "umid", 'geracao_energia', "gas_consumido" , "vento", "precip", "eficiencia_horaria", "interacao_temp_umid"),
                             labels = c("Temperatura (°C)", "Umidade (%)",
                                        "Geração Bruta (MW)",
                                        "Qualidade do Gás Consumido (MMBTU)", "Vento (m/s)","Precipitação ", "Eficiência", 
                                        "Interação (Temp x Umid)"))
  )

# 2. Gerar o Gráfico de Séries Temporais Empilhadas
ggplot(df_long_viz, aes(x = DateTime, y = Valor)) +
  geom_line(color = "darkblue", size = 0.5) +
  facet_wrap(~ Variavel_Rótulo, scales = "free_y", ncol = 1) +
  theme_classic(base_size = 14) +
  theme(
    strip.text.y = element_text(angle = 0, face = "bold"),
    axis.title.y = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold"),
    strip.background = element_rect(fill = "gray95", color = "white")
  ) +
  labs(
    title = "Séries Temporais das Variáveis Operacionais e Climáticas",
    x = "Tempo"
  )

# =========================================================
# 4. TESTES DE ESTACIONARIEDADE (ADF e PP) - HORÁRIO E DIÁRIO
# =========================================================

# 1. Função generalizada para testar todas as variáveis de um dataframe
gerar_tabela_estacionariedade <- function(df, nome_base) {
  
  # Definindo as variáveis que serão testadas e seus nomes de exibição
  variaveis <-  c("temp", "umid", 'geracao_energia', "gas_consumido" , "vento", "precip", "eficiencia_horaria", "interacao_temp_umid")
  nomes_ex <- c("Temperatura (°C)", "Umidade (%)",
                "Geração Bruta (MW)",
                "Qualidade do Gás Consumido (MMBTU)", "Vento (m/s)","Precipitação", "Eficiência", 
                "Interação (Temp x Umid)")
  
  # Loop (lapply) para aplicar os dois testes em cada variável
  resultados <- lapply(seq_along(variaveis), function(i) {
    serie_limpa <- na.omit(df[[variaveis[i]]])
    
    # Executa os testes (suprimindo os warnings normais de p-valor tabelado)
    teste_adf <- suppressWarnings(adf.test(serie_limpa, alternative = "stationary", k = 1))
    teste_pp  <- suppressWarnings(pp.test(serie_limpa, alternative = "stationary"))
    
    # Retorna uma linha do dataframe
    data.frame(
      Variavel = nomes_ex[i],
      ADF_Estat = round(teste_adf$statistic, 4),
      ADF_P_Valor = round(teste_adf$p.value, 4),
      ADF_Result = ifelse(teste_adf$p.value <= 0.05, "Estacionária", "Não Estacionária"),
      PP_Estat = round(teste_pp$statistic, 4),
      PP_P_Valor = round(teste_pp$p.value, 4),
      PP_Result = ifelse(teste_pp$p.value <= 0.05, "Estacionária", "Não Estacionária"),
      stringsAsFactors = FALSE
    )
  })
  
  # Junta todas as linhas em uma única tabela
  tabela_final <- do.call(rbind, resultados)
  
  # Imprime o cabeçalho personalizado
  cat("\n====================================================================\n")
  cat(" Resumo dos Testes de Estacionariedade (ADF e PP) - Base:", nome_base, "\n")
  cat("====================================================================\n")
  print(tabela_final, row.names = FALSE)
  
  return(tabela_final)
}

# 2. Gerar e exibir a tabela para os dados HORÁRIOS
tabela_estacionariedade_horaria <- gerar_tabela_estacionariedade(df_intradiario, "Horário")

# ==========================================================
# 5. MODELOS E TESTES
# ==========================================================

# 5.1. Engenharia de Variáveis e Padronização (Z-score)
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

# ==========================================================
# 6. ANÁLISE DE AUTOCORRELAÇÃO DA VARIÁVEL DEPENDENTE
# ==========================================================

# Inspeção visual das funções de autocorrelação (ACF) e autocorrelação parcial (PACF).
# Como não há autocorrelação significativa, a especificação de um modelo estático é adequada.
acf(df_intradiario$eficiencia_horaria, main = "ACF da Eficiência")
pacf(df_intradiario$eficiencia_horaria, main = "PACF da Eficiência")


# ==========================================================
# 7. REGRESSÕES MULTIPLAS (VARIÁVEIS ORIGINAIS)
# ==========================================================

# 7.1. Estimação dos Modelos (Níveis Originais)
# Grupo 1: Modelos Aditivos (Sem a Interação)

m1_orig <- lm(eficiencia ~ lag(eficiencia), data = df_intradiario_scaled)
m2_orig <- lm(eficiencia ~ lag(eficiencia) + temp, data = df_intradiario_scaled)
m3_orig <- lm(eficiencia ~ lag(eficiencia) + umid, data = df_intradiario_scaled)
m4_orig <- lm(eficiencia ~ lag(eficiencia) + temp + umid, data = df_intradiario_scaled)
m5_orig <- lm(eficiencia ~ lag(eficiencia) + temp + umid + interacao_temp_umid, data = df_intradiario_scaled)
m6_orig <- lm(eficiencia ~ lag(eficiencia) + temp + umid + interacao_temp_umid + log(precip + 1), data = df_intradiario_scaled)
m7_orig <- lm(eficiencia ~ lag(eficiencia) + temp + umid + interacao_temp_umid + log(precip + 1) + vento, data = df_intradiario_scaled)

# 7.2 Testes de Diagnóstico dos Resíduos (Originais) - BLINDADOS
modelos_lista_orig <- list(m1_orig, m2_orig, m3_orig, m4_orig, m5_orig, m6_orig, m7_orig)

# Aplicando unname() e as.numeric() para extrair os números puros
bg_resultados_orig <- unname(sapply(modelos_lista_orig, function(m) as.numeric(bgtest(m)$statistic)))
bg_p_valores_orig  <- unname(sapply(modelos_lista_orig, function(m) as.numeric(bgtest(m)$p.value)))

bp_resultados_orig <- unname(sapply(modelos_lista_orig, function(m) as.numeric(bptest(m)$statistic)))
bp_p_valores_orig  <- unname(sapply(modelos_lista_orig, function(m) as.numeric(bptest(m)$p.value)))

arch_resultados_orig <- unname(sapply(modelos_lista_orig, function(m) as.numeric(ArchTest(resid(m), lags = 1)$statistic)))
arch_p_valores_orig  <- unname(sapply(modelos_lista_orig, function(m) as.numeric(ArchTest(resid(m), lags = 1)$p.value)))

# 7.3 Gerar Tabela Comparativa - Formatando tudo como texto puro (sprintf)
linhas_diagnosticos_orig <- list(
  c("Breusch-Godfrey stat", sprintf("%.3f", bg_resultados_orig)),
  c("BG p-value",           sprintf("%.3f", bg_p_valores_orig)),
  c("Breusch-Pagan stat",   sprintf("%.3f", bp_resultados_orig)),
  c("BP p-value",           sprintf("%.3f", bp_p_valores_orig)),
  c("ARCH-LM stat",         sprintf("%.3f", arch_resultados_orig)),
  c("ARCH p-value",         sprintf("%.3f", arch_p_valores_orig))
)

# Truque para driblar o bug de limite de caracteres do stargazer (deparse cutoff)
x1  <- m1_orig
x2  <- m2_orig
x3  <- m3_orig
x4  <- m4_orig
x5  <- m5_orig
x6  <- m6_orig
x7  <- m7_orig

nomes_modelos <- c("M1", "M2", "M3", "M4", "M5", "M6", "M7")

# Agora a chamada tem menos de 60 caracteres e o R não vai quebrar a string!
stargazer(x1, x2, x3, x4, x5, x6, x7,
          type = "text",
          title = "Resultados das Regressões (Variáveis Originais)",
          column.labels = nomes_modelos,
          dep.var.labels = "Eficiência", # Corrigido para escala em nível
          covariate.labels = c("Eficiência (t-1)", "Temperatura", "Umidade", "Interação (Temp x Umid)", "Precipitação (log)", "Vento", "Constate"),
          add.lines = linhas_diagnosticos_orig,
          omit.stat = c("f", "ser"),
          digits = 4)

# =========================================================
# 9. MODELOS ESTIMADOS COM ERROS-PADRÃO ROBUSTOS (HAC)
# =========================================================

# 2. Criar a lista com TODOS os 11 modelos já estimados
modelos_lista <- modelos_lista_orig

# 3. Calcular os Erros-Padrão Robustos (HAC / Newey-West) para cada modelo
# A função vcovHAC cria a nova matriz de covariância.
# Extraímos a diagonal e tiramos a raiz quadrada para obter os novos erros-padrão.
erros_hac <- lapply(modelos_lista, function(modelo) sqrt(diag(vcovHAC(modelo))))


stargazer(x1, x2, x3, x4, x5, x6, x7,
          type = "text",
          se = erros_hac, # <-- Injeta os erros-padrão robustos aqui
          title = "Resultados das Regressões (Variáveis Originais): Erros-Padrão Robustos (HAC)",
          column.labels = nomes_modelos,
          dep.var.labels = "Eficiência", # Corrigido para escala em nível
          covariate.labels = c("Eficiência (t-1)", "Temperatura", "Umidade", "Interação (Temp x Umid)", "Precipitação (log)", "Vento", "Constate"),
          omit.stat = c("f", "ser"),
          notes = "Nota: Erros-padrão robustos (HAC / Newey-West) entre parênteses.",
          digits = 4) # Usando 4 casas para a tabela caber inteira no console



# ==========================================================
# 8. REGRESSÕES MULTIPLAS (VARIÁVEIS PADRONIZADAS)
# ==========================================================

# 8.1. Estimação dos Modelos (Z-score)
# Grupo 1: Sem a Interação
m1 <- lm(eficiencia ~ lag(eficiencia), data = df_intradiario_scaled)
m2 <- lm(eficiencia ~ lag(eficiencia) + temp_z, data = df_intradiario_scaled)
m3 <- lm(eficiencia ~ lag(eficiencia) + umid_z, data = df_intradiario_scaled)
m4 <- lm(eficiencia ~ lag(eficiencia) + temp_z + umid_z , data = df_intradiario_scaled)
m5 <- lm(eficiencia ~ lag(eficiencia) + temp_z + umid_z  + interacao_temp_umid_z, data = df_intradiario_scaled)
m6 <- lm(eficiencia ~ lag(eficiencia) + temp_z + umid_z  + interacao_temp_umid_z + log(precip + 1), data = df_intradiario_scaled)
m7 <- lm(eficiencia ~ lag(eficiencia) + temp_z + umid_z  + interacao_temp_umid_z + log(precip + 1) + vento, data = df_intradiario_scaled)

# 8.2. Testes de Diagnóstico dos Resíduos (Padronizados)
modelos_lista <- list(m1, m2, m3, m4, m5, m6, m7)

# A. Teste de Autocorrelação de Breusch-Godfrey
bg_resultados <- sapply(modelos_lista, function(m) bgtest(m)$statistic)
bg_p_valores  <- sapply(modelos_lista, function(m) bgtest(m)$p.value)

# B. Teste de Heterocedasticidade de Breusch-Pagan
bp_resultados <- sapply(modelos_lista, function(m) bptest(m)$statistic)
bp_p_valores  <- sapply(modelos_lista, function(m) bptest(m)$p.value)

# C. Teste ARCH-LM
arch_resultados <- sapply(modelos_lista, function(m) ArchTest(resid(m), lags = 1)$statistic)
arch_p_valores  <- sapply(modelos_lista, function(m) ArchTest(resid(m), lags = 1)$p.value)

# 8.3. Tabela Comparativa Stargazer (Padronizados)
linhas_diagnosticos <- list(
  c("Breusch-Godfrey stat", round(bg_resultados, 3)),
  c("BG p-value",           round(bg_p_valores, 3)),
  c("Breusch-Pagan stat",   round(bp_resultados, 3)),
  c("BP p-value",           round(bp_p_valores, 3)),
  c("ARCH-LM stat",         round(arch_resultados, 3)),
  c("ARCH p-value",         round(arch_p_valores, 3))
)

stargazer(m1, m2, m3, m4, m5, m6, m7,
          type = "text",
          title = "Resultados das Regressões (Variáveis Padronizadas)",
          column.labels = nomes_modelos,
          dep.var.labels = "Eficiência", # Corrigido para escala em nível
          covariate.labels = c("Eficiência (t-1)", "Temperatura", "Umidade", "Interação (Temp x Umid)", "Precipitação (log)", "Vento", "Constate"),
          add.lines = linhas_diagnosticos,
          omit.stat = c("f", "ser"),
          digits = 4)

# =========================================================
# 9. MODELOS ESTIMADOS COM ERROS-PADRÃO ROBUSTOS (HAC)
# =========================================================

# 2. Criar a lista com TODOS os 11 modelos já estimados
modelos_lista <- list(m1, m2, m3, m4, m5, m6, m7)

# 3. Calcular os Erros-Padrão Robustos (HAC / Newey-West) para cada modelo
# A função vcovHAC cria a nova matriz de covariância.
# Extraímos a diagonal e tiramos a raiz quadrada para obter os novos erros-padrão.
erros_hac <- lapply(modelos_lista, function(modelo) sqrt(diag(vcovHAC(modelo))))


stargazer(m1, m2, m3, m4, m5, m6, m7,
          type = "text",
          se = erros_hac, # <-- Injeta os erros-padrão robustos aqui
          title = "Resultados das Regressões (Padronizadas): Erros-Padrão Robustos (HAC)",
          column.labels = nomes_modelos,
          dep.var.labels = "Eficiência", # Corrigido para escala em nível
          covariate.labels = c("Eficiência (t-1)", "Temperatura", "Umidade", "Interação (Temp x Umid)", "Precipitação (log)", "Vento", "Constate"),
          omit.stat = c("f", "ser"),
          notes = "Nota: Erros-padrão robustos (HAC / Newey-West) entre parênteses.",
          digits = 4) # Usando 4 casas para a tabela caber inteira no console














































# ==========================================================
# 6.5. CRIAÇÃO DAS DUMMIES DE EXTREMOS CLIMÁTICOS (TEMPERATURA)
# ==========================================================

# 1. Calcular os limites dos percentis de temperatura
p90 <- quantile(df_intradiario_scaled$temp, 0.90, na.rm = TRUE)
p95 <- quantile(df_intradiario_scaled$temp, 0.95, na.rm = TRUE)
p99 <- quantile(df_intradiario_scaled$temp, 0.99, na.rm = TRUE)

# 2. Criar as variáveis dummy no dataframe baseadas na temperatura
df_intradiario_scaled <- df_intradiario_scaled %>%
  mutate(
    dummy_t90 = ifelse(temp >= p90, 1, 0),
    dummy_t95 = ifelse(temp >= p95, 1, 0),
    dummy_t99 = ifelse(temp >= p99, 1, 0)
  )

# 3. Definir a dummy "ativa" para a rodada principal (M1 a M7)
# Altere para dummy_t95 ou dummy_t99 conforme a análise que desejar focar
df_intradiario_scaled$dummy_extremo <- df_intradiario_scaled$dummy_t90


# ==========================================================
# 7. REGRESSÕES MULTIPLAS (VARIÁVEIS ORIGINAIS + EXTREMOS)
# ==========================================================

# 7.1. Estimação dos Modelos (Níveis Originais)
m1_orig <- lm(eficiencia ~ lag(eficiencia) + dummy_extremo, data = df_intradiario_scaled)
m2_orig <- lm(eficiencia ~ lag(eficiencia) + dummy_extremo * (temp), data = df_intradiario_scaled)
m3_orig <- lm(eficiencia ~ lag(eficiencia) + dummy_extremo * (umid), data = df_intradiario_scaled)
m4_orig <- lm(eficiencia ~ lag(eficiencia) + dummy_extremo * (temp + umid), data = df_intradiario_scaled)
m5_orig <- lm(eficiencia ~ lag(eficiencia) + dummy_extremo * (temp + umid + interacao_temp_umid), data = df_intradiario_scaled)
m6_orig <- lm(eficiencia ~ lag(eficiencia) + dummy_extremo * (temp + umid + interacao_temp_umid + log(precip + 1)), data = df_intradiario_scaled)
m7_orig <- lm(eficiencia ~ lag(eficiencia) + dummy_extremo * (temp + umid + interacao_temp_umid + log(precip + 1) + vento), data = df_intradiario_scaled)

# ==========================================================
# 7.2 Testes de Diagnóstico dos Resíduos (Originais)
# COM TRATAMENTO DE ERRO (tryCatch) PARA MATRIZES SINGULARES
# ==========================================================
modelos_lista_orig <- list(m1_orig, m2_orig, m3_orig, m4_orig, m5_orig, m6_orig, m7_orig)

# Funções auxiliares seguras: se o teste quebrar, retorna NA ao invés de abortar o script
safe_bg_stat <- function(m) tryCatch(as.numeric(bgtest(m)$statistic), error = function(e) NA)
safe_bg_pval <- function(m) tryCatch(as.numeric(bgtest(m)$p.value), error = function(e) NA)

safe_bp_stat <- function(m) tryCatch(as.numeric(bptest(m)$statistic), error = function(e) NA)
safe_bp_pval <- function(m) tryCatch(as.numeric(bptest(m)$p.value), error = function(e) NA)

safe_arch_stat <- function(m) tryCatch(as.numeric(ArchTest(resid(m), lags = 1)$statistic), error = function(e) NA)
safe_arch_pval <- function(m) tryCatch(as.numeric(ArchTest(resid(m), lags = 1)$p.value), error = function(e) NA)

# Aplicação dos testes usando as funções seguras
bg_resultados_orig <- unname(sapply(modelos_lista_orig, safe_bg_stat))
bg_p_valores_orig  <- unname(sapply(modelos_lista_orig, safe_bg_pval))

bp_resultados_orig <- unname(sapply(modelos_lista_orig, safe_bp_stat))
bp_p_valores_orig  <- unname(sapply(modelos_lista_orig, safe_bp_pval))

arch_resultados_orig <- unname(sapply(modelos_lista_orig, safe_arch_stat))
arch_p_valores_orig  <- unname(sapply(modelos_lista_orig, safe_arch_pval))

# ==========================================================
# 7.3 Gerar Tabela Comparativa Original
# ==========================================================
# Mantida apenas a versão segura (com ifelse) para evitar sobrescrever com a versão falha
linhas_diagnosticos_orig <- list(
  c("Breusch-Godfrey stat", ifelse(is.na(bg_resultados_orig), "NA", sprintf("%.3f", bg_resultados_orig))),
  c("BG p-value",           ifelse(is.na(bg_p_valores_orig), "NA", sprintf("%.3f", bg_p_valores_orig))),
  c("Breusch-Pagan stat",   ifelse(is.na(bp_resultados_orig), "NA", sprintf("%.3f", bp_resultados_orig))),
  c("BP p-value",           ifelse(is.na(bp_p_valores_orig), "NA", sprintf("%.3f", bp_p_valores_orig))),
  c("ARCH-LM stat",         ifelse(is.na(arch_resultados_orig), "NA", sprintf("%.3f", arch_resultados_orig))),
  c("ARCH p-value",         ifelse(is.na(arch_p_valores_orig), "NA", sprintf("%.3f", arch_p_valores_orig)))
)

nomes_modelos <- c("M1", "M2", "M3", "M4", "M5", "M6", "M7")

stargazer(m1_orig, m2_orig, m3_orig, m4_orig, m5_orig, m6_orig, m7_orig,
          type = "text",
          title = "Resultados das Regressões (Variáveis Originais + Interação Extremos)",
          column.labels = nomes_modelos,
          dep.var.labels = "Eficiência", 
          add.lines = linhas_diagnosticos_orig,
          omit.stat = c("f", "ser"),
          digits = 4)

# =========================================================
# 7.4 MODELOS ESTIMADOS COM ERROS-PADRÃO ROBUSTOS (HAC) - ORIGINAIS
# =========================================================

erros_hac_orig <- lapply(modelos_lista_orig, function(modelo) sqrt(diag(vcovHAC(modelo))))

stargazer(m1_orig, m2_orig, m3_orig, m4_orig, m5_orig, m6_orig, m7_orig,
          type = "text",
          se = erros_hac_orig,
          title = "Regressões Originais: Erros-Padrão Robustos (HAC / Newey-West)",
          column.labels = nomes_modelos,
          dep.var.labels = "Eficiência", 
          omit.stat = c("f", "ser"),
          notes = "Nota: Erros-padrão robustos (HAC) entre parênteses.",
          digits = 4)


# ==========================================================
# 8. REGRESSÕES MULTIPLAS (VARIÁVEIS PADRONIZADAS + EXTREMOS)
# ==========================================================

# 8.1. Estimação dos Modelos (Z-score)
m1 <- lm(eficiencia ~ lag(eficiencia) + dummy_extremo, data = df_intradiario_scaled)
m2 <- lm(eficiencia ~ lag(eficiencia) + dummy_extremo * (temp_z), data = df_intradiario_scaled)
m3 <- lm(eficiencia ~ lag(eficiencia) + dummy_extremo * (umid_z), data = df_intradiario_scaled)
m4 <- lm(eficiencia ~ lag(eficiencia) + dummy_extremo * (temp_z + umid_z) , data = df_intradiario_scaled)
m5 <- lm(eficiencia ~ lag(eficiencia) + dummy_extremo * (temp_z + umid_z  + interacao_temp_umid_z), data = df_intradiario_scaled)
m6 <- lm(eficiencia ~ lag(eficiencia) + dummy_extremo * (temp_z + umid_z  + interacao_temp_umid_z + log(precip + 1)), data = df_intradiario_scaled)
m7 <- lm(eficiencia ~ lag(eficiencia) + dummy_extremo * (temp_z + umid_z  + interacao_temp_umid_z + log(precip + 1) + vento_z), data = df_intradiario_scaled)

# ==========================================================
# 8.2 Testes de Diagnóstico dos Resíduos (Padronizados)
# COM TRATAMENTO DE ERRO (tryCatch) PARA MATRIZES SINGULARES
# ==========================================================
modelos_lista <- list(m1, m2, m3, m4, m5, m6, m7)

bg_resultados <- unname(sapply(modelos_lista, safe_bg_stat))
bg_p_valores  <- unname(sapply(modelos_lista, safe_bg_pval))

bp_resultados <- unname(sapply(modelos_lista, safe_bp_stat))
bp_p_valores  <- unname(sapply(modelos_lista, safe_bp_pval))

arch_resultados <- unname(sapply(modelos_lista, safe_arch_stat))
arch_p_valores  <- unname(sapply(modelos_lista, safe_arch_pval))

# ==========================================================
# 8.3 Tabela Comparativa Stargazer (Padronizados)
# ==========================================================
linhas_diagnosticos <- list(
  c("Breusch-Godfrey stat", ifelse(is.na(bg_resultados), "NA", sprintf("%.3f", bg_resultados))),
  c("BG p-value",           ifelse(is.na(bg_p_valores), "NA", sprintf("%.3f", bg_p_valores))),
  c("Breusch-Pagan stat",   ifelse(is.na(bp_resultados), "NA", sprintf("%.3f", bp_resultados))),
  c("BP p-value",           ifelse(is.na(bp_p_valores), "NA", sprintf("%.3f", bp_p_valores))),
  c("ARCH-LM stat",         ifelse(is.na(arch_resultados), "NA", sprintf("%.3f", arch_resultados))),
  c("ARCH p-value",         ifelse(is.na(arch_p_valores), "NA", sprintf("%.3f", arch_p_valores)))
)

z1 <- m1; z2 <- m2; z3 <- m3; z4 <- m4; z5 <- m5; z6 <- m6; z7 <- m7
nomes_modelos <- c("M1", "M2", "M3", "M4", "M5", "M6", "M7")

stargazer(z1, z2, z3, z4, z5, z6, z7,
          type = "text",
          title = "Resultados das Regressões (Variáveis Padronizadas + Interação Extremos)",
          column.labels = nomes_modelos,
          dep.var.labels = "Eficiência", 
          add.lines = linhas_diagnosticos,
          omit.stat = c("f", "ser"),
          digits = 4)

# =========================================================
# 9. MODELOS ESTIMADOS COM ERROS-PADRÃO ROBUSTOS (HAC) - PADRONIZADOS
# =========================================================

erros_hac <- lapply(modelos_lista, function(modelo) sqrt(diag(vcovHAC(modelo))))

stargazer(m1, m2, m3, m4, m5, m6, m7,
          type = "text",
          se = erros_hac,
          title = "Regressões Padronizadas: Erros-Padrão Robustos (HAC / Newey-West)",
          column.labels = nomes_modelos,
          dep.var.labels = "Eficiência", 
          omit.stat = c("f", "ser"),
          notes = "Nota: Erros-padrão robustos (HAC) entre parênteses.",
          digits = 4)

# =========================================================
# 10. ANÁLISE DE SENSIBILIDADE: COMPARAÇÃO DOS PERCENTIS (90, 95 e 99)
# =========================================================

m_full_90 <- lm(eficiencia ~ lag(eficiencia) + dummy_t90 * (temp + umid + interacao_temp_umid + log(precip + 1) + vento), data = df_intradiario_scaled)
m_full_95 <- lm(eficiencia ~ lag(eficiencia) + dummy_t95 * (temp + umid + interacao_temp_umid + log(precip + 1) + vento), data = df_intradiario_scaled)
m_full_99 <- lm(eficiencia ~ lag(eficiencia) + dummy_t99 * (temp + umid + interacao_temp_umid + log(precip + 1) + vento), data = df_intradiario_scaled)

# Cálculo HAC para os modelos de sensibilidade
erros_hac_sensibilidade <- lapply(list(m_full_90, m_full_95, m_full_99), function(modelo) sqrt(diag(vcovHAC(modelo))))

stargazer(m_full_90, m_full_95, m_full_99,
          type = "text",
          se = erros_hac_sensibilidade,
          title = "Sensibilidade aos Percentis de Temperatura (Modelo Completo com HAC)",
          column.labels = c("Percentil 90%", "Percentil 95%", "Percentil 99%"),
          dep.var.labels = "Eficiência", 
          omit.stat = c("f", "ser"),
          notes = "Nota: Comparação do impacto de diferentes definições de extremos climáticos.",
          digits = 4)






# ==========================================================
# 6.5. CRIAÇÃO DAS DUMMIES DE EXTREMOS CLIMÁTICOS
# ==========================================================

# 1. Calcular os limites dos percentis de temperatura
p90 <- quantile(df_intradiario_scaled$umid, 0.90, na.rm = TRUE)
p95 <- quantile(df_intradiario_scaled$umid, 0.95, na.rm = TRUE)
p99 <- quantile(df_intradiario_scaled$umid, 0.99, na.rm = TRUE)

# 2. Criar as variáveis dummy no dataframe
df_intradiario_scaled <- df_intradiario_scaled %>%
  mutate(
    dummy_t90 = ifelse(umid >= p90, 1, 0),
    dummy_t95 = ifelse(umid >= p95, 1, 0),
    dummy_t99 = ifelse(umid >= p99, 1, 0)
  )

# 3. Definir a dummy "ativa" para a rodada principal (M1 a M7)
# Altere para dummy_t95 ou dummy_t99 conforme a análise que desejar focar
df_intradiario_scaled$dummy_extremo <- df_intradiario_scaled$dummy_t90


# ==========================================================
# 7. REGRESSÕES MULTIPLAS (VARIÁVEIS ORIGINAIS + EXTREMOS)
# ==========================================================

# 7.1. Estimação dos Modelos (Níveis Originais)
# A sintaxe 'dummy_extremo * (x + y)' garante que a dummy entre como regressor 
# individual e também interaja com todas as variáveis dentro dos parênteses.
# A defasagem lag(eficiencia) fica de fora da interação propositalmente.

m1_orig <- lm(eficiencia ~ lag(eficiencia) + dummy_extremo, data = df_intradiario_scaled)
m2_orig <- lm(eficiencia ~ lag(eficiencia) + dummy_extremo * (temp), data = df_intradiario_scaled)
m3_orig <- lm(eficiencia ~ lag(eficiencia) + dummy_extremo * (umid), data = df_intradiario_scaled)
m4_orig <- lm(eficiencia ~ lag(eficiencia) + dummy_extremo * (temp + umid), data = df_intradiario_scaled)
m5_orig <- lm(eficiencia ~ lag(eficiencia) + dummy_extremo * (temp + umid + interacao_temp_umid), data = df_intradiario_scaled)
m6_orig <- lm(eficiencia ~ lag(eficiencia) + dummy_extremo * (temp + umid + interacao_temp_umid + log(precip + 1)), data = df_intradiario_scaled)
m7_orig <- lm(eficiencia ~ lag(eficiencia) + dummy_extremo * (temp + umid + interacao_temp_umid + log(precip + 1) + vento), data = df_intradiario_scaled)

# ==========================================================
# 7.2 Testes de Diagnóstico dos Resíduos (Originais)
# COM TRATAMENTO DE ERRO (tryCatch) PARA MATRIZES SINGULARES
# ==========================================================
modelos_lista_orig <- list(m1_orig, m2_orig, m3_orig, m4_orig, m5_orig, m6_orig, m7_orig)

# Funções auxiliares seguras: se o teste quebrar, retorna NA ao invés de abortar o script
safe_bg_stat <- function(m) tryCatch(as.numeric(bgtest(m)$statistic), error = function(e) NA)
safe_bg_pval <- function(m) tryCatch(as.numeric(bgtest(m)$p.value), error = function(e) NA)

safe_bp_stat <- function(m) tryCatch(as.numeric(bptest(m)$statistic), error = function(e) NA)
safe_bp_pval <- function(m) tryCatch(as.numeric(bptest(m)$p.value), error = function(e) NA)

safe_arch_stat <- function(m) tryCatch(as.numeric(ArchTest(resid(m), lags = 1)$statistic), error = function(e) NA)
safe_arch_pval <- function(m) tryCatch(as.numeric(ArchTest(resid(m), lags = 1)$p.value), error = function(e) NA)

# Aplicação dos testes usando as funções seguras
bg_resultados_orig <- unname(sapply(modelos_lista_orig, safe_bg_stat))
bg_p_valores_orig  <- unname(sapply(modelos_lista_orig, safe_bg_pval))

bp_resultados_orig <- unname(sapply(modelos_lista_orig, safe_bp_stat))
bp_p_valores_orig  <- unname(sapply(modelos_lista_orig, safe_bp_pval))

arch_resultados_orig <- unname(sapply(modelos_lista_orig, safe_arch_stat))
arch_p_valores_orig  <- unname(sapply(modelos_lista_orig, safe_arch_pval))

# ==========================================================
# 7.3 Gerar Tabela Comparativa Original
# ==========================================================
linhas_diagnosticos_orig <- list(
  c("Breusch-Godfrey stat", ifelse(is.na(bg_resultados_orig), "NA", sprintf("%.3f", bg_resultados_orig))),
  c("BG p-value",           ifelse(is.na(bg_p_valores_orig), "NA", sprintf("%.3f", bg_p_valores_orig))),
  c("Breusch-Pagan stat",   ifelse(is.na(bp_resultados_orig), "NA", sprintf("%.3f", bp_resultados_orig))),
  c("BP p-value",           ifelse(is.na(bp_p_valores_orig), "NA", sprintf("%.3f", bp_p_valores_orig))),
  c("ARCH-LM stat",         ifelse(is.na(arch_resultados_orig), "NA", sprintf("%.3f", arch_resultados_orig))),
  c("ARCH p-value",         ifelse(is.na(arch_p_valores_orig), "NA", sprintf("%.3f", arch_p_valores_orig)))
)

# 7.3 Gerar Tabela Comparativa Original
linhas_diagnosticos_orig <- list(
  c("Breusch-Godfrey stat", sprintf("%.3f", bg_resultados_orig)),
  c("BG p-value",           sprintf("%.3f", bg_p_valores_orig)),
  c("Breusch-Pagan stat",   sprintf("%.3f", bp_resultados_orig)),
  c("BP p-value",           sprintf("%.3f", bp_p_valores_orig)),
  c("ARCH-LM stat",         sprintf("%.3f", arch_resultados_orig)),
  c("ARCH p-value",         sprintf("%.3f", arch_p_valores_orig))
)

nomes_modelos <- c("M1", "M2", "M3", "M4", "M5", "M6", "M7")

stargazer(m1_orig, m2_orig, m3_orig, m4_orig, m5_orig, m6_orig, m7_orig,
          type = "text",
          title = "Resultados das Regressões (Variáveis Originais + Interação Extremos)",
          column.labels = nomes_modelos,
          dep.var.labels = "Eficiência", 
          add.lines = linhas_diagnosticos_orig,
          omit.stat = c("f", "ser"),
          digits = 4)

# =========================================================
# 7.4 MODELOS ESTIMADOS COM ERROS-PADRÃO ROBUSTOS (HAC) - ORIGINAIS
# =========================================================

erros_hac_orig <- lapply(modelos_lista_orig, function(modelo) sqrt(diag(vcovHAC(modelo))))

stargazer(m1_orig, m2_orig, m3_orig, m4_orig, m5_orig, m6_orig, m7_orig,
          type = "text",
          se = erros_hac_orig,
          title = "Regressões Originais: Erros-Padrão Robustos (HAC / Newey-West)",
          column.labels = nomes_modelos,
          dep.var.labels = "Eficiência", 
          omit.stat = c("f", "ser"),
          notes = "Nota: Erros-padrão robustos (HAC) entre parênteses.",
          digits = 4)


# ==========================================================
# 8. REGRESSÕES MULTIPLAS (VARIÁVEIS PADRONIZADAS + EXTREMOS)
# ==========================================================

# 8.1. Estimação dos Modelos (Z-score)
m1 <- lm(eficiencia ~ lag(eficiencia) + dummy_extremo, data = df_intradiario_scaled)
m2 <- lm(eficiencia ~ lag(eficiencia) + dummy_extremo * (temp_z), data = df_intradiario_scaled)
m3 <- lm(eficiencia ~ lag(eficiencia) + dummy_extremo * (umid_z), data = df_intradiario_scaled)
m4 <- lm(eficiencia ~ lag(eficiencia) + dummy_extremo * (temp_z + umid_z) , data = df_intradiario_scaled)
m5 <- lm(eficiencia ~ lag(eficiencia) + dummy_extremo * (temp_z + umid_z  + interacao_temp_umid_z), data = df_intradiario_scaled)
m6 <- lm(eficiencia ~ lag(eficiencia) + dummy_extremo * (temp_z + umid_z  + interacao_temp_umid_z + log(precip + 1)), data = df_intradiario_scaled)
m7 <- lm(eficiencia ~ lag(eficiencia) + dummy_extremo * (temp_z + umid_z  + interacao_temp_umid_z + log(precip + 1) + vento_z), data = df_intradiario_scaled)

# ==========================================================
# 8.2 Testes de Diagnóstico dos Resíduos (Padronizados)
# COM TRATAMENTO DE ERRO (tryCatch) PARA MATRIZES SINGULARES
# ==========================================================
modelos_lista <- list(m1, m2, m3, m4, m5, m6, m7)

# Usando as funções auxiliares seguras (que você já rodou na Seção 7)
# Se o teste quebrar, retorna NA ao invés de abortar o script
bg_resultados <- unname(sapply(modelos_lista, safe_bg_stat))
bg_p_valores  <- unname(sapply(modelos_lista, safe_bg_pval))

bp_resultados <- unname(sapply(modelos_lista, safe_bp_stat))
bp_p_valores  <- unname(sapply(modelos_lista, safe_bp_pval))

arch_resultados <- unname(sapply(modelos_lista, safe_arch_stat))
arch_p_valores  <- unname(sapply(modelos_lista, safe_arch_pval))

# ==========================================================
# 8.3 Tabela Comparativa Stargazer (Padronizados)
# ==========================================================
linhas_diagnosticos <- list(
  c("Breusch-Godfrey stat", ifelse(is.na(bg_resultados), "NA", sprintf("%.3f", bg_resultados))),
  c("BG p-value",           ifelse(is.na(bg_p_valores), "NA", sprintf("%.3f", bg_p_valores))),
  c("Breusch-Pagan stat",   ifelse(is.na(bp_resultados), "NA", sprintf("%.3f", bp_resultados))),
  c("BP p-value",           ifelse(is.na(bp_p_valores), "NA", sprintf("%.3f", bp_p_valores))),
  c("ARCH-LM stat",         ifelse(is.na(arch_resultados), "NA", sprintf("%.3f", arch_resultados))),
  c("ARCH p-value",         ifelse(is.na(arch_p_valores), "NA", sprintf("%.3f", arch_p_valores)))
)

# Truque para driblar o bug do stargazer (conforme ajustado na Seção 10)
z1 <- m1; z2 <- m2; z3 <- m3; z4 <- m4; z5 <- m5; z6 <- m6; z7 <- m7
nomes_modelos <- c("M1", "M2", "M3", "M4", "M5", "M6", "M7")

stargazer(z1, z2, z3, z4, z5, z6, z7,
          type = "text",
          title = "Resultados das Regressões (Variáveis Padronizadas + Interação Extremos)",
          column.labels = nomes_modelos,
          dep.var.labels = "Eficiência", 
          add.lines = linhas_diagnosticos,
          omit.stat = c("f", "ser"),
          digits = 4)

# =========================================================
# 9. MODELOS ESTIMADOS COM ERROS-PADRÃO ROBUSTOS (HAC) - PADRONIZADOS
# =========================================================

erros_hac <- lapply(modelos_lista, function(modelo) sqrt(diag(vcovHAC(modelo))))

stargazer(m1, m2, m3, m4, m5, m6, m7,
          type = "text",
          se = erros_hac,
          title = "Regressões Padronizadas: Erros-Padrão Robustos (HAC / Newey-West)",
          column.labels = nomes_modelos,
          dep.var.labels = "Eficiência", 
          omit.stat = c("f", "ser"),
          notes = "Nota: Erros-padrão robustos (HAC) entre parênteses.",
          digits = 4)

# =========================================================
# 10. ANÁLISE DE SENSIBILIDADE: COMPARAÇÃO DOS PERCENTIS (90, 95 e 99)
# =========================================================
# Estimando o modelo mais completo (M7 Original) para os 3 cenários de cauda

m_full_90 <- lm(eficiencia ~ lag(eficiencia) + dummy_t90 * (temp + umid + interacao_temp_umid + log(precip + 1) + vento), data = df_intradiario_scaled)
m_full_95 <- lm(eficiencia ~ lag(eficiencia) + dummy_t95 * (temp + umid + interacao_temp_umid + log(precip + 1) + vento), data = df_intradiario_scaled)
m_full_99 <- lm(eficiencia ~ lag(eficiencia) + dummy_t99 * (temp + umid + interacao_temp_umid + log(precip + 1) + vento), data = df_intradiario_scaled)

# Cálculo HAC para os modelos de sensibilidade
erros_hac_sensibilidade <- lapply(list(m_full_90, m_full_95, m_full_99), function(modelo) sqrt(diag(vcovHAC(modelo))))

stargazer(m_full_90, m_full_95, m_full_99,
          type = "text",
          se = erros_hac_sensibilidade,
          title = "Sensibilidade aos Percentis de Temperatura (Modelo Completo com HAC)",
          column.labels = c("Percentil 90%", "Percentil 95%", "Percentil 99%"),
          dep.var.labels = "Eficiência", 
          omit.stat = c("f", "ser"),
          notes = "Nota: Comparação do impacto de diferentes definições de extremos climáticos.",
          digits = 4)