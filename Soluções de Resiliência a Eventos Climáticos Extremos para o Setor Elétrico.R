############################################################
# Soluções de Resiliência a Eventos Climáticos             #
# Extremos para o Setor Elétrico                           #
# Nomes: Enrico C. Mira                                    #
#        Leandro S. Maciel                                 #
# Data: 23/03/2026                                         #
############################################################

# Limpeza de ambiente 
# rm(list=ls()); graphics.off()

# ==========================================================
# 1. PACOTES E AMBIENTE
# ==========================================================
# Todos os pacotes necessários centralizados aqui
pacotes <- c("readxl", "dplyr", "lubridate", "ggplot2", "GGally", "tidyr", "stargazer", "FinTS","lmtest", "sandwich")

# Instala (se necessário) e carrega todos os pacotes
for (p in pacotes) {
  if (!require(p, character.only = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}

# ==========================================================
# 2. IMPORTAÇÃO DOS DADOS
# ==========================================================
arquivo <- "C:/Users/enric/Downloads/Dados (1).xlsx"

df_usina      <- read_excel(arquivo, sheet = "dados usina (geracao e clima)")
df_clima_adic <- read_excel(arquivo, sheet = "dados clima adicionais")
df_gas        <- read_excel(arquivo, sheet = "dados do gas natural consumido")

# ==========================================================
# 3. TRATAMENTO INDIVIDUAL DAS BASES
# ==========================================================

# 3.1. Usina: Variáveis Climáticas
df_usina_clima <- df_usina %>%
  select(DateTime, `Temperatura ambiente (°C)`, `Umidade Relativa do ar (%)`) %>%
  mutate(
    DateTime                     = as.POSIXct(DateTime, format="%m/%d/%y %H:%M:%S"),
    `Temperatura ambiente (°C)`  = as.numeric(`Temperatura ambiente (°C)`),
    `Umidade Relativa do ar (%)` = as.numeric(`Umidade Relativa do ar (%)`)
  )

# 3.2. Usina: Agregação Horária da Geração
df_usina_geracao <- df_usina %>%
  mutate(
    Hora_Inicio   = substr(Patamar, 1, 5),
    DateTime_Temp = dmy_hm(paste(Data, Hora_Inicio)),
    DateTime      = floor_date(DateTime_Temp, unit = "hour")
  ) %>%
  group_by(DateTime) %>%
  summarise(
    `Geração Programada` = sum(`Geração Programada`, na.rm = TRUE),
    `Geração Despachada` = sum(`Geração Despachada`, na.rm = TRUE),
    .groups = "drop" 
  )

# 3.3. Clima Adicional
df_clima_adic <- df_clima_adic %>%
  mutate(
    DateTime            = dmy_hm(Data),
    `Precipitação [mm]` = coalesce(as.numeric(`Precipitação [mm]`), as.numeric(`...5`))
  ) %>%
  select(DateTime, `Velocidade do Vento [m/s]`, `Direção do Vento [°]`, `Precipitação [mm]`)

# 3.4. Gás Natural (Interpolação Diária para Horária)
df_gas <- df_gas %>%
  uncount(24) %>%
  group_by(Data) %>%
  mutate(
    DateTime          = Data + hours(row_number() - 1),
    `Volume (m³)`     = `Volume (m³)` / 24,
    `Massa (kg)`      = `Massa (kg)` / 24,
    `Energia (MMBTU)` = `Energia (MMBTU)` / 24
  ) %>%
  ungroup() %>%
  select(DateTime, `Volume (m³)`, `Massa (kg)`, `Energia (MMBTU)`, 
         `PCS (BTU/m³)`, `PCS (kcal/m³)`, `PCS (kJ/m³)`)

# ==========================================================
# 4. JUNÇÃO E ENGENHARIA DE VARIÁVEIS (DF FINAL)
# ==========================================================

df_regressao <- df_usina_clima %>%
  inner_join(df_usina_geracao, by = "DateTime") %>%
  inner_join(df_clima_adic, by = "DateTime") %>%
  inner_join(df_gas, by = "DateTime") %>%
  filter(`Geração Despachada` > 0, `Energia (MMBTU)` > 0) %>%
  mutate(
    heat_rate           = (`Energia (MMBTU)` * 1.05506 / `Geração Despachada`) * 1000,
    eficiencia          = 1 / heat_rate,
    interacao_temp_umid = `Temperatura ambiente (°C)` * `Umidade Relativa do ar (%)`
  ) %>%
  select(
    DateTime,
    eficiencia, 
    temp         = `Temperatura ambiente (°C)`, 
    umid         = `Umidade Relativa do ar (%)`, 
    vento        = `Velocidade do Vento [m/s]`, 
    precip       = `Precipitação [mm]`,
    interacao_temp_umid,
    qualidadegas = `PCS (BTU/m³)`
  ) %>%
  # Define prep como log(precip + 1) já na base horária (opcional, útil para gráficos horários)
  mutate(prep = log(precip + 1))

# ==========================================================
# 5. AGREGAÇÃO PARA FREQUÊNCIA DIÁRIA
# ==========================================================

df_diario <- df_regressao %>%
  mutate(Data = as.Date(DateTime)) %>%
  group_by(Data) %>%
  summarise(
    eficiencia          = mean(eficiencia, na.rm = TRUE),
    temp                = mean(temp, na.rm = TRUE),
    umid                = mean(umid, na.rm = TRUE),
    vento               = mean(vento, na.rm = TRUE),
    precip              = sum(precip, na.rm = TRUE), # Soma a precipitação bruta primeiro
    interacao_temp_umid = mean(interacao_temp_umid, na.rm = TRUE),
    qualidadegas        = mean(qualidadegas, na.rm = TRUE),
    .groups             = "drop"
  ) %>%
  # Define prep como log(precip + 1) na base diária após a soma
  mutate(prep = log(precip + 1))

# ==========================================================
# 6. ESTATÍSTICAS DESCRITIVAS DOS DADOS DIÁRIOS
# ==========================================================

# Preparar os dados para o stargazer (removendo a Data e a precip bruta para focar no prep)
df_descritiva <- df_diario %>%
  select(-Data, -precip) %>% 
  as.data.frame()

# Gerar a tabela diretamente no Console
stargazer(df_descritiva, 
          type = "text", 
          summary = TRUE, 
          title = "Estatísticas Descritivas - Base Diária Agregada",
          digits = 4, 
          covariate.labels = c("Eficiência", "Temperatura (°C)", "Umidade (%)", 
                               "Vento (m/s)", "Interação (Temp x Umid)", 
                               "Qualidade do Gás (PCS)", "Precipitação (log(mm+1))"))

# ==========================================================
# 7. VISUALIZAÇÃO DAS SÉRIES TEMPORAIS (HORÁRIAS)
# ==========================================================

# 1. Preparar os dados para um gráfico multi-painel (Long Format)
# Usando 'prep' ao invés de 'precip'
df_long_viz <- df_regressao %>%
  pivot_longer(
    cols = c(eficiencia, temp, umid, vento, prep, qualidadegas),
    names_to = "Variavel",
    values_to = "Valor"
  ) %>%
  mutate(
    Variavel_Rótulo = factor(Variavel, 
                             levels = c("eficiencia", "temp", "umid", "vento", "prep", "qualidadegas"),
                             labels = c("Eficiência (Geração/MMBTU)", 
                                        "Temperatura ambiente (°C)", 
                                        "Umidade Relativa (%)", 
                                        "Vento [m/s]", 
                                        "Precipitação [log(mm+1)]",
                                        "Qualidade Gás (PCS BTU/m³)"))
  )

# 2. Gerar o Gráfico de Séries Temporais Empilhadas
ggplot(df_long_viz, aes(x = DateTime, y = Valor)) +
  geom_line(color = "darkblue", size = 0.5) +
  facet_wrap(~ Variavel_Rótulo, scales = "free_y", ncol = 1) +
  scale_x_datetime(date_breaks = "6 months", date_labels = "%b/%Y") +
  theme_minimal(base_size = 14) +
  theme(
    strip.text.y = element_text(angle = 0, face = "bold"),
    axis.title.y = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold"),
    strip.background = element_rect(fill = "gray95", color = "white")
  ) +
  labs(
    title = "Séries Temporais das Variáveis Operacionais e Climáticas",
    subtitle = "Dados horários limpos (df_regressao, N = 11.741)",
    x = "Tempo"
  )


# =========================================================
# 4. TESTES DE ESTACIONARIEDADE (ADF e PP) - HORÁRIO E DIÁRIO
# =========================================================

if (!require("tseries")) install.packages("tseries")
library(tseries)

# 1. Função generalizada para testar todas as variáveis de um dataframe
gerar_tabela_estacionariedade <- function(df, nome_base) {
  
  # Definindo as variáveis que serão testadas e seus nomes de exibição
  variaveis <- c("eficiencia", "temp", "umid", "vento", "precip", "interacao_temp_umid", "qualidadegas")
  nomes_ex <- c("Eficiência (Heat Rate)", "Temperatura", "Umidade", "Velocidade do Vento", 
                "Precipitação", "Interação (Temp x Umid)", "Qualidade do Gás")
  
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
tabela_estacionariedade_horaria <- gerar_tabela_estacionariedade(df_regressao, "HORÁRIA (df_regressao)")

# 3. Gerar e exibir a tabela para os dados DIÁRIOS AGREGADOS
tabela_estacionariedade_diaria <- gerar_tabela_estacionariedade(df_diario, "DIÁRIA (df_diario)")


# =========================================================
# 5. MODELOS E TESTES
# =========================================================

# 5.0 Criar base, padronizar, e criar a variável defasada (Y_t-1)
df_diario_scaled <- df_diario %>%
  arrange(Data) %>%
  mutate(
    eficiencia          = eficiencia,
    temp                = as.numeric(scale(temp)),
    umid                = as.numeric(scale(umid)),
    vento               = as.numeric(scale(vento)),
    interacao_temp_umid = as.numeric(scale(interacao_temp_umid)),
    qualidadegas        = as.numeric(scale(qualidadegas))
  ) %>%
  mutate(
    eficiencia_lag1 = lag(eficiencia, n = 1)
  ) %>%
  na.omit()

# 5.1 Definição dos 11 Modelos LM Dinâmicos
# Grupo 1: Sem a Interação
m1 <- lm(eficiencia ~ eficiencia_lag1 + temp, data = df_diario_scaled)
m2 <- lm(eficiencia ~ eficiencia_lag1 + temp + umid, data = df_diario_scaled)
m3 <- lm(eficiencia ~ eficiencia_lag1 + temp + umid + vento, data = df_diario_scaled)
m4 <- lm(eficiencia ~ eficiencia_lag1 + temp + umid + log(precip + 1), data = df_diario_scaled)
m5 <- lm(eficiencia ~ eficiencia_lag1 + temp + umid + log(precip + 1) + vento, data = df_diario_scaled)

# Grupo 2: Com a Interação (Termos Base + Interação)
m6 <- lm(eficiencia ~ eficiencia_lag1 + temp + umid + interacao_temp_umid, data = df_diario_scaled)
m7 <- lm(eficiencia ~ eficiencia_lag1 + temp + umid + interacao_temp_umid + log(precip + 1), data = df_diario_scaled)
m8 <- lm(eficiencia ~ eficiencia_lag1 + temp + umid + interacao_temp_umid + vento, data = df_diario_scaled)
m9 <- lm(eficiencia ~ eficiencia_lag1 + temp + umid + interacao_temp_umid + log(precip + 1) + vento, data = df_diario_scaled)

# Grupo 3: Modelos Completos (Com Qualidade do Gás)
m10 <- lm(eficiencia ~ eficiencia_lag1 + temp + umid + log(precip + 1) + vento + qualidadegas, data = df_diario_scaled)
m11 <- lm(eficiencia ~ eficiencia_lag1 + temp + umid + interacao_temp_umid + log(precip + 1) + vento + qualidadegas, data = df_diario_scaled)

# 5.2 Testes de Diagnóstico dos Resíduos
modelos_lista <- list(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11)

# A. Teste de Autocorrelação de Breusch-Godfrey
bg_resultados <- sapply(modelos_lista, function(m) bgtest(m)$statistic)
bg_p_valores  <- sapply(modelos_lista, function(m) bgtest(m)$p.value)

# B. Teste de Heterocedasticidade de Breusch-Pagan
bp_resultados <- sapply(modelos_lista, function(m) bptest(m)$statistic)
bp_p_valores  <- sapply(modelos_lista, function(m) bptest(m)$p.value)

# C. Teste ARCH-LM
arch_resultados <- sapply(modelos_lista, function(m) ArchTest(resid(m), lags = 1)$statistic)
arch_p_valores  <- sapply(modelos_lista, function(m) ArchTest(resid(m), lags = 1)$p.value)

# 5.3 Gerar Tabela Comparativa (Console e Arquivo)
linhas_diagnosticos <- list(
  c("Breusch-Godfrey stat", round(bg_resultados, 3)),
  c("BG p-value",           round(bg_p_valores, 3)),
  c("Breusch-Pagan stat",   round(bp_resultados, 3)),
  c("BP p-value",           round(bp_p_valores, 3)),
  c("ARCH-LM stat",         round(arch_resultados, 3)),
  c("ARCH p-value",         round(arch_p_valores, 3))
)

# Rótulos das colunas
nomes_modelos <- c("M1", "M2", "M3", "M4", "M5", "M6", "M7", "M8", "M9", "M10", "M11")

stargazer(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11,
          type = "text",
          title = "Resultados das Regressões",
          column.labels = nomes_modelos,
          dep.var.labels = "Log(Eficiência)",
          add.lines = linhas_diagnosticos,
          omit.stat = c("f", "ser"),
          digits = 4) # Reduzido para 4 casas para caber melhor na tela

# =========================================================
# 6. MODELOS ESTIMADOS COM ERROS-PADRÃO ROBUSTOS (HAC)
# =========================================================

# 2. Criar a lista com TODOS os 11 modelos já estimados
modelos_lista <- list(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11)

# 3. Calcular os Erros-Padrão Robustos (HAC / Newey-West) para cada modelo
# A função vcovHAC cria a nova matriz de covariância.
# Extraímos a diagonal e tiramos a raiz quadrada para obter os novos erros-padrão.
erros_hac <- lapply(modelos_lista, function(modelo) sqrt(diag(vcovHAC(modelo))))

# 4. Gerar Tabela Comparativa Corrigida no Console
nomes_modelos <- c("M1", "M2", "M3", "M4", "M5", "M6", "M7", "M8", "M9", "M10", "M11")

stargazer(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, # Agora chamando todos os 11
          type = "text",
          se = erros_hac, # <-- Injeta os erros-padrão robustos aqui
          title = "Resultados das Regressões Dinâmicas: Erros-Padrão Robustos (HAC)",
          column.labels = nomes_modelos,
          dep.var.labels = "Eficiência (Heat Rate)", # Corrigido para escala em nível
          covariate.labels = c("Eficiência (t-1)", "Temperatura", "Umidade", "Vento", 
                               "Log(Precip + 1)", "Interação (Temp x Umid)", "Qualidade do Gás"),
          omit.stat = c("f", "ser"),
          notes = "Nota: Erros-padrão robustos (HAC / Newey-West) entre parênteses.",
          digits = 4) # Usando 4 casas para a tabela caber inteira no console
