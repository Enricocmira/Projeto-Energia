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
pacotes <- c("readxl", "dplyr", "lubridate", "ggplot2", "GGally", "tidyr", "stargazer", "FinTS","lmtest", "sandwich")

for (p in pacotes) {
  if (!require(p, character.only = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}

# ==========================================================
# 2. IMPORTAÇÃO DOS DADOS
# ==========================================================
#setwd("~/Desktop/Produto #4 Acende/Analises R")
#arquivo <- "~/Desktop/Produto #4 Acende/Analises R/dados.xlsx"
arquivo <- "C:/Users/enric/Downloads/Dados (1).xlsx"

df_usina      <- read_excel(arquivo, sheet = "dados usina (geracao e clima)")
df_clima      <- read_excel(arquivo, sheet = "dados clima adicionais")
df_gas        <- read_excel(arquivo, sheet = "dados do gas natural consumido")

# ==========================================================
# 3. TRATAMENTO PARA BASE INTRADIÁRIA (HORÁRIA)
# ==========================================================

# 3.1. Usina: Variáveis Climáticas (Horárias)
df_usina_clima <- df_usina %>%
  select(DateTime, `Temperatura ambiente (°C)`, `Umidade Relativa do ar (%)`) %>%
  mutate(
    DateTime = as.POSIXct(DateTime, format="%m/%d/%y %H:%M:%S"),
    temp     = as.numeric(`Temperatura ambiente (°C)`),
    umid     = as.numeric(`Umidade Relativa do ar (%)`)
  ) %>%
  select(DateTime, temp, umid)

# 3.2. Usina: Agregação Horária da Geração
df_usina_geracao <- df_usina %>%
  mutate(
    Hora_Inicio   = substr(Patamar, 1, 5),
    DateTime_Temp = dmy_hm(paste(Data, Hora_Inicio)),
    DateTime      = floor_date(DateTime_Temp, unit = "hour")
  ) %>%
  group_by(DateTime) %>%
  summarise(
    geracao_programada = sum(`Geração Programada`, na.rm = TRUE),
    geracao_despachada = sum(`Geração Despachada`, na.rm = TRUE),
    .groups = "drop" 
  )%>%
  select(DateTime,geracao_programada,geracao_despachada)

# 3.3. Clima Adicional (Horário)
df_clima_horario <- df_clima %>%
  mutate(
    DateTime   = dmy_hm(Data),
    precip_num = coalesce(as.numeric(`Precipitação [mm]`), as.numeric(`...5`)),
    vento_num  = as.numeric(`Velocidade do Vento [m/s]`)
  ) %>%
  select(DateTime, vento = vento_num, precip = precip_num)

# 3.4. Gás Natural (Interpolação Diária para Horária)
df_gas_horario <- df_gas %>%
  uncount(24) %>%
  group_by(Data) %>%
  mutate(
    DateTime      = as.POSIXct(Data) + hours(row_number() - 1),
    energia_mmbtu = `Energia (MMBTU)` / 24,
    qualidadegas  = `PCS (BTU/m³)`
  ) %>%
  ungroup() %>%
  select(DateTime, energia_mmbtu, qualidadegas)

# ==========================================================
# 4. JUNÇÃO E ENGENHARIA (DF INTRADIÁRIO)
# ==========================================================

df_intradiario <- df_usina_geracao %>%
  full_join(df_usina_clima , by = "DateTime") %>%
  full_join(df_gas_horario, by = "DateTime") %>%
  full_join(df_clima_horario, by = "DateTime") %>%
  filter(geracao_despachada > 0) %>%
  filter(energia_mmbtu > 0) %>% # Coluna onde eliminamos dias sem produção
  mutate(
    # Eficiência horária (fração) e interações para análise de alta frequência
    eficiencia_horaria  = (geracao_despachada * 3.6) / (energia_mmbtu * 1.05506),
    interacao_temp_umid = temp * umid
  )

# ==========================================================
# 5. AGREGAÇÃO PARA FREQUÊNCIA DIÁRIA
# ==========================================================

df_diario <- df_intradiario %>%
    mutate(Data = as.Date(DateTime)) %>%
    group_by(Data) %>%
    summarise(
      # Soma total de saídas e entradas para o cálculo exato da eficiência diária
      geracao_total_dia = sum(geracao_despachada, na.rm = TRUE),
      energia_total_dia = sum(energia_mmbtu, na.rm = TRUE),
      despachada   = mean(geracao_despachada, na.rm = TRUE),
      programada   = mean(geracao_programada, na.rm = TRUE), 
      temp         = mean(temp, na.rm = TRUE),
      umid         = mean(umid, na.rm = TRUE),
      vento        = mean(vento, na.rm = TRUE),
      qualidadegas = mean(qualidadegas, na.rm = TRUE),
      precip       = sum(precip, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # Cálculos finais derivados na base diária
    mutate(
      eficiencia          = (geracao_total_dia * 3.6) / (energia_total_dia * 1.05506),
      prep                = log(precip + 1),
      interacao_temp_umid = temp * umid 
    ) %>%
    # Limpeza das colunas auxiliares de soma, mantendo apenas as de interesse
    select(
      Data, 
      eficiencia, 
      temp, 
      umid, 
      vento, 
      precip, 
      interacao_temp_umid, 
      qualidadegas, 
      prep,
      despachada,
      programada
    )


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
df_long_viz <- df_diario %>%
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
ggplot(df_long_viz, aes(x = Data, y = Valor)) +
  geom_line(color = "darkblue", size = 0.5) +
  facet_wrap(~ Variavel_Rótulo, scales = "free_y", ncol = 1) +
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

# ==========================================================
# 9. VISUALIZAÇÕES
# ==========================================================

# 1. Preparar os dados (formato longo)
df_plot <- df_diario %>%
  # Seleciona apenas a data e as variáveis de interesse
  select(Data, despachada, programada) %>% 
  pivot_longer(
    cols = c(despachada, programada),
    names_to = "Tipo",
    values_to = "Energia"
  )

# 2. Criar o gráfico com ggplot2
grafico_energia <- ggplot(df_plot, aes(x = Data, y = Energia, color = Tipo)) +
  geom_line() + # Linha um pouco mais grossa (equivale ao lwd = 1.5)
  
  # Aplica as cores sóbrias que você escolheu
  scale_color_manual(
    values = c("despachada" = "blue", "programada" = "#e74c3c"),
    labels = c("Despachada", "Programada"),
    name = NULL # Remove o título da legenda para despoluir
  ) +
  
  # Textos e Rótulos
  labs(
    title = "Energia Produzida (MWh)",
    x = NULL, # Remove o rótulo do eixo X (já é intuitivo que são datas)
    y = "Energia (MWh)"
  ) +
  
  # Estética Minimalista
  theme_minimal() +
  theme(
    legend.position = "top",                  # Legenda no topo
    legend.justification = "left",            # Alinha a legenda à esquerda
    plot.title = element_text(face = "bold"), # Título em negrito
    panel.grid.minor = element_blank(),       # Remove linhas de grade menores
    axis.text = element_text(color = "gray30")# Suaviza a cor do texto dos eixos
  )

# Imprime o gráfico na tela
print(grafico_energia)

# =========================================================
# 4. TESTES DE ESTACIONARIEDADE (ADF e PP) - HORÁRIO E DIÁRIO
# =========================================================

if (!require("tseries")) install.packages("tseries")
library(tseries)
library(dplyr) # Garantir que o dplyr está carregado para o pipeline

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
tabela_estacionariedade_horaria <- df_intradiario %>%
  rename(eficiencia = eficiencia_horaria) %>% # CORREÇÃO AQUI: Renomeia para dar "match" com a função
  na.omit() %>%
  select(where(is.numeric)) %>%
  gerar_tabela_estacionariedade("HORÁRIA")

# 3. Gerar e exibir a tabela para os dados DIÁRIOS AGREGADOS
tabela_estacionariedade_diaria <- gerar_tabela_estacionariedade(df_diario, "DIÁRIA")

# ==========================================================
# 5. MODELOS E TESTES
# ==========================================================

# 5.1. Engenharia de Variáveis e Padronização (Z-score)
# Padroniza as variáveis explicativas e cria a interação correta 
# para mitigar a multicolinearidade estrutural.
df_diario_scaled <- df_diario %>%
  arrange(Data) %>%
  mutate(
    eficiencia = eficiencia,
    
    # Padronização (média 0, desvio padrão 1)
    temp_z         = as.numeric(scale(temp)),
    umid_z         = as.numeric(scale(umid)),
    vento_z        = as.numeric(scale(vento)),
    qualidadegas_z = as.numeric(scale(qualidadegas))
  ) %>%
  mutate(
    # Interação calculada a partir das variáveis já padronizadas
    interacao_temp_umid_z = temp_z * umid_z
    
    # Obs: Variável defasada (Y_t-1) comentada caso seja necessário modelo dinâmico futuro
    # eficiencia_lag1 = lag(eficiencia, n = 1)
  ) %>%
  na.omit()

# ==========================================================
# 6. ANÁLISE DE AUTOCORRELAÇÃO DA VARIÁVEL DEPENDENTE
# ==========================================================

# Inspeção visual das funções de autocorrelação (ACF) e autocorrelação parcial (PACF).
# Como não há autocorrelação significativa, a especificação de um modelo estático é adequada.
acf(df_diario_scaled$eficiencia, main = "ACF da Eficiência")
pacf(df_diario_scaled$eficiencia, main = "PACF da Eficiência")


# ==========================================================
# 7. REGRESSÕES MULTIPLAS (VARIÁVEIS ORIGINAIS)
# ==========================================================

# 7.1. Estimação dos Modelos (Níveis Originais)
# Grupo 1: Modelos Aditivos (Sem a Interação)
m1_orig <- lm(eficiencia ~ temp, data = df_diario_scaled)
m2_orig <- lm(eficiencia ~ temp + umid, data = df_diario_scaled)
m3_orig <- lm(eficiencia ~ temp + umid + vento, data = df_diario_scaled)
m4_orig <- lm(eficiencia ~ temp + umid + log(precip + 1), data = df_diario_scaled)
m5_orig <- lm(eficiencia ~ temp + umid + log(precip + 1) + vento, data = df_diario_scaled)

# Grupo 2: Modelos Multiplicativos (Termos Base + Interação)
m6_orig <- lm(eficiencia ~ temp + umid + interacao_temp_umid, data = df_diario_scaled)
m7_orig <- lm(eficiencia ~ temp + umid + interacao_temp_umid + log(precip + 1), data = df_diario_scaled)
m8_orig <- lm(eficiencia ~ temp + umid + interacao_temp_umid + vento, data = df_diario_scaled)
m9_orig <- lm(eficiencia ~ temp + umid + interacao_temp_umid + log(precip + 1) + vento, data = df_diario_scaled)

# Grupo 3: Modelos Completos (Incluindo Qualidade do Gás)
m10_orig <- lm(eficiencia ~ temp + umid + log(precip + 1) + vento + qualidadegas, data = df_diario_scaled)
m11_orig <- lm(eficiencia ~ temp + umid + interacao_temp_umid + log(precip + 1) + vento + qualidadegas, data = df_diario_scaled)

# 7.2 Testes de Diagnóstico dos Resíduos (Originais) - BLINDADOS
modelos_lista_orig <- list(m1_orig, m2_orig, m3_orig, m4_orig, m5_orig, m6_orig, m7_orig, m8_orig, m9_orig, m10_orig, m11_orig)

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
x8  <- m8_orig
x9  <- m9_orig
x10 <- m10_orig
x11 <- m11_orig

nomes_modelos <- c("M1", "M2", "M3", "M4", "M5", "M6", "M7", "M8", "M9", "M10", "M11")

# Agora a chamada tem menos de 60 caracteres e o R não vai quebrar a string!
stargazer(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11,
          type = "text",
          title = "Resultados das Regressões (Variáveis Originais)",
          column.labels = nomes_modelos,
          dep.var.labels = "Eficiência",
          add.lines = linhas_diagnosticos_orig,
          omit.stat = c("f", "ser"),
          digits = 4)

# ==========================================================
# 8. REGRESSÕES MULTIPLAS (VARIÁVEIS PADRONIZADAS)
# ==========================================================

# 8.1. Estimação dos Modelos (Z-score)
# Grupo 1: Sem a Interação
m1 <- lm(eficiencia ~ temp_z, data = df_diario_scaled)
m2 <- lm(eficiencia ~ temp_z + umid_z, data = df_diario_scaled)
m3 <- lm(eficiencia ~ temp_z + umid_z + vento_z, data = df_diario_scaled)
m4 <- lm(eficiencia ~ temp_z + umid_z + log(precip + 1), data = df_diario_scaled)
m5 <- lm(eficiencia ~ temp_z + umid_z + log(precip + 1) + vento_z, data = df_diario_scaled)

# Grupo 2: Com a Interação (Termos Base + Interação)
m6 <- lm(eficiencia ~ temp_z + umid_z + interacao_temp_umid_z, data = df_diario_scaled)
m7 <- lm(eficiencia ~ temp_z + umid_z + interacao_temp_umid_z + log(precip + 1), data = df_diario_scaled)
m8 <- lm(eficiencia ~ temp_z + umid_z + interacao_temp_umid_z + vento_z, data = df_diario_scaled)
m9 <- lm(eficiencia ~ temp_z + umid_z + interacao_temp_umid_z + log(precip + 1) + vento_z, data = df_diario_scaled)

# Grupo 3: Modelos Completos (Com Qualidade do Gás)
m10 <- lm(eficiencia ~ temp_z + umid_z + log(precip + 1) + vento_z + qualidadegas_z, data = df_diario_scaled)
m11 <- lm(eficiencia ~ temp_z + umid_z + interacao_temp_umid_z + log(precip + 1) + vento_z + qualidadegas_z, data = df_diario_scaled)

# 8.2. Testes de Diagnóstico dos Resíduos (Padronizados)
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

# 8.3. Tabela Comparativa Stargazer (Padronizados)
linhas_diagnosticos <- list(
  c("Breusch-Godfrey stat", round(bg_resultados, 3)),
  c("BG p-value",           round(bg_p_valores, 3)),
  c("Breusch-Pagan stat",   round(bp_resultados, 3)),
  c("BP p-value",           round(bp_p_valores, 3)),
  c("ARCH-LM stat",         round(arch_resultados, 3)),
  c("ARCH p-value",         round(arch_p_valores, 3))
)

stargazer(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11,
          type = "text",
          title = "Resultados das Regressões (Variáveis Padronizadas)",
          column.labels = nomes_modelos,
          dep.var.labels = "Eficiência",
          add.lines = linhas_diagnosticos,
          omit.stat = c("f", "ser"),
          digits = 4)