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
pacotes <- c("readxl", "dplyr", "lubridate", "ggplot2", "GGally", "tidyr", "stargazer", "FinTS","lmtest", "sandwich", "tseries", "writexl")

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
  filter(geracao_energia > 441) 

# Comando para salvar a base intermediária
write_xlsx(df_intradiario, "df_intermediario.xlsx")
cat("\nBase intermediária salva com sucesso como 'df_intermediario.xlsx'.\n")
