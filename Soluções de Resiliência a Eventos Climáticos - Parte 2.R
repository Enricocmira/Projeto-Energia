############################################################
# Soluções de Resiliência a Eventos Climáticos             #
# Extremos para o Setor Elétrico                           #
# Nomes: Enrico C. Mira                                    #
#        Leandro S. Maciel                                 #
# Data: 18/05/2026                                         #
############################################################

# Instale o pacote zoo caso ainda não tenha:
install.packages(c("tidyverse", "lubridate", "writexl", "readxl", "zoo"))

library(tidyverse)
library(lubridate)
library(writexl)
library(readxl)
library(zoo) # Necessário para a interpolação (suavização) da maré

setwd("C:/Users/enric/Downloads")

# ==============================================================================
# 1. Dados de Captação e Nível do Rio
# ==============================================================================
df_captacao <- read_excel("Dados_captacao_agua.xlsx") %>%
  mutate(DataHora = as.POSIXct(Data)) %>%
  select(DataHora,
         Vazao_Captacao_m3_h = `Vazão de água da captação(m³/h)`,
         Nivel_Rio_m = `Nível do Rio (m)`)

# ==============================================================================
# 2. Dados de Temperatura e Umidade (Clima Usina)
# ==============================================================================
df_temp_umidade <- read_excel("dados_v2.xlsx", sheet = "dados usina (geracao e clima)") %>%
  mutate(DataHora = as.POSIXct(DateTime)) %>%
  select(DataHora,
         Temperatura_C = `Temperatura ambiente (°C)`,
         Umidade_Relativa_pct = `Umidade Relativa do ar (%)`)

# ==============================================================================
# 3. Dados de Clima Adicionais (Vento e Precipitação)
# ==============================================================================
meses_pt_abrev <- c("jan" = "01", "fev" = "02", "mar" = "03", "abr" = "04",
                    "mai" = "05", "jun" = "06", "jul" = "07", "ago" = "08",
                    "set" = "09", "out" = "10", "nov" = "11", "dez" = "12")

df_clima_add <- read_excel("dados_v2.xlsx", sheet = "dados clima adicionais") %>%
  mutate(
    Data_Limpa = str_to_lower(as.character(Data)),
    Data_Limpa = str_replace_all(Data_Limpa, meses_pt_abrev),
    DataHora = dmy_hm(Data_Limpa)
  ) %>%
  mutate(`Precipitação [mm]` = log(1 + coalesce(as.numeric(`Precipitação [mm]`), as.numeric(`...5`)))) %>%
  select(DataHora,
         Velocidade_Vento_m_s = `Velocidade do Vento [m/s]`,
         Precipitacao_mm = `Precipitação [mm]`)

# ==============================================================================
# 4. Dados de Maré (Suavização com Spline)
# ==============================================================================
meses_pt <- c("Janeiro"=1, "Fevereiro"=2, "Março"=3, "Abril"=4, "Maio"=5, "Junho"=6,
              "Julho"=7, "Agosto"=8, "Setembro"=9, "Outubro"=10, "Novembro"=11, "Dezembro"=12)

processar_mare <- function(arquivo, ano) {
  df <- read_excel(arquivo)
  
  # Renomeia a 4ª coluna para evitar erros de digitação/espaços no Excel original
  colnames(df)[4] <- "Altura_Original"
  
  df %>%
    mutate(
      Mes_Num = meses_pt[Mês],
      Hora_Limpa = str_extract(as.character(Hora), "[0-9]{2}:[0-9]{2}"),
      Hora_Num = as.numeric(str_sub(Hora_Limpa, 1, 2)),
      Min_Num = as.numeric(str_sub(Hora_Limpa, 4, 5)),
      DataHora_Exata = make_datetime(ano, Mes_Num, as.numeric(Dia), Hora_Num, Min_Num),
      DataHora = round_date(DataHora_Exata, "hour"),
      Altura_Original = as.numeric(Altura_Original)
    ) %>%
    group_by(DataHora) %>%
    summarise(Altura_Mare_m = mean(Altura_Original, na.rm = TRUE), .groups = "drop")
}

df_mare_2024 <- processar_mare("Soluções de Resiliência a Eventos Climáticos Extremos para o Setor Elétrico - Marés/dados_mare_2024.xlsx", 2024)
df_mare_2025 <- processar_mare("Soluções de Resiliência a Eventos Climáticos Extremos para o Setor Elétrico - Marés/dados_mare_2025.xlsx", 2025)
df_mare_bruta <- bind_rows(df_mare_2024, df_mare_2025) %>% arrange(DataHora)

# Criar uma grade contínua de hora em hora do primeiro ao último registro da maré
grade_horas <- tibble(
  DataHora = seq(min(df_mare_bruta$DataHora, na.rm = TRUE), 
                 max(df_mare_bruta$DataHora, na.rm = TRUE), 
                 by = "hour")
)

# Interpolar (suavizar) os dados de maré
df_mare_suavizada <- grade_horas %>%
  left_join(df_mare_bruta, by = "DataHora") %>%
  mutate(
    # na.spline preenche os NAs seguindo a curvatura dos dados conhecidos
    Altura_Mare_m = zoo::na.spline(Altura_Mare_m)
  )

# ==============================================================================
# 5. Juntar todos os dataframes
# ==============================================================================
df_final <- df_captacao %>%
  full_join(df_temp_umidade, by = "DataHora") %>%
  full_join(df_clima_add, by = "DataHora") %>%
  full_join(df_mare_suavizada, by = "DataHora") %>%
  arrange(DataHora) %>%
  filter(!is.na(Vazao_Captacao_m3_h) | !is.na(Temperatura_C) | 
           !is.na(Velocidade_Vento_m_s) | !is.na(Altura_Mare_m))

# ==============================================================================
# 6. Salvar e Exportar
# ==============================================================================
nome_arquivo_saida <- "Base_Consolidada.xlsx"

write_xlsx(df_final, nome_arquivo_saida)

message("Processo concluído! Os dados de maré foram suavizados (interpolados).")
message(paste("O arquivo", nome_arquivo_saida, "foi salvo na pasta: C:/Users/enric/Downloads"))