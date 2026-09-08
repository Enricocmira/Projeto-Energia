############################################################
# Soluções de Resiliência a Eventos Climáticos             #
# Extremos para o Setor Elétrico                           #
# Nomes: Enrico C. Mira                                    #
#        Leandro S. Maciel                                 #
# Data: 26/05/2026                                         #
############################################################

# Instale os pacotes caso ainda não os possua:
# install.packages(c("tidyverse", "readxl", "quantreg", "lmtest", "knitr", "patchwork"))

library(tidyverse)
library(readxl)
library(quantreg)  # Pacote oficial para regressão quantílica
library(lmtest)    # Para o teste de Heterocedasticidade (Breusch-Pagan)
library(knitr)     # Para formatação da tabela final
library(patchwork) # NOVO: Para criar sub-gráficos independentes com eixos próprios

# 1. Importação dos Dados
setwd("C:/Users/enric/Downloads")
df_importado <- read_excel("Base_Consolidada.xlsx", 
                           col_types = c("date", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric"))

# ==============================================================================
# 2. PREPARAÇÃO DOS DADOS
# ==============================================================================
df_modelo <- df_importado %>%
  # Limpeza inicial (garantindo que o R leia como numérico com ponto)
  mutate(across(-DataHora, ~ suppressWarnings(as.numeric(gsub(",", ".", .))))) %>%
  drop_na() %>%
  mutate(Umidade_Decimal = Umidade_Relativa_pct / 100) %>%
  dplyr::select(DataHora, Nivel_Rio_m, Temperatura_C, Umidade_Decimal, Precipitacao_mm, Altura_Mare_m)

n_observacoes <- nrow(df_modelo)

# ==============================================================================
# 2.1 ESTATÍSTICAS DA VARIÁVEL DEPENDENTE (Valores dos Decis)
# ==============================================================================
decis_rio <- quantile(df_modelo$Nivel_Rio_m, probs = seq(0.1, 0.9, by = 0.1))

cat("\n====================================================================\n")
cat(" VALORES DOS DECIS DA VARIÁVEL DEPENDENTE (Nível do Rio em metros) \n")
cat("====================================================================\n")
print(decis_rio)
cat("====================================================================\n\n")

eq_quantilica <- Nivel_Rio_m ~ Temperatura_C + Umidade_Decimal + Precipitacao_mm + Altura_Mare_m

# ==============================================================================
# 3. TESTES DE DIAGNÓSTICO (Autocorrelação e Heterocedasticidade)
# ==============================================================================
modelo_ols <- lm(eq_quantilica, data = df_modelo)
teste_bp <- bptest(modelo_ols)

modelo_mediana <- rq(eq_quantilica, tau = 0.50, data = df_modelo)
teste_lb <- Box.test(resid(modelo_mediana), lag = 24, type = "Ljung-Box")

cat("\n====================================================================\n")
cat(" TESTES DE DIAGNÓSTICO ESTRUTURAL \n")
cat("====================================================================\n")
cat("1. Teste de Breusch-Pagan (Heterocedasticidade):\n")
cat("   P-valor:", teste_bp$p.value, "\n")
cat("   Conclusão:", ifelse(teste_bp$p.value < 0.05, "Presença de Heterocedasticidade confirmada.", "Homocedasticidade mantida."), "\n\n")

cat("2. Teste de Ljung-Box (Autocorrelação Serial - 24 horas):\n")
cat("   P-valor:", teste_lb$p.value, "\n")
cat("   Conclusão:", ifelse(teste_lb$p.value < 0.05, "Presença de Autocorrelação confirmada.", "Ausência de Autocorrelação."), "\n")
cat("====================================================================\n\n")

# ==============================================================================
# 4. REGRESSÃO QUANTÍLICA E INFERÊNCIA ROBUSTA
# ==============================================================================
quantis_interesse <- c(0.01,0.025,0.05, 0.075)#seq(0, 1, 0.1); quantis_interesse[-c(1, length(quantis_interesse))]

modelo_rq <- rq(eq_quantilica, tau = quantis_interesse, data = df_modelo)

cat("Calculando erros-padrão via XY-Pair Bootstrap (Robustos). Aguarde...\n")
resumo_robusto <- summary(modelo_rq, se = "boot", bsmethod = "xy", R = 500)

# ==============================================================================
# 5. VISUALIZAÇÃO GRÁFICA DA DINÂMICA DOS COEFICIENTES
# ==============================================================================
# Passo 1: Extrair coeficientes e intervalos de confiança (95%)
df_grafico <- lapply(1:length(quantis_interesse), function(i) {
  res <- resumo_robusto[[i]]$coefficients
  data.frame(
    Tau = quantis_interesse[i],
    Variavel = rownames(res),
    Coef = res[, "Value"],
    Limite_Inf = res[, "Value"] - 1.96 * res[, "Std. Error"],
    Limite_Sup = res[, "Value"] + 1.96 * res[, "Std. Error"]
  )
}) %>% bind_rows()

# Passo 2: Extrair modelo OLS
res_ols <- summary(modelo_ols)$coefficients
df_ols <- data.frame(
  Variavel = rownames(res_ols),
  OLS_Coef = res_ols[, "Estimate"],
  OLS_Inf = res_ols[, "Estimate"] - 1.96 * res_ols[, "Std. Error"],
  OLS_Sup = res_ols[, "Estimate"] + 1.96 * res_ols[, "Std. Error"]
)

# Passo 3: Juntar e renomear variáveis
df_plot_final <- df_grafico %>%
  left_join(df_ols, by = "Variavel") %>%
  mutate(Variavel = recode(Variavel,
                           "(Intercept)" = "Intercepto",
                           "Temperatura_C" = "Efeito da Temperatura",
                           "Umidade_Decimal" = "Efeito da Umidade (Dec)",
                           "Precipitacao_mm" = "Efeito da Precipitação",
                           "Altura_Mare_m" = "Efeito da Maré")) %>%
  mutate(Variavel = factor(Variavel, levels = c("Intercepto", "Efeito da Temperatura", 
                                                "Efeito da Umidade (Dec)", "Efeito da Precipitação", 
                                                "Efeito da Maré")))

# Passo 4: Criar os gráficos INDIVIDUAIS para garantir rótulos X e Y em todos eles
variaveis_niveis <- levels(df_plot_final$Variavel)

# Função para forçar a vírgula na hora de desenhar os eixos
formata_virgula <- function(x) format(x, decimal.mark = ",", scientific = FALSE, trim = TRUE)

lista_graficos <- lapply(variaveis_niveis, function(var_nome) {
  df_sub <- df_plot_final %>% filter(Variavel == var_nome)
  
  ggplot(df_sub, aes(x = Tau)) +
    geom_hline(yintercept = 0, color = "black", size = 0.4) +
    geom_ribbon(aes(ymin = Limite_Inf, ymax = Limite_Sup), fill = "grey75", alpha = 0.8) +
    geom_line(aes(y = OLS_Coef), color = "red", linetype = "solid", size = 0.8) +
    geom_line(aes(y = OLS_Inf), color = "red", linetype = "dashed", size = 0.8) +
    geom_line(aes(y = OLS_Sup), color = "red", linetype = "dashed", size = 0.8) +
    geom_line(aes(y = Coef), color = "black", size = 0.8) +
    geom_point(aes(y = Coef), color = "black", size = 1.5, shape = 16) +
    
    # Eixos formatados com vírgula de forma nativa e segura
    scale_y_continuous(labels = formata_virgula) +
    scale_x_continuous(labels = formata_virgula) +
    
    theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, size = 0.6),
      plot.title = element_text(color = "black", face = "bold", size = 11, hjust = 0.5),
      axis.title = element_text(face = "bold", size = 10)
    ) +
    # Adiciona o título e os nomes dos eixos X e Y EM CADA sub-gráfico
    labs(title = var_nome, x = "Quantis (\u03C4)", y = "Efeito Marginal")
})

# Passo 5: Costurar todos os gráficos numa única imagem usando patchwork
grafico_quantilico <- wrap_plots(lista_graficos, ncol = 2)

# Exibir o Gráfico
print(grafico_quantilico)

# ==============================================================================
# 6. TABELA CONSOLIDADA DE RESULTADOS (Padrão Acadêmico)
# ==============================================================================
lista_resultados <- lapply(1:length(quantis_interesse), function(i) {
  res <- resumo_robusto[[i]]$coefficients
  
  # Extrai os valores numéricos, garante 4 casas decimais e troca o ponto pela vírgula com format()
  valores_formatados <- format(round(res[, "Value"], 4), nsmall = 4, decimal.mark = ",")
  erros_formatados   <- format(round(res[, "Std. Error"], 4), nsmall = 4, decimal.mark = ",")
  
  df_coef <- data.frame(
    Nome_Var = rownames(res),
    Tipo = "Coef",
    Quantil = paste0("Q_", quantis_interesse[i]),
    Valor = paste0(valores_formatados,
                   ifelse(res[, "Pr(>|t|)"] < 0.01, "***",
                          ifelse(res[, "Pr(>|t|)"] < 0.05, "**",
                                 ifelse(res[, "Pr(>|t|)"] < 0.10, "*", "")))),
    Ordem = 1:nrow(res) * 2 - 1 
  )
  
  df_se <- data.frame(
    Nome_Var = rownames(res),
    Tipo = "SE",
    Quantil = paste0("Q_", quantis_interesse[i]),
    Valor = paste0("(", erros_formatados, ")"),
    Ordem = 1:nrow(res) * 2 
  )
  
  rbind(df_coef, df_se)
})

tabela_larga <- do.call(rbind, lista_resultados) %>%
  arrange(Ordem) %>%
  pivot_wider(names_from = Quantil, values_from = Valor) %>%
  mutate(Variavel = recode(Nome_Var,
                           "(Intercept)" = "Intercepto",
                           "Temperatura_C" = "Temperatura (°C)",
                           "Umidade_Decimal" = "Umidade (Decimal)",
                           "Precipitacao_mm" = "Precipitação (mm)",
                           "Altura_Mare_m" = "Maré (m)")) %>%
  mutate(Variavel = ifelse(Tipo == "SE", "", Variavel)) %>%
  dplyr::select(Variavel, starts_with("Q_"))

linha_obs <- data.frame(Variavel = "Observações")
for(q in paste0("Q_", quantis_interesse)) {
  linha_obs[[q]] <- as.character(n_observacoes)
}

tabela_final <- bind_rows(tabela_larga, linha_obs)

cat("\n==================================================================================================================\n")
cat(" TABELA CONSOLIDADA DA REGRESSÃO QUANTÍLICA (Erro-Padrão entre parênteses)\n")
cat("==================================================================================================================\n")
print(kable(tabela_final, align = "c", caption = "Nota: Erros-padrão robustos em parênteses. Significância: *** p<0.01, ** p<0.05, * p<0.10"))