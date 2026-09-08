############################################################
# Soluções de Resiliência a Eventos Climáticos             #
# Extremos para o Setor Elétrico                           #
# Nomes: Enrico C. Mira                                    #
#        Leandro S. Maciel                                 #
# Data: 26/05/2026                                         #
############################################################

# Instale os pacotes necessários caso ainda não os possua:
# install.packages(c("tidyverse", "readxl", "cluster", "factoextra", "patchwork","clusterSim", "knitr"))

library(clusterSim) # Para calcular o Davies-Bouldin
library(knitr)      # Para gerar a tabela de características formatada
library(tidyverse)
library(readxl)
library(cluster)    # Algoritmos de clusterização
library(factoextra) # Visualização elegante de clusters e PCA
library(patchwork)  # Para juntar gráficos

# 1. Importar os dados consolidados
setwd("C:/Users/enric/Downloads")
df_importado <- read_excel("Base_Consolidada.xlsx", 
                           col_types = c("date", "numeric", "numeric", 
                                         "numeric", "numeric", "numeric", 
                                         "numeric", "numeric"))

# ==============================================================================
# 2. PREPARAÇÃO DOS DADOS PARA CLUSTERIZAÇÃO (Versão Ultra-Blindada)
# ==============================================================================
# 1. Limpa dados e silencia o aviso de coerção (textos como "No Data" viram NA silenciosamente)
df_limpo <- df_importado %>%
  mutate(across(-DataHora, ~ suppressWarnings(as.numeric(gsub(",", ".", .))))) %>%
  dplyr::select(-Vazao_Captacao_m3_h, -Velocidade_Vento_m_s) %>%
  drop_na()

# TRAVA DE SEGURANÇA: Verifica se o drop_na() não apagou a base inteira
if(nrow(df_limpo) == 0) {
  stop("ERRO: Após remover os NAs, o dataframe ficou com 0 linhas. Verifique se alguma variável está com falha geral de leitura.")
}

# 2. Seleciona apenas as numéricas
df_modelo <- df_limpo %>% dplyr::select(-DataHora)

# 3. REMOVE COLUNAS PROBLEMÁTICAS (Impede o erro da variância)
# Condição segura: É numérica? Tem mais de 1 valor válido? A variância é maior que zero?
df_modelo <- df_modelo %>% 
  dplyr::select(where(~ is.numeric(.) && sum(!is.na(.)) > 1 && !is.na(var(., na.rm = TRUE)) && var(., na.rm = TRUE) > 0))

# 4. Padroniza os dados (Z-score)
df_padronizado <- scale(df_modelo)

# 5. Garante a remoção absoluta de qualquer linha problemática residual
df_padronizado <- na.omit(df_padronizado)

# ==============================================================================
# 3. DETERMINAÇÃO DO NÚMERO IDEAL DE CLUSTERS (K)
# ==============================================================================

# Certifique-se de que o tema clássico foi declarado antes neste script:
tema_classico_cluster <- theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.6),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

# ==============================================================================
# 3. DETERMINAÇÃO DO NÚMERO IDEAL DE CLUSTERS (K)
# ==============================================================================

# 3.1 Método do Cotovelo (WSS)
grafico_cotovelo <- fviz_nbclust(df_padronizado, kmeans, method = "wss", 
                                 linecolor = "black") + # Remove a cor azul padrão
  labs(title = "Método do Cotovelo",
       subtitle = "Definição do número ideal de regimes",
       x = "Número de Clusters (k)", y = "Soma dos Quadrados Intra-Cluster") +
  tema_classico_cluster +
  # Grade horizontal pontilhada discreta para facilitar a leitura da redução da variância
  theme(panel.grid.major.y = element_line(color = "gray90", linetype = "dashed"))

# 3.2 Método da Silhueta (Média de Silhouette Score por K)
grafico_silhueta_k <- fviz_nbclust(df_padronizado, kmeans, method = "silhouette", 
                                   linecolor = "black") + 
  labs(title = "Método da Silhueta",
       subtitle = "Maior valor indica o número ótimo de clusters",
       x = "Número de Clusters (k)", y = "Largura Média da Silhueta") +
  tema_classico_cluster +
  theme(panel.grid.major.y = element_line(color = "gray90", linetype = "dashed"))

# O fviz_nbclust (no método silhouette) injeta uma linha tracejada indicadora do k ótimo 
# na cor "steelblue". Forçamos essa camada geom_vline a ficar preta:
if(length(grafico_silhueta_k$layers) > 1) {
  grafico_silhueta_k$layers[[2]]$aes_params$colour <- "black"
}

# Exibir os dois métodos juntos para comparação (usando patchwork para colocá-los lado a lado)
print(grafico_cotovelo)
print(grafico_silhueta_k)

# ==============================================================================
# 4. APLICAÇÃO DO ALGORITMO K-MEANS E ÍNDICE DAVIES-BOULDIN
# ==============================================================================
set.seed(123) # Fixa a semente para reprodutibilidade
k_otimo <- 4 
modelo_kmeans <- kmeans(df_padronizado, centers = k_otimo, nstart = 25)

# Adicionamos a classificação do cluster de volta ao dataframe original limpo
df_resultado <- df_limpo %>%
  mutate(Cluster = as.factor(modelo_kmeans$cluster))

# Calcula o Índice de Davies-Bouldin
# (Quanto menor o valor, melhor a qualidade do agrupamento)
db_index <- index.DB(df_padronizado, modelo_kmeans$cluster, d = dist(df_padronizado))

cat("\n====================================================================\n")
cat(" MÉTRICA DE QUALIDADE DO MODELO \n")
cat("====================================================================\n")
cat("Índice de Davies-Bouldin (k =", k_otimo, "):", round(db_index$DB, 4), "\n")
cat("* Nota: Valores menores indicam clusters mais compactos e bem separados.\n")

# ==============================================================================
# 5. ANÁLISE DESCRITIVA DOS CLUSTERS (TABELA DE CARACTERÍSTICAS)
# ==============================================================================
# Calcula a média de cada variável dentro de cada cluster
perfil_clusters <- df_resultado %>%
  group_by(Cluster) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>%
  arrange(Cluster)

cat("\n====================================================================\n")
cat(" CARACTERÍSTICAS DOS CLUSTERS (Média das Variáveis por Regime) \n")
cat("====================================================================\n")

# Usa a função kable para imprimir uma tabela limpa e formatada no console
print(kable(perfil_clusters, 
            digits = 2, # Arredonda para 2 casas decimais para facilitar a leitura
            caption = "Perfil Médio de Cada Cluster",
            align = "c")) 

# ==============================================================================
# 6. VISUALIZAÇÃO DOS CLUSTERS E QUALIDADE (SILHUETA) COM ESTÉTICA CLÁSSICA
# ==============================================================================

# Criação de um tema customizado que imita a estética clássica solicitada
tema_classico_cluster <- theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(color = "black", face = "bold", size = 11),
    panel.border = element_rect(color = "black", fill = NA, size = 0.6),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

# Gráfico 1: Projeção PCA (Biplot)
grafico_pca <- fviz_cluster(modelo_kmeans, data = df_padronizado,
                            geom = "point",
                            ellipse.type = "convex", 
                            # Usa o ggtheme base limpo e aplica nossa paleta jco
                            ggtheme = tema_classico_cluster,
                            main = "Dispersão dos Clusters (Componentes Principais)",
                            palette = "jco") 

# Gráfico 2: Boxplots comparativos (Sem outliers, cores claras e títulos em cada painel)
df_long_cluster <- df_resultado %>%
  dplyr::select(Cluster, Nivel_Rio_m, Altura_Mare_m, Precipitacao_mm, Temperatura_C, Umidade_Relativa_pct) %>% 
  pivot_longer(cols = -Cluster, names_to = "Variavel", values_to = "Valor") %>%
  # Traduz os nomes das variáveis para que funcionem como Títulos em cada sub-gráfico
  mutate(Variavel = recode(Variavel,
                           "Nivel_Rio_m" = "Nível do Rio (m)",
                           "Altura_Mare_m" = "Altura da Maré (m)",
                           "Precipitacao_mm" = "Precipitação (mm)",
                           "Temperatura_C" = "Temperatura (°C)",
                           "Umidade_Relativa_pct" = "Umidade Relativa (%)")) %>%
  # Define a ordem de exibição dos gráficos (opcional, mas deixa organizado)
  mutate(Variavel = factor(Variavel, levels = c("Nível do Rio (m)", "Altura da Maré (m)", 
                                                "Precipitação (mm)", "Temperatura (°C)", 
                                                "Umidade Relativa (%)")))

grafico_boxplots <- ggplot(df_long_cluster, aes(x = Cluster, y = Valor, fill = Cluster)) +
  # outlier.shape = NA remove a plotagem dos pontos extremos
  geom_boxplot(alpha = 0.9, outlier.shape = NA, color = "black", size = 0.5) +
  
  # facet_wrap cria um gráfico para cada variável usando o nome formatado acima como título
  facet_wrap(~ Variavel, scales = "free_y", ncol = 2) +
  
  # Aplica o tema clássico limpo
  tema_classico_cluster +
  
  # Utiliza uma paleta de cores claras (tons pastéis)
  scale_fill_brewer(palette = "Pastel1") + 
  
  # Título geral e nomes dos eixos (o Y fica vazio pois a unidade já está no título do painel)
  labs(title = "Distribuição das Variáveis Chave por Cluster",
       x = "Regime (Cluster)", y = "") +
  
  theme(legend.position = "none")

print(grafico_boxplots)

# Gráfico 3: Análise de Silhueta do Modelo Final (k = 4)
# O cálculo da matriz de distância
distancias <- dist(df_padronizado)
sil <- silhouette(modelo_kmeans$cluster, distancias)

grafico_silhueta_modelo <- fviz_silhouette(sil, 
                                           palette = "jco", 
                                           ggtheme = tema_classico_cluster) +
  labs(title = paste("Qualidade do Agrupamento (Silhouette Score) - K =", k_otimo),
       subtitle = "Valores próximos de 1 indicam boa coesão. Valores negativos indicam classificação incorreta.") +
  # Força a linha de média (que vem tracejada em vermelho por padrão) a ficar preta para o estilo clássico
  theme(plot.subtitle = element_text(hjust = 0.5))

# Como o fviz_silhouette cria uma camada estática vermelha para a média, forçamos ela a ser preta
grafico_silhueta_modelo$layers[[2]]$aes_params$colour <- "black"

# Exibe os gráficos
print(grafico_pca)
print(grafico_boxplots)
print(grafico_silhueta_modelo)