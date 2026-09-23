# Configuracao inicial e amostragem herdadas da Questao 1
url_dados <- "https://raw.githubusercontent.com/marisalinhares/homework_1_estatistica/refs/heads/main/HW1_bike_sharing.csv"
dados_completos <- read.csv(url_dados)
M <- 588071
r <- 1 + (M %% 100)
data_group <- dados_completos[r:(r+299), ]

# Questao 2.1: Caracterizacao e classificacao
data_group$total_user <- data_group$casual + data_group$registered

cat("Categorias de season:", sort(unique(data_group$season)), "\n")
cat("Categorias de weathersit:", sort(unique(data_group$weathersit)), "\n")
print(colSums(is.na(data_group)))

# Questao 2.2: Medidas de tendencia central
calcular_moda <- function(vetor) {
  valores_unicos <- unique(vetor)
  valores_unicos[which.max(tabulate(match(vetor, valores_unicos)))]
}

variaveis_relevantes <- c("temp", "casual", "registered", "total_user")

medidas_centrais <- data.frame(
  Variavel = variaveis_relevantes,
  Media = sapply(data_group[variaveis_relevantes], mean),
  Mediana = sapply(data_group[variaveis_relevantes], median),
  Moda = sapply(data_group[variaveis_relevantes], calcular_moda)
)

print(medidas_centrais)

# Questao 2.3: Quartis e Valores Atipicos (Outliers)
# Escolha da variavel: total_user
Q1 <- quantile(data_group$total_user, 0.25)
Q2 <- quantile(data_group$total_user, 0.50)
Q3 <- quantile(data_group$total_user, 0.75)
IQR <- Q3 - Q1

limite_inferior <- Q1 - 1.5 * IQR
limite_superior <- Q3 + 1.5 * IQR

cat("\n--- Medidas de Posicao e Dispersao ---\n")
cat("Q1:", Q1, "\nQ2 (Mediana):", Q2, "\nQ3:", Q3, "\nIQR:", IQR, "\n")
cat("Limite Inferior:", limite_inferior, "\nLimite Superior:", limite_superior, "\n")

# Identificacao de possiveis outliers
outliers <- subset(data_group, total_user < limite_inferior | total_user > limite_superior)
cat("\nNumero de outliers encontrados:", nrow(outliers), "\n")

if(nrow(outliers) > 0) {
  cat("Datas e valores dos outliers:\n")
  print(outliers[, c("dteday", "total_user")])
}

# Questao 2.4: Visualizacao Univariada (Histograma e Boxplot)
# Configuracao para exibir os dois graficos lado a lado
par(mfrow = c(1, 2))

# Construcao do Histograma
hist(data_group$total_user,
     main = "Distribuicao de Usuarios Totais",
     xlab = "Total de Usuarios Diarios",
     ylab = "Frequencia (Dias)",
     col = "lightblue",
     border = "black")

# Construcao do Boxplot
boxplot(data_group$total_user,
        main = "Boxplot de Usuarios Totais",
        ylab = "Total de Usuarios Diarios",
        col = "lightgreen")

# Restaurando a configuracao original da area de plotagem
par(mfrow = c(1, 1))

# Questao 2.5: Criacao da variavel binaria low_usage
# (O valor de Q1 ja foi calculado na etapa 2.3)

# Criacao da variavel utilizando a condicao pedida na Equacao 1
data_group$low_usage <- ifelse(data_group$total_user < Q1, 1, 0)

# Calculo do numero de dias e da proporcao
num_dias_low <- sum(data_group$low_usage == 1)
proporcao_low <- num_dias_low / nrow(data_group)

# Exibicao dos resultados
cat("\n--- Variavel low_usage ---\n")
cat("Valor de corte (Q1):", Q1, "\n")
cat("Numero de dias de baixa utilizacao (low_usage = 1):", num_dias_low, "\n")
cat("Proporcao de dias de baixa utilizacao:", round(proporcao_low * 100, 2), "%\n")