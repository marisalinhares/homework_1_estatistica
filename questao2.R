url_dados <- "https://raw.githubusercontent.com/marisalinhares/homework_1_estatistica/refs/heads/main/HW1_bike_sharing.csv"
dados_completos <- read.csv(url_dados)

M <- 588071
r <- 1 + (M %% 100)
cat("Valor de r:", r, "\n")                     # r = 72

data_group <- dados_completos[r:(r + 299), ]
dim(data_group)                                  # 300 linhas

primeira_linha <- data_group$dteday[1]
ultima_linha   <- data_group$dteday[300]
cat("Primeira linha:", primeira_linha, "\n")     # 2011-03-13
cat("Última linha:", ultima_linha, "\n")         # 2012-01-06

data_group$total_user <- data_group$casual + data_group$registered

data_group_10 <- data_group[1:10, ]
dim(data_group_10)

# 2.1 Classificação das variáveis, categorias e valores ausentes
str(data_group)
summary(data_group)

# Categorias das variáveis categóricas
table(data_group$season)        # 1, 2, 3, 4  
table(data_group$weathersit)    # 1, 2, 3     

# Período observado
range(as.Date(data_group$dteday))

# Valores ausentes por variável
colSums(is.na(data_group))

# Unidades / faixa de valores da temperatura 
summary(data_group$temp)

# 2.2 Cálculo das 10 primeiras observações
x10 <- data_group_10$total_user
n10 <- length(x10)
print(data.frame(instant = data_group_10$instant,
                 dteday = data_group_10$dteday,
                 casual = data_group_10$casual,
                 registered = data_group_10$registered,
                 total_user = x10))

# Média
soma10   <- sum(x10)
media10m <- soma10 / n10
cat("Soma =", soma10, "| Média manual =", media10m, "\n")

# Mediana
x10_ord   <- sort(x10)
mediana10m <- (x10_ord[n10 / 2] + x10_ord[n10 / 2 + 1]) / 2
cat("Ordenado:", x10_ord, "\nMediana manual =", mediana10m, "\n")

# Moda
calcular_moda <- function(x) {
  tab  <- table(x)
  fmax <- max(tab)
  if (fmax == 1) return(NA)                      # amodal: nenhum valor se repete
  as.numeric(names(tab)[tab == fmax])            # devolve TODAS as modas (empates)
}
cat("Moda (10 obs.):", calcular_moda(x10), "\n")

# Variância e desvio padrão 
desv10      <- x10 - media10m
soma_quad10 <- sum(desv10^2)
var10m      <- soma_quad10 / (n10 - 1)
dp10m       <- sqrt(var10m)
cat("Soma dos quadrados dos desvios =", soma_quad10,
    "\nVariância amostral manual =", var10m,
    "\nDesvio padrão amostral manual =", dp10m, "\n")

# Versão populacional (divisor n), só para comparar com outras convenções
var10_pop <- soma_quad10 / n10
cat("Variância populacional =", var10_pop, "| DP populacional =", sqrt(var10_pop), "\n")

# Quartis
quartil_manual <- function(x, p) {
  xs  <- sort(x); n <- length(xs)
  h   <- (n - 1) * p + 1
  inf <- floor(h)
  xs[inf] + (h - inf) * (xs[min(inf + 1, n)] - xs[inf])
}
Q1_10m <- quartil_manual(x10, 0.25)
Q2_10m <- quartil_manual(x10, 0.50)
Q3_10m <- quartil_manual(x10, 0.75)
IQR10m <- Q3_10m - Q1_10m
lim_inf10m <- Q1_10m - 1.5 * IQR10m
lim_sup10m <- Q3_10m + 1.5 * IQR10m
cat("Q1 =", Q1_10m, "| Q2 =", Q2_10m, "| Q3 =", Q3_10m, "| IQR =", IQR10m,
    "\nLimites:", lim_inf10m, "e", lim_sup10m,
    "\nOutliers:", x10[x10 < lim_inf10m | x10 > lim_sup10m], "\n")

metade_inf <- x10_ord[1:(n10 / 2)]
metade_sup <- x10_ord[(n10 / 2 + 1):n10]
cat("Mediana das metades: Q1 =", median(metade_inf), "| Q3 =", median(metade_sup), "\n")

cat("Dias com total_user < Q1 (10 obs.):", sum(x10 < Q1_10m), "de", n10, "\n")

# 2.3 VERIFICAÇÃO no R (funções nativas) e comparação

verificacao10 <- data.frame(
  medida = c("Média", "Mediana", "Variância (n-1)", "Desvio padrão (n-1)", "Q1", "Q2", "Q3", "IQR"),
  manual = c(media10m, mediana10m, var10m, dp10m, Q1_10m, Q2_10m, Q3_10m, IQR10m),
  R      = c(mean(x10), median(x10), var(x10), sd(x10),
             quantile(x10, 0.25), quantile(x10, 0.50), quantile(x10, 0.75), IQR(x10))
)
verificacao10$diferenca <- verificacao10$manual - verificacao10$R
print(verificacao10, row.names = FALSE)

# Efeito da convenção de cálculo dos quartis (tipos do quantile())
sapply(c(2, 5, 6, 7), function(tp) {
  q <- quantile(x10, c(0.25, 0.75), type = tp)
  c(Q1 = unname(q[1]), Q3 = unname(q[2]), IQR = unname(q[2] - q[1]))
}) -> comp_tipos
colnames(comp_tipos) <- paste0("tipo ", c(2, 5, 6, 7))
print(comp_tipos)

# Efeito do divisor da variância: R usa n-1; n daria valor diferente
cat("var() do R (n-1):", var(x10), "| variância com divisor n:", var10_pop, "\n")

# 2.4 Código aplicado às 300 observações 

resumo_descritivo <- function(x) {
  q <- quantile(x, c(0.25, 0.5, 0.75))                 
  iqr <- q[3] - q[1]
  c(n = length(x), media = mean(x), mediana = median(x),
    dp = sd(x), variancia = var(x), minimo = min(x), maximo = max(x),
    Q1 = unname(q[1]), Q2 = unname(q[2]), Q3 = unname(q[3]), IQR = unname(iqr),
    lim_inf = unname(q[1] - 1.5 * iqr), lim_sup = unname(q[3] + 1.5 * iqr))
}

cat("\n--- 10 primeiras observações ---\n")
print(round(resumo_descritivo(x10), 2))

x300 <- data_group$total_user
cat("\n--- 300 observações ---\n")
res300 <- resumo_descritivo(x300)
print(round(res300, 3))

# Medidas de tendência central e dispersão (300 obs.)
media300   <- mean(x300)
mediana300 <- median(x300)
modas300   <- calcular_moda(x300)
cat("Média:", media300, "| Mediana:", mediana300, "\n")
cat("Modas (", length(modas300), " valores empatados):", modas300, "\n")
cat("Coeficiente de variação:", round(100 * sd(x300) / media300, 1), "%\n")

# 2.5 Quartis, IQR e valores atípicos (300 obs.)

Q1 <- quantile(x300, 0.25); Q2 <- quantile(x300, 0.50); Q3 <- quantile(x300, 0.75)
IQR_total <- Q3 - Q1
lim_inf <- Q1 - 1.5 * IQR_total
lim_sup <- Q3 + 1.5 * IQR_total
cat("Q1 =", Q1, "| Q2 =", Q2, "| Q3 =", Q3, "| IQR =", IQR_total,
    "\nLimite inferior =", lim_inf, "| Limite superior =", lim_sup, "\n")

outliers <- subset(data_group, total_user < lim_inf | total_user > lim_sup,
                   select = c(instant, dteday, weathersit, total_user))
print(outliers)
cat("Número de outliers:", nrow(outliers), "\n")

# Sensibilidade à convenção de quartil (300 obs.)
sapply(c(2, 5, 6, 7), function(tp) quantile(x300, c(0.25, 0.75), type = tp))

# 2.6 Histograma e boxplot

png("Rplot01.png", width = 1200, height = 500, res = 120)
par(mfrow = c(1, 2))
hist(x300, breaks = seq(0, 7000, by = 1000), right = FALSE,
     col = "lightblue", border = "white",
     main = "Histograma de total_user", xlab = "Usuários por dia", ylab = "Frequência")
abline(v = c(media300, mediana300), col = c("red", "darkgreen"), lwd = 2, lty = c(2, 1))
legend("topleft", legend = c("Média", "Mediana"), col = c("red", "darkgreen"),
       lwd = 2, lty = c(2, 1), bty = "n", cex = 0.8)
boxplot(x300, horizontal = TRUE, col = "lightblue",
        main = "Boxplot de total_user", xlab = "Usuários por dia")
dev.off()

# Contagem por classe
table(cut(x300, breaks = seq(0, 7000, by = 1000), right = FALSE))

# Assimetria 
assim <- mean((x300 - media300)^3) / sd(x300)^3
cat("Coeficiente de assimetria (momento):", round(assim, 3), "\n")

# 2.7 Variável binária low_usage 

data_group$low_usage <- ifelse(data_group$total_user < Q1, 1, 0)
cat("Q1 =", Q1, "\n")
cat("Dias de baixa utilização:", sum(data_group$low_usage), "\n")
cat("Proporção:", mean(data_group$low_usage), "\n")
table(data_group$low_usage)
