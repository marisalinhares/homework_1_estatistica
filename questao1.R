# dataset direto do GitHub
url_dados <- "https://raw.githubusercontent.com/marisalinhares/homework_1_estatistica/refs/heads/main/HW1_bike_sharing.csv"
dados_completos <- read.csv(url_dados)

M <- 588071
r <- 1 + (M%%100)
cat("Valor de r:", r, "\n")

data_group <- dados_completos[r:(r+299),]
dim(data_goup)

primeira_linha <- data_group$dteday[1]
ultima_linha <- data_group$dteday[300]

cat("Primeira linha:", primeira_linha,"\n")
cat("Última linha:", ultima_linha,"\n")

data_group_10 <- data_group[1:10,]
dim(data_group_10)
