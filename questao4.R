arquivo <- "https://raw.githubusercontent.com/marisalinhares/homework_1_estatistica/refs/heads/main/HW1_bike_sharing.csv"
dados_completos <- read.csv(arquivo)
if (ncol(dados_completos) == 1) dados_completos <- read.csv2(arquivo)

data_group <- dados_completos[72:371, ]
data_group$total_user <- as.numeric(data_group$casual) + as.numeric(data_group$registered)
Q1 <- as.numeric(quantile(data_group$total_user, 0.25))
data_group$low_usage <- ifelse(data_group$total_user < Q1, 1, 0)

data_group$estacao <- factor(data_group$season, levels = c(1, 2, 3, 4), labels = c("Inverno", "Primavera", "Verao", "Outono"))
data_group$clima <- factor(data_group$weathersit, levels = c(1, 2, 3), labels = c("Ceu Limpo", "Nublado", "Chuva Fraca"))
data_group$temp_norm <- data_group$temp / 41

#    q4.1
data_group$dia_idx <- 1:300
#png("q4_1_serie_temporal.png", width = 800, height = 450)     ->download da imagem
plot(data_group$dia_idx, data_group$total_user, type = "l", main = "Série Temporal de Total de Usuários (300 dias)", xlab = "Dia (índice sequencial)", ylab = "Total de usuários por dia", col = "darkblue")
abline(h = Q1, lty = 2, col = "red")
#dev.off()     ->download da imagem
# picos e quedas
data_group[order(-data_group$total_user), c("dteday","total_user")][1:5,]
data_group[order(data_group$total_user), c("dteday","total_user")][1:5,]

#    q4.2a
#png("q4_2a_densidade_clima.png", width = 800, height = 450)     ->download da imagem
d_ceu <- density(data_group$total_user[data_group$clima == "Ceu Limpo"])
d_nub <- density(data_group$total_user[data_group$clima == "Nublado"])
d_chu <- density(data_group$total_user[data_group$clima == "Chuva Fraca"])
plot(d_ceu, col = "skyblue", lwd = 2, main = "Densidade de total_user por Condição Meteorológica", xlab = "Total de usuários por dia", ylim = c(0, max(d_ceu$y, d_nub$y, d_chu$y)))
lines(d_nub, col = "gray50", lwd = 2); lines(d_chu, col = "steelblue4", lwd = 2)
legend("topright", legend = c("Céu Limpo","Nublado","Chuva Fraca"), col = c("skyblue","gray50","steelblue4"), lwd = 2)
#dev.off()     ->download da imagem

anova_clima <- aov(total_user ~ clima, data = data_group)
summary(anova_clima)

#    q4.2b
#png("q4_2b_boxplot_tempbin.png", width = 800, height = 450)     ->download da imagem
data_group$temp_bin <- cut(data_group$temp_norm, breaks = quantile(data_group$temp_norm, probs = c(0,0.25,0.5,0.75,1)), labels = c("Frio (Q1)","Ameno (Q2)","Quente (Q3)","Muito Quente (Q4)"), include.lowest = TRUE)
boxplot(total_user ~ temp_bin, data = data_group, main = "Total de usuários por faixa de temperatura", xlab = "Faixa de temperatura normalizada", ylab = "Total de usuários por dia", col = c("lightblue","lightgreen","gold","salmon"))
cor(data_group$temp_norm, data_group$total_user, method = "spearman")
#dev.off()     ->download da imagem

#    q4.3
#png("q4_3_scatter_lowusage.png", width = 800, height = 450)     ->download da imagem
cores <- ifelse(data_group$low_usage == 1, "red", "darkorange")
plot(data_group$temp_norm, data_group$total_user, col = cores, pch = 16, main = "Temperatura vs Total de Usuários, por low_usage", xlab = "Temperatura Normalizada", ylab = "Total de usuários por dia")
legend("topleft", legend = c("low_usage = 0","low_usage = 1"), col = c("darkorange","red"), pch = 16)
cor(data_group$temp_norm[data_group$low_usage==0], data_group$total_user[data_group$low_usage==0])
cor(data_group$temp_norm[data_group$low_usage==1], data_group$total_user[data_group$low_usage==1])
#dev.off()     ->download da imagem