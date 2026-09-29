data_group$dia_idx <- 1:300

plot(data_group$dia_idx, data_group$total_user, type = "l",
     main = "Série Temporal de Total de Usuários (300 dias)",
     xlab = "Dia (índice sequencial)", ylab = "Total de usuários por dia",
     col = "darkblue")
abline(h = Q1, lty = 2, col = "red")

# picos e quedas
data_group[order(-data_group$total_user), c("dteday","total_user")][1:5,]
data_group[order(data_group$total_user), c("dteday","total_user")][1:5,]