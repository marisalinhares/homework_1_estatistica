#importacao de dados
arquivo <- "https://raw.githubusercontent.com/marisalinhares/homework_1_estatistica/refs/heads/main/HW1_bike_sharing.csv"
dados_completos <- read.csv(arquivo)
if (ncol(dados_completos) == 1) dados_completos <- read.csv2(arquivo)

data_group <- dados_completos[72:371, ]
data_group$total_user <- as.numeric(data_group$casual) + as.numeric(data_group$registered)
Q1 <- as.numeric(quantile(data_group$total_user, 0.25))
data_group$low_usage <- ifelse(data_group$total_user < Q1, 1, 0)

#conferindo apenas
print(nrow(data_group))
print(Q1)
print(sum(data_group$low_usage))

#q3.1
data_group$estacao <- factor(data_group$season, levels = c(1, 2, 3, 4),labels = c("Inverno", "Primavera", "Verao", "Outono"))
data_group_10 <- data_group[1:10, ]

#10 primeir
print(aggregate(total_user ~ estacao, data = data_group_10, FUN = mean))
print(aggregate(total_user ~ estacao, data = data_group_10, FUN = median))
print(aggregate(total_user ~ estacao, data = data_group_10, FUN = sd))
print(aggregate(low_usage ~ estacao, data = data_group_10, FUN = mean))

#300 ob
print(aggregate(total_user ~ estacao, data = data_group, FUN = mean))
print(aggregate(total_user ~ estacao, data = data_group, FUN = median))
print(aggregate(total_user ~ estacao, data = data_group, FUN = sd))
print(aggregate(low_usage ~ estacao, data = data_group, FUN = mean))
print(table(data_group$estacao))

#boxplot
boxplot(total_user ~ estacao, data = data_group,
        main = "Total de usuários por estação do ano",
        xlab = "Estaçãoo", ylab = "Total de usuários por dia",
        col = c("lightblue", "lightgreen", "khaki", "salmon"))
abline(h = Q1, lty = 2, col = "red")

# Questão 3.2

# Transforma a variavel numerica 'weathersit' num fator com rotulos compreensiveis
data_group$clima <- factor(data_group$weathersit, 
            levels = c(1, 2, 3), 
            labels = c("Ceu Limpo", "Nublado", "Chuva Fraca"))

# Calcula Média de total_user por clima
print("Média de usuários por clima:")
aggregate(total_user ~ clima, data = data_group, FUN = mean)

# Calcula Desvio Padrão de total_user por clima
print("Desvio padrão de usuários por clima:")
aggregate(total_user ~ clima, data = data_group, FUN = sd)

# Determina a proporção de dias 'low_usage' por clima
print("Proporção de dias de baixa utilização por clima:")
aggregate(low_usage ~ clima, data = data_group, FUN = mean)

# Gráfico para comparar as condições
boxplot(total_user ~ clima, data = data_group,
        main = "Total de usuários por Condição Meteorológica",
        xlab = "Condição do Tempo", 
        ylab = "Total de usuários por dia",
        col = c("skyblue", "lightgray", "steelblue"))
abline(h = Q1, lty = 2, col = "red")
