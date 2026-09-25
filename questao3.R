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

# Questão 3.1

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
> data_group$clima <- factor(data_group$weathersit, 
+                           levels = c(1, 2, 3), 
+                           labels = c("Ceu Limpo", "Nublado", "Chuva Fraca"))
> 
> # Seleciona as 10 primeiras observacoes
> data_group_10 <- data_group[1:10, ]
> 
> # 10 primeiras observacoes
> 
> # Calcula Média de total_user por clima (10 primeiras)
> print("Média de usuários por clima (10 primeiras observações):")
[1] "Média de usuários por clima (10 primeiras observações):"
> aggregate(total_user ~ clima, data = data_group_10, FUN = mean)
      clima total_user
1 Ceu Limpo   2676.714
2   Nublado   2108.333
> 
> # Calcula Desvio Padrão de total_user por clima (10 primeiras)
> print("Desvio padrão de usuários por clima (10 primeiras observações):")
[1] "Desvio padrão de usuários por clima (10 primeiras observações):"
> aggregate(total_user ~ clima, data = data_group_10, FUN = sd)
      clima total_user
1 Ceu Limpo   412.8772
2   Nublado    73.2143
> 
> # Determina a proporção de dias 'low_usage' por clima (10 primeiras)
> print("Proporção de dias de baixa utilização por clima (10 primeiras observações):")
[1] "Proporção de dias de baixa utilização por clima (10 primeiras observações):"
> aggregate(low_usage ~ clima, data = data_group_10, FUN = mean)
      clima low_usage
1 Ceu Limpo 0.8571429
2   Nublado 1.0000000
> 
> # Frequência de dias por clima (10 primeiras)
> print("Frequência de dias na amostra (10 primeiras observações):")
[1] "Frequência de dias na amostra (10 primeiras observações):"
> table(data_group_10$clima)

  Ceu Limpo     Nublado Chuva Fraca 
          7           3           0 
> 
> # 300 observacoes 
> 
> # Calcula Média de total_user por clima
> print("Média de usuários por clima:")
[1] "Média de usuários por clima:"
> aggregate(total_user ~ clima, data = data_group, FUN = mean)
        clima total_user
1   Ceu Limpo   4131.836
2     Nublado   3541.776
3 Chuva Fraca   1844.846
> 
> # Calcula Desvio Padrão de total_user por clima
> print("Desvio padrão de usuários por clima:")
[1] "Desvio padrão de usuários por clima:"
> aggregate(total_user ~ clima, data = data_group, FUN = sd)
        clima total_user
1   Ceu Limpo   988.8999
2     Nublado  1034.0396
3 Chuva Fraca   753.2358
> 
> # Determina a proporção de dias 'low_usage' por clima
> print("Proporção de dias de baixa utilização por clima:")
[1] "Proporção de dias de baixa utilização por clima:"
> aggregate(low_usage ~ clima, data = data_group, FUN = mean)
        clima low_usage
1   Ceu Limpo 0.1587302
2     Nublado 0.3265306
3 Chuva Fraca 1.0000000
> 
> # Frequência de dias por clima
> print("Frequência de dias na amostra:")
[1] "Frequência de dias na amostra:"
> table(data_group$clima)

  Ceu Limpo     Nublado Chuva Fraca 
        189          98          13 
> 
> # Gráfico para comparar as condições
> boxplot(total_user ~ clima, data = data_group,
+        main = "Total de usuários por Condição Meteorológica",
+        xlab = "Condição do Tempo", 
+        ylab = "Total de usuários por dia",
+        col = c("skyblue", "lightgray", "steelblue"))
> abline(h = Q1, lty = 2, col = "red")

# Questão 3.3

# Seleciona as 10 primeiras observacoes
data_group_10 <- data_group[1:10, ]

# Calcula o coeficiente de correlação (Pearson) - 10 primeiras observações
print("Coeficiente de correlação entre Temperatura e Total de Usuários (10 primeiras observações):")
correlacao_10 <- cor(data_group_10$temp, data_group_10$total_user)
print(correlacao_10)

# Calcula o coeficiente de correlação (Pearson) - 300 observações
print("Coeficiente de correlação entre Temperatura e Total de Usuários:")
correlacao <- cor(data_group$temp, data_group$total_user)
print(correlacao)

# Constrói o gráfico de dispersão (Scatter plot)
plot(data_group$temp, data_group$total_user,
     main = "Relação entre Temperatura e Utilização do Sistema",
     xlab = "Temperatura Normalizada",
     ylab = "Total de usuários por dia",
     col = "darkorange",
     pch = 16) # pch = 16 deixa os pontos preenchidos (bolinhas)

# linha de tendência para visualizar melhor a relação
abline(lm(total_user ~ temp, data = data_group), col = "blue", lwd = 2)
