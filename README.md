# Homework 1 — Estatística (Bike Sharing)

Trabalho da disciplina de Estatística, Curso de Engenharia de Computação — Universidade Federal do Ceará (UFC), Centro de Tecnologia.

**Professora:** Michela Mulas
**Grupo:**
- Marisa de Fátima Girão Linhares — 581121
- Moisés Barbosa Miranda de Carvalho — 582969
- José Gustavo Cavalcante Barata — 585644
- Álvaro Mendonça Vasconcelos Nunes Campelo — 588071

Repositório: https://github.com/marisalinhares/homework_1_estatistica

## Sobre o projeto

O trabalho analisa o dataset `HW1_bike_sharing.csv`, com registros diários de um sistema de compartilhamento de bicicletas (data, estação do ano, condição climática, temperatura e número de usuários casuais e registrados). Todo o processamento foi feito em **R**, com amostragem individualizada por grupo, análise univariada e bivariada, e uma síntese final integrando os resultados.

## Estrutura da análise

**1. Construção da amostra (`data_group`)**
A partir da maior matrícula do grupo (588071), calculou-se `r = 1 + (588071 mod 100) = 72`. A amostra contém as 300 linhas consecutivas do dataset original, da linha 72 à 371 (13/03/2011 a 06/01/2012).

**2. Caracterização e análise univariada**
- Criação da variável `total_user` (casual + registered).
- Classificação das variáveis (categóricas, numéricas discretas/contínuas, temporal).
- Medidas de tendência central e dispersão (média, mediana, moda, variância, desvio padrão), calculadas manualmente para as 10 primeiras observações e depois verificadas em R para as 300.
- Cálculo de quartis, IQR e identificação de outliers (4 valores atípicos, todos de baixa demanda, ligados a chuva ou ao feriado de Natal).
- Histograma e boxplot de `total_user`.
- Criação da variável binária `low_usage` (dias abaixo do primeiro quartil, Q1 = 3137,25), correspondendo a 25% da amostra.

**3. Características associadas aos dias de baixa utilização**
Investigação de três fatores associados a `low_usage`:
- **Estação do ano:** Verão com maior demanda (média 4464,4; só 4,3% de dias de baixa utilização) e Inverno com a menor (média 2355,6; 88% de baixa utilização).
- **Condições meteorológicas:** demanda cai de forma acentuada com o clima, de céu limpo (média 4131,8) para chuva fraca (média 1844,8; 100% dos dias classificados como baixa utilização).
- **Temperatura:** correlação de Pearson de 0,63 com `total_user`, positiva mas não perfeitamente linear (queda de demanda em temperaturas muito altas).
- Seleção justificada das duas variáveis mais associadas à demanda: **condições meteorológicas** e **temperatura**.

**4. Síntese e aprofundamento**
- Série temporal de `total_user` ao longo dos 300 dias, destacando o pico (04/07/2011) e as quedas mais acentuadas.
- ANOVA de `total_user` por condição climática (F = 38,62; p < 0,001; η² ≈ 0,206).
- Discretização da temperatura em quartis e cálculo do coeficiente de Spearman (ρ ≈ 0,631), confirmando relação predominantemente monotônica.
- Comparação da correlação temperatura × demanda entre dias de baixa utilização e os demais, mostrando que em dias já classificados como `low_usage` outros fatores (clima adverso) pesam mais do que a temperatura.

## Principais conclusões

- A utilização do sistema tem forte componente sazonal e meteorológico.
- Condições climáticas adversas (chuva) e feriados reduzem drasticamente a demanda.
- A temperatura tem relação positiva robusta com o uso, mas perde força em dias já marcados por baixa utilização, sugerindo que o clima predomina sobre a temperatura nesses casos.

## Tecnologias

- **R** (funções base: `aggregate`, `quantile`, `cor`, `aov`, `boxplot`, `hist`, `plot`, `density`)
- Dataset hospedado no GitHub, lido diretamente via `read.csv()`
