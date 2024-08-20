Este código realiza uma classificação de câncer de mama utilizando um algoritmo de aprendizado de máquina em R, especificamente o algoritmo K-Nearest Neighbors (KNN). O objetivo é classificar tumores como benignos ou malignos com base em características extraídas de exames.

1. Instalação e Carregamento das Bibliotecas
As bibliotecas necessárias para a análise foram instaladas e carregadas:

class: Fornece funções para classificação, incluindo o algoritmo KNN.
gmodels: Oferece funções para análise estatística, incluindo a criação de tabelas de contingência.
readxl: Permite a leitura de arquivos Excel.
2. Carregamento e Preparação dos Dados
Os dados foram carregados a partir de um arquivo CSV contendo informações sobre características dos tumores. A primeira coluna, que provavelmente continha um identificador irrelevante para a análise, foi removida para focar nas características úteis para a classificação.

3. Exploração Inicial dos Dados
Foi feita uma análise preliminar dos dados:

A distribuição dos diagnósticos (benigno e maligno) foi verificada com a função table.
A proporção de cada tipo de diagnóstico foi calculada e exibida em porcentagem.
Um resumo estatístico foi gerado para algumas das variáveis mais relevantes, como radius_mean, area_mean e smoothness_mean.
4. Normalização dos Dados
Uma função de normalização foi criada para escalar os dados entre 0 e 1. A normalização é importante em algoritmos de aprendizado de máquina como o KNN, pois ela assegura que todas as variáveis contribuam igualmente para o cálculo da distância entre os pontos, evitando que variáveis com valores maiores dominem o modelo.

5. Separação dos Dados em Conjuntos de Treinamento e Teste
Os dados foram divididos em dois conjuntos:

BC_train: Conjunto de treinamento com 469 observações.
BC_test: Conjunto de teste com 100 observações.
As labels (rótulos) correspondentes ao diagnóstico (benigno ou maligno) foram separadas para ambos os conjuntos, sendo utilizadas posteriormente na validação do modelo.

6. Construção e Aplicação do Modelo KNN
O modelo KNN foi aplicado utilizando k = 3, ou seja, o algoritmo considera os 3 vizinhos mais próximos para classificar cada ponto no conjunto de teste. O modelo foi treinado com o conjunto de dados BC_train e, em seguida, foi utilizado para prever o diagnóstico no conjunto BC_test.

7. Avaliação do Modelo
Uma tabela de contingência foi gerada com a função CrossTable para comparar as previsões feitas pelo modelo com os rótulos reais do conjunto de teste. A tabela mostra a quantidade de acertos e erros, permitindo uma avaliação da performance do modelo.

Código:

install.packages("class")
library(class)
install.packages("gmodels")
library(gmodels)
library(readxl)

BC <- read.csv("C:\\Users\\euric\\Documents\\GitHub\\Aprendizado de Máquina Cancer de Mama\\BC.csv")

BC<- BC[-1]

table(BC$diagnosis)

round(prop.table(table(BC$diagnosis))*100,digits=1)

summary(BC[c("radius_mean", "area_mean", "smoothness_mean")])

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

BC_normalized <- BC

BC_normalized[, -1] <- apply(BC_normalized[, -1], 2, normalize)

summary(BC_normalized$area_mean)

BC_train <-BC_normalized[1:469, ]

BC_test <-BC_normalized[470:569, ] 

BC_train_labels <- BC[1:469, 1]

BC_test_labels <- BC[470:569, 1]


previsoes <- knn(train = BC_train[, -1], test = BC_test[, -1], cl = BC_train_labels, k = 3)

cross_tab <- CrossTable(x = previsoes, y = BC_test_labels, prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE)