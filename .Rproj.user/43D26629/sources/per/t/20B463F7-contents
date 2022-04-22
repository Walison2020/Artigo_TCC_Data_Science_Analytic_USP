
#----------------correlação da variaveis----------------------#

install.packages("corrplot")
library(corrplot)
install.packages("corrgram")
library(corrgram)

testeG <- read_csv("testeG.csv")
#modulacao da base de dados, verifica um a um se e numerica
colunas_numericas <- sapply(testeG, is.numeric)
colunas_numericas
#Pacote que verifica a correlação entre as variaveis
data_cor <- cor(testeG[ ,colunas_numericas])
data_cor

# Plot do data_cor
corrplot(data_cor, method = 'color') 

