# Carregando os pacotes
require(readr)
library(readr)
require(readxl)
library(readxl)
require(caret)
library(caret)
require(dplyr)
library(dplyr)
require(tidyverse)
library(tidyverse)


# Estruturando dados 

teste <- read_csv("teste.csv")
head(teste)
# verificar se existe NA no arquivo teste.csv
ver <- is.na(teste)
table(ver)

# transforma o arquivo em um data.frame
teste = data.frame(teste)

#Observar a estrutura do arquivo teste
glimpse(teste)
class(teste)

#transformar a variavel HIPERTENSO E IDADE EM FATOR
teste$HIPERTENSO<- as.factor(teste$HIPERTENSO)
teste$IDADE<- as.integer(teste$IDADE)
glimpse(teste)
class(teste)

tes_hiper<- filter(teste, HIPERTENSO == "HIPERT")

n<- filter(teste, HIPERTENSO == "NAO_HIPER")
n_hper<- n[1:400, ]

tes_bal = bind_rows(tes_hiper, n_hper)

#Gerando planilha de dados csv
#write.csv(tes_bal,'amostra_bal.csv')
#write.csv(teste,'teste.csv')

