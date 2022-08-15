# Criando dados de treino e dados de teste
set.seed(4444)

# Instalando os pacotes
install.packages("caret")
install.packages("ROCR")
install.packages("e1071")
install.packages("readr")
# Carregando os pacotes
library(caret)
library(ROCR) 
library(e1071) 
library(readr)

testeG <- read_csv("testeG.csv")
View(testeG)
dim(testeG)

# balanceamento das variaveis
tes_hiper_G <- filter(testeG, HIPERTENSO == 1) # seleciona na amostra os HAS
dim(tes_hiper_G)

n_G<- filter(testeG, HIPERTENSO == 0) # seleciona na amostra os nao HAS
dim(n_G)
n_hper_G<-sample_frac(n_G, 0.60) # 60% de forma aleatoria
dim(n_hper_G)
tes_bal_G <- bind_rows(tes_hiper_G, n_hper_G)
dim(tes_bal_G)
testeG <- tes_bal_G[ ,-9] # retirado a variavel tuberculose
dim(testeG)



# Treinando o modelo
log.model <- glm(formula = HIPERTENSO ~ . , family = binomial(link ='logit'), data = testeG)

# Podemos ver que as variaveis Idade, Diabetes e Alcool sao as variaveis mais significantes
summary(log.model)

# Fazendo as previsoes nos dados de teste
library(caTools)
set.seed(4444)

# Split dos dados, de forma aleatoria seleciona 70% da amostra
split = sample.split(testeG$HIPERTENSO, SplitRatio = 0.70)

# os que se encontra como TRUE sao os emementos que foram selecionados
table(split)

# Datasets de treino e de teste
dados_treino= subset(testeG, split == TRUE) # 70% da amostra
dados_teste= subset(testeG, split == FALSE) # 30% da amostra

# Gerando o modelo com a versao final do dataset 
final.log.model <- glm(formula = HIPERTENSO ~., family = binomial(link='logit'), data = dados_treino)


# Resumo
summary(final.log.model)

# Prevendo a acuracia
fitted.probabilities <- predict(final.log.model, newdata = dados_teste, type = 'response')
#previsoes <- round(fitted.probabilities)

# Calculando os valores
fitted.results <- ifelse(fitted.probabilities > 0.5, 1, 0)


# Conseguimos em torna de 82% de acuracia
misClasificError <- mean(fitted.results != dados_teste$HIPERTENSO)
print(paste('Acuracia', 1-misClasificError))

# Criando a confusion matrix
table(dados_teste$HIPERTENSO, fitted.probabilities > 0.5)

---------------------------------------------------------------------------------------------------


# Avaliando a performance do modelo

# Plot do modelo com melhor acurácia

previsoes <- predict(final.log.model, dados_teste , type = "response")
previsoes_finais <- prediction(previsoes, dados_teste$HIPERTENSO)


# Plot
par(mfrow = c(1, 2))
plot.roc.curve(previsoes_finais, title.text = "Curva ROC")




