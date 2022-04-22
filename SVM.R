# Support Vector Machines 

# Criando dados de treino e dados de teste

head(tes_bal)
indxTreino <- createDataPartition(y = tes_bal$HIPERTENSO , p = 0.75, list = FALSE)
treino<- tes_bal[indxTreino,]
testando<- tes_bal[-indxTreino,]

## Treinando o Modelo
install.packages("kernlab")
library(kernlab)

# Criando o modelo com o kernel vanilladot
classifier <- ksvm(HIPERTENSO~ ., data = treino, kernel = "vanilladot")

# Visualizando resultado do modelo
classifier

# Avaliando a performance do modelo
predictions <- predict(classifier, testando)
head(predictions)
table(predictions, testando$HIPERTENSO)

# Criando um vetor de TRUE/FALSE indicando previsoes corretas/incorretas
agreement <- predictions == testando$HIPERTENSO
table(agreement)
prop.table(table(agreement)) * 100

## Otimizando o Modelo
set.seed(12345)

# Recriando o modelo com outro tipo de kernel
classifier_rbf <- ksvm(HIPERTENSO ~ ., data = treino, kernel = "rbfdot")

# Novas previsoes
predictions_rbf <- predict(classifier_rbf, testando)

# Compare os resultados com a primeira versao do modelo
agreement_rbf <- predictions_rbf == testando$HIPERTENSO
table(agreement_rbf)
prop.table(table(agreement_rbf)) * 100

