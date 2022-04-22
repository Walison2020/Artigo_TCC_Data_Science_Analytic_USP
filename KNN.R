
#-----------------------------KNN--------------------------------------------

# Split do dataset em treino e teste

head(tes_bal)
indxTreino <- createDataPartition(y = tes_bal$HIPERTENSO , p = 0.75, list = FALSE)
treino<- tes_bal[indxTreino,]
testando<- tes_bal[-indxTreino,]

# Verificando a distirbuicao dos dados originais e das particoes.

prop.table(table(treino$HIPERTENSO)) * 100
prop.table(table(tes_bal$HIPERTENSO)) * 100

# Normalizando os dados

treinandoH <- treino[ ,names(treino) != "HIPERTENSO"] # tirei a coluna HIPERTENSO
preProcValues <- preProcess(x = treinandoH, method = c("center", "scale"))
preProcValues

# Construindo o Modelo
set.seed(4444)
??trainControl
?train
ctrl <- trainControl(method = "repeatedcv", repeats = 3) 
knnH <- train(HIPERTENSO ~ ., data = treino, method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)

# Modelo KNN
knnH

# Numero de Vizinhos x Acuracia
plot(knnH)

# Fazendo previsões
knnPredict <- predict(knnH, newdata = testando)


# Criando a Confusion Matrix
confusionMatrix(knnPredict, testando$HIPERTENSO )


#------------------------
