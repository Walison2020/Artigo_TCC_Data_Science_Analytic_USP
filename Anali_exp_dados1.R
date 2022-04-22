# Analise Exploratoria de Dados

# Resumo dos dados
head(tes_hiper) #amostra dos HAS
str(tes_hiper)


# Medidas de Tendencia Central
summary(tes_hiper$IDADE)


## Explorando variaveis numericas

# Usando as funcoes
mean(tes_hiper$IDADE) # media das idades entre os HAS
median(tes_hiper$IDADE) # mediana das idades entre os HAS
quantile(tes_hiper$IDADE) # quartins das idades entre os HAS
# quartins em idades 20% e 75% da amostra com HAS
quantile(tes_hiper$IDADE, probs = c(0.20, 0.75))
#quartins da amostra de 5 e 5 percente na amostra HAS
quantile(tes_hiper$IDADE, seq( from = 0, to = 1, by = 0.05))
#varavilidade idade dos HAS
range(tes_hiper$IDADE)
# diferença entre a menor e maior idade da amostra com HAS
diff(range(tes_hiper$IDADE))

# Plots

# Boxplot

boxplot(tes_hiper$IDADE, main = "Boxplot das Idades dos Hipertensos", ylab = "IDADE")

---# boxplot da amostra separados por HAS ou nao, genero e idade
  
ggplot(teste, aes(HIPERTENSO, IDADE, fill = factor(GENERO))) +
  geom_boxplot(alpha = 0.5) +
  labs(title = "Grafico de Hipertensos", x = "HIPERTEMSO", y = "IDADE", fill = "GENERO") +
  theme_bw()

# Histograma
--#Indicam a frequencia de valores (classe de valores)
hist(tes_hiper$IDADE, main = "Histograma das Idades dos Hipertensos ", xlab = "IDADE")


# Medidas de Dispersao
--# Ao interpretar a variancia, numeros maiores indicam que os dados estao
--#espalhados mais amplamente em torno da media.
#O desvio padrao indica, em media, a quantidade de cada valor diferente da media.

sd(tes_hiper$IDADE)

## Explorando variaveis categoricas

# Criando tabelas de contingencia - representam uma unica variavel categorica
# Lista as categorias das variaveis nominais

str(tes_hiper$DIABETES)
table(tes_hiper$DIABETES)
table(tes_hiper$GENERO)
table(tes_hiper$INTERNACAO)
table(tes_hiper$OBESIDADE)
table(tes_hiper$DOENCAS_CARDIACAS)

# Verificando o relacionamento entre 2 variaveis categoricas
# Criando uma crosstable 
# Tabelas de contingencia fornecem uma maneira de exibir 
# as frequencias e frequencias relativas de observacoes 
#, que sao classificados de acordo com duas variaveis categoricas.
# Os elementos de uma categoria sao exibidas atraves das colunas; 
# os elementos de outra categoria sao exibidas sobre as linhas.
install.packages("gmodels")
library(gmodels)
?CrossTable
CrossTable(x = teste$GENERO, y = teste$HIPERTENSO)


## Teste do Qui-quadrado

# Qui Quadrado, simbolizado por χ2 eh um teste de 
# hipoteses que se destina a encontrar um valor da 
# dispersao para duas variaveis nominais, avaliando a 
# associacao existente entre variaveis qualitativas.

# Eh um teste nao parametrico, ou seja, nao depende dos 
# parametros populacionais, como media e variancia.

# O principio basico deste metodo eh comparar proporcoes, 
# isto eh, as possiveis divergencias entre as frequencias 
# observadas e esperadas para um certo evento.
# Evidentemente, pode-se dizer que dois grupos se 
# comportam de forma semelhante se as diferencas entre 
# as frequencias observadas e as esperadas em cada 
# categoria forem muito pequenas, proximas a zero.

# Ou seja, Se a probabilidade eh muito baixa, ele fornece 
# fortes evidencias de que as duas variaveis estao 
# associadas.

CrossTable(x = teste$GENERO, y = teste$HIPERTENSO, chisq = TRUE)
chisq.test(x = teste$GENERO, y = teste$HIPERTENSO)


# Trabalhamos com 2 hipoteses:
# Hipotese nula: As frequencias observadas nao sao diferentes das frequencias esperadas.
# Nao existe diferenca entre as frequencias (contagens) dos grupos.
# Portanto, nao ha associacao entre os grupos
# Hipotese alternativa: As frequencias observadas são diferentes da frequencias esperadas, 
# portanto existe diferenca entre as frequencias.
# Portanto, ha associacao entre os grupos.


