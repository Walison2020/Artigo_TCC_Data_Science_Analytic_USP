#------------------------Arvore de Decisão---------------------------------
# Carregando o pacote
install.packages("rpart")
library(rpart) 

tree <- rpart(HIPERTENSO~., 
              data=teste,
              control=rpart.control(maxdepth = 3, cp=0))

# Plotando a árvore
paleta = scales::viridis_pal(begin=.75, end=1)(20)
rpart.plot::rpart.plot(tree,
                       box.palette = paleta) # Paleta de cores

# Valores preditos
teste_pred = predict(tree, teste)
dim(teste_pred)

teste['r'] = teste_pred[,1] - teste_pred[,2]


# Valores esperados e observados
boost0_O_vs_E <- ggplot(teste, aes(x,HIPERTENSOS)) + 
  geom_point(alpha=.7, size=.5, aes(colour='Observado')) +
  geom_path(aes(x,p, colour='Esperado')) + #Ploting
  scale_color_viridis(discrete=TRUE, begin=0, end=.8, name = "Dado: ") +
  theme_bw() +
  theme(legend.position="bottom") +
  # guides(colour = guide_legend(label.position = "bottom")) +
  labs(title="Valores observados vs esperados") +
  scale_y_continuous(name= "hipertensos") +
  scale_x_continuous(name= "x")

boost0_O_vs_E

# Gráfico de resíduos
boost0_res <- ggplot(df, aes(x,r)) + 
  geom_point(alpha=.7, size=.5, aes(colour='Resíduo')) +
  scale_color_viridis(discrete=TRUE, begin=0, end=.8, name = "Dado: ") +
  theme_bw() +
  theme(legend.position="bottom") +
  labs(title="Gráfico de resíduos") +
  scale_y_continuous(name= "r") +
  scale_x_continuous(name= "x")
boost0_res

ggpubr::ggarrange(boost0_O_vs_E, boost0_res, 
                  # labels = c("A", "B"),
                  ncol = 2, nrow = 1)












# Criando o modelo
?rpart
?rpart.control
HIPER_tree <- rpart(HIPERTENSO ~ ., 
                    data = teste, 
                    method = "class", # Usa class pois o target e factor mais poderia ser "anova", "exp" ou "poisson"
                    parms = list(split = "information."), #The splitting index can be gini or information.
                    control = rpart.control(minsplit = 1))

# Visualizando o ganho de informação para cada atributo
HIPER_tree

# Gerando o plot
install.packages("rpart.plot")
library(rpart.plot) 

# Plot
?prp
prp(HIPER_tree, type = 0, extra = 1, under = TRUE, compress = TRUE)



teste %>% filter (teste$IDADE < 52 & teste$DIABETES == 0 & teste$HIPERTENSO == "HIPERT" )
teste %>% filter (teste$HIPERTENSO == "NAO_HIPER" & teste$IDADE >= 52)
