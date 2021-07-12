#Importamos las librerias necesarias
library(tidyverse)
library(dplyr)
library(nnet)
library(caret)
library(discretization)
library(MASS)
library(ggplot2)
library(AUC)
library(ROCR)
library(prob)
library(MLmetrics)
library(gridExtra)
library(knitr)
library(party)

################### Importación de datos #################################
path <- file.path("C:/Users/PILAR/Documents/R", "Data Sets", "data.csv")
disparos <- read.delim(path, header = TRUE, sep = ",", stringsAsFactors = TRUE)

################## Observacion del Data Set inicial
str(disparos)

################# Vemos que no hay valores faltantes  en los datos supervisados
anyNA(disparos[!is.na(disparos[,"shot_made_flag"]),])

############### Observamos los datos supervisados
str(disparos[!is.na(disparos[,"shot_made_flag"]),])

############### Observamos los datos no supervisados
str(disparos[is.na(disparos[,"shot_made_flag"]),])

##################################################
### Disparos Anomalos de 3 puntos
ggplot(disparos[(disparos[,"shot_zone_basic"]=="Mid-Range" | 
                   disparos[,"shot_zone_basic"]=="In The Paint (Non-RA)" |
                   disparos[,"shot_zone_basic"]=="Restricted Area") & 
                  disparos[,"shot_type"]=="3PT Field Goal",], 
       aes(loc_x,loc_y,colour = factor(shot_made_flag))) +
  geom_point(alpha = I(0.7))
###

### Disparos Anomalos de 2 puntos
ggplot(disparos[(disparos[,"shot_zone_basic"]=="Above the Break 3" | 
                   disparos[,"shot_zone_basic"]=="Backcourt" |
                   disparos[,"shot_zone_basic"]=="Left Corner 3" |
                   disparos[,"shot_zone_basic"]=="Right Corner 3") & 
                  disparos[,"shot_type"]=="2PT Field Goal",], 
       aes(loc_x,loc_y,colour = factor(shot_made_flag))) +
  geom_point(alpha = I(0.7))
###

### Disparos Anomalos Tipo de disparo
ggplot(disparos[(disparos[,"shot_zone_basic"]=="Mid-Range" | 
                   disparos[,"shot_zone_basic"]=="Left Corner 3" |
                   disparos[,"shot_zone_basic"]=="Right Corner 3" |
                   disparos[,"shot_zone_basic"]=="Above the Break 3"),], 
       aes(loc_x,loc_y,colour = factor(shot_type), shape = factor(shot_zone_basic))) +
  geom_point(alpha = I(0.7))
###
######################################################

############# Eliminacion y modificacion de valore erroneos y ruidosos ###########
disparos <- disparos[!((disparos[,"shot_zone_basic"]=="Mid-Range" | 
                          disparos[,"shot_zone_basic"]=="In The Paint (Non-RA)" |
                          disparos[,"shot_zone_basic"]=="Restricted Area") & 
                         disparos[,"shot_type"]=="3PT Field Goal" &
                         disparos[,"shot_distance"] < 10),]

disparos <- disparos[!((disparos[,"shot_zone_basic"]=="Above the Break 3" | 
                          disparos[,"shot_zone_basic"]=="Backcourt" |
                          disparos[,"shot_zone_basic"]=="Left Corner 3" |
                          disparos[,"shot_zone_basic"]=="Right Corner 3") & 
                         disparos[,"shot_type"]=="2PT Field Goal"),]

disparos[(disparos[,"shot_zone_basic"]=="Mid-Range" | 
            disparos[,"shot_zone_basic"]=="In The Paint (Non-RA)" |
            disparos[,"shot_zone_basic"]=="Restricted Area") & 
           disparos[,"shot_type"]=="3PT Field Goal",][,"shot_type"] = "2PT Field Goal"

####### Comprobacion de que solo han sido eliminados 6 valores 
str(disparos)

##### Vemos que sigue habiendo 5000 entradas de datos no supervisados
str(disparos[is.na(disparos[,"shot_made_flag"]),])
########################################################

library(plyr)

############### Modificamos la variable shot_distance en funcion de la posicion x e y
disparos[,"shot_distance"] <- sqrt(disparos[,"loc_x"]^2+disparos[,"loc_y"]^2)

############### Convertimos la variable playoffs en nominal
disparos[,"playoffs"] <- colwise(as.factor)(as.data.frame(disparos[,"playoffs"]))

############### Convertimos la variable shot_made_flag en nominal
disparos[,"shot_made_flag"] <- colwise(as.factor)(as.data.frame(disparos[,"shot_made_flag"]))
levels(disparos[,"shot_made_flag"]) <- c("Falla","Acierta")

############### Convertimos la variable game_date en tipo fecha
disparos[,"game_date"] <- colwise(as.Date)(as.data.frame(disparos[,"game_date"]))

detach("package:plyr", unload=TRUE)


######### Separamos los datos en supervisados y no supervisados
disparos_supervisados <- disparos[!is.na(disparos[,"shot_made_flag"]),]
disparos_no_supervisados <- disparos[is.na(disparos[,"shot_made_flag"]),]

str(disparos_supervisados)
str(disparos_no_supervisados)
anyNA(disparos_supervisados)

################# Cremos dos particiones de train teste 
set.seed(33)
disparos_Partition <- createDataPartition(disparos_supervisados$shot_made_flag,p=0.8,list=FALSE)
disparos_Train <- disparos_supervisados[disparos_Partition,]
disparos_Test <- disparos_supervisados[-disparos_Partition,]

# ------------------Discretización shot_distance-----------------
set.seed(33)
disparos_Partition <- createDataPartition(disparos_Train$shot_made_flag,p=0.85,list=FALSE)
disparos_Train_0 <- disparos_Train[disparos_Partition,]
disparos_Discret <- disparos_Train[-disparos_Partition,]

str(disparos_Discret)

# Discretizacion y factorizacion de datos numericos
set.seed(33)
discr <- chiM(disparos_Discret[,c("shot_distance","shot_made_flag")], alpha = 0.0027)
discr$cutp

# función que discretiza cada valor
discretizacion <- function(numero,puntos_corte) {
  num_punto_corte <- 0
  num_punto_corte <- length(puntos_corte)
  if (num_punto_corte==1){
    if (numero<=puntos_corte[[1]]){
      numero <- 1
    } else {
      numero <- 1
    }
  } 
  if(num_punto_corte>1){
    if (numero<=puntos_corte[[1]]){
      numero <- 1
    } 
    for (int in 1:(num_punto_corte-1)){
      if (numero>puntos_corte[[int]] & numero<=puntos_corte[[int+1]]) {
        numero <- int 
      }
    }
    if (numero>puntos_corte[[num_punto_corte]]) {
      numero <- num_punto_corte
    }
  }
  return(numero)
}

# Funcion que discretiza una columna
discretizaciones <- function(numeros,puntos_corte){
  discretos <- 1:length(numeros)
  i <- 1
  for(numero in numeros){
    numero <- discretizacion(numero,puntos_corte)
    discretos[i]<- numero
    i <- i + 1
  }
  return(discretos)
}

# Implementamos los intervalos de discretizacion
disparos_Train_0[,"shot_distance"] <- discretizaciones(disparos_Train_0[,"shot_distance"],discr$cutp[[1]])
disparos_Test[,"shot_distance"] <- discretizaciones(disparos_Test[,"shot_distance"],discr$cutp[[1]])
disparos_no_supervisados[,"shot_distance"] <- discretizaciones(disparos_no_supervisados[,"shot_distance"],discr$cutp[[1]])

### La variable distancia pasa a ser numérica entera
disparos_Train_0[,"shot_distance"] <- as.integer(disparos_Train_0[,"shot_distance"])
disparos_Test[,"shot_distance"] <- as.integer(disparos_Test[,"shot_distance"])
disparos_no_supervisados[,"shot_distance"] <- as.integer(disparos_no_supervisados[,"shot_distance"])

str(disparos_Train_0)
str(disparos_Test)
str(disparos_no_supervisados)

disparos_Train_1 <- disparos_Train_0[,c("loc_x","loc_y","period"
                                        ,"minutes_remaining","seconds_remaining"
                                        ,"game_date","playoffs","action_type"
                                        ,"shot_distance","shot_zone_area","shot_zone_basic"
                                        ,"opponent","shot_made_flag")]

disparos_Test <- disparos_Test[,c("loc_x","loc_y","period"
                                  ,"minutes_remaining","seconds_remaining"
                                  ,"game_date","playoffs","action_type"
                                  ,"shot_distance","shot_zone_area","shot_zone_basic"
                                  ,"opponent","shot_made_flag")]

disparos_no_supervisados <- disparos_no_supervisados[,c("shot_id","loc_x","loc_y","period"
                                                        ,"minutes_remaining","seconds_remaining"
                                                        ,"game_date","playoffs","action_type"
                                                        ,"shot_distance","shot_zone_area","shot_zone_basic"
                                                        ,"opponent","shot_made_flag")]
str(disparos_Train_1)
str(disparos_Test)
str(disparos_no_supervisados)



### Posicion y aciertos en funcion del shot_distance
ggplot(disparos_Train_1, aes(loc_x,loc_y,colour = factor(shot_distance), shape = factor(shot_made_flag))) +
  geom_point()
ggplot(disparos_Train_1, aes(factor(shot_distance), fill = shot_made_flag)) + geom_bar(position = "fill")

disparos_Train_1 %>% count("shot_distance")

### Posicion y aciertos en funcion de shot_zone_area
g1 <- ggplot(disparos_Train_1, aes(loc_x,loc_y,colour = factor(shot_zone_area))) +
  geom_point() +
  labs(title="Posición según Shot Zone Area")
g2 <- ggplot(disparos_Train_1, aes(factor(shot_zone_area), fill = shot_made_flag)) + geom_bar(position = "fill") +
  labs(title="Rátio aciertos según Shot Zone Area")
grid.arrange(g1, g2, ncol=2)

### Posicion y aciertos en funcion de shot_zone_basic
g3 <- ggplot(disparos_Train_1, aes(loc_x,loc_y,colour = factor(shot_zone_basic))) +
  geom_point() +
  labs(title="Posición según Shot Zone Basic")
g4 <- ggplot(disparos_Train_1, aes(factor(shot_zone_basic), fill = shot_made_flag)) + geom_bar(position = "fill") +
  labs(title="Rátio aciertos según Shot Zone Basic")
grid.arrange(g3, g4, ncol=2)


# Frecuencia según la zona de disparo
p1 <- ggplot(disparos_Train_1, aes(x=fct_infreq(shot_zone_area))) + 
  geom_bar(aes(fill=shot_zone_area)) +
  labs(y="Frequency") +
  theme_bw() +
  theme(axis.text.x=element_text(size=7),
        axis.title.x=element_blank(), 
        legend.position="none") + 
  labs(title="Distribución según Shot Zone Area")


p2 <- ggplot(disparos_Train_1, aes(x=fct_infreq(shot_zone_basic))) + 
  geom_bar(aes(fill=shot_zone_basic)) +
  labs(y="Frequency") +
  theme_bw() +
  theme(axis.text.x=element_text(size=6.3),
        axis.title.x=element_blank(), 
        legend.position="none") + 
  labs(title="Distribución según Shot Zone Basic")


grid.arrange(p1, p2, ncol=2)

# Adaptar el conjunto para su exploracion
disparos_Train_2 <- disparos_Train_1
levels(disparos_Train_2$"shot_made_flag") <- c("0","1")
disparos_Train_2$"shot_made_flag" <- as.integer(disparos_Train_2$"shot_made_flag") - 1


disparos_2 <- disparos
levels(disparos_2$"shot_made_flag") <- c("0","1")
disparos_2$"shot_made_flag" <- as.integer(disparos_2$"shot_made_flag") - 1


####### Precision segun el tipo de disparo
h1 <- disparos_2 %>%
  group_by(action_type) %>%
  summarise(Precision=mean(shot_made_flag),
            counts=n()) %>%
  ggplot(aes(x=reorder(action_type, counts), y=Precision))+ 
  geom_point(aes(colour=Precision), size=3) +
  scale_colour_gradient(low="red", high="blue") +
  labs(title="Precisión según el tipo de lanzamiento - Conjunto Supervisado") +
  theme_bw() +
  theme(axis.title.y=element_blank(),
        legend.position="none",
        plot.title=element_text(hjust=0.5)) +
  coord_flip() +
  geom_col(aes(y=counts/sum(counts)))

h2 <- disparos_2 %>%
  group_by(action_type) %>%
  summarise(Precision=mean(shot_made_flag),
            counts=n()) %>%
  ggplot(aes(x=reorder(action_type, counts), y=Precision))+ 
  geom_point(aes(colour=Precision), size=3) +
  scale_colour_gradient(low="red", high="blue") +
  labs(title="Precisión según el tipo de lanzamiento - Conjunto de entrenamiento") +
  theme_bw() +
  theme(axis.title.y=element_blank(),
        legend.position="none",
        plot.title=element_text(hjust=0.5)) +
  coord_flip() +
  geom_col(aes(y=counts/sum(counts)))

grid.arrange(h1, h2, ncol=2)

### Distribucion de la variable action_type
a = c()
b = c()
c = c()
for (tipo_disparo in levels(disparos_Train_2$"action_type")){
  a <- append(a, tipo_disparo)
  b <- append(b, sum( disparos_Train_2$"action_type" == tipo_disparo)[1])
  c <- append(c, sum( disparos_no_supervisados$"action_type" == tipo_disparo)[1])
}

df_1 <- data.frame(a = a, b = b, c = c)
df_1 %>%
  ggplot(aes(x=reorder(a, b), y=b/sum(b)))+ 
  geom_point(aes(colour=b), size=3) +
  scale_colour_gradient(low="red", high="blue") +
  labs(title="Relación en la distribución en la variable action_type entre datos de entrenamiento y no supervisados") +
  theme_bw() +
  theme(axis.title.y=element_blank(),
        legend.position="none",
        plot.title=element_text(hjust=0.5)) +
  coord_flip() +
  geom_col(aes(y=c/sum(c)))


############# Evolucion temporal
disparos_Train_2 %>%
  group_by(game_date) %>%
  summarise(Accuracy=mean(shot_made_flag)) %>%
  ggplot(aes(x=game_date, y=Accuracy, group=1)) +
  geom_point(aes(colour=Accuracy), size=3) +
  stat_smooth() +
  labs(title="Evolución Temporal de la precisión", x="Season",
       subtitle="Todos las partidos") +
  theme_bw() +
  theme(legend.position="none",
        axis.text.x=element_text(angle=90, hjust=1),
        plot.title=element_text(hjust=0.5))

disparos_Train_2 %>%
  group_by(game_date) %>%
  summarise(Playoff=mean(shot_made_flag[playoffs==1]),
            RegularSeason=mean(shot_made_flag[playoffs==0])) %>%
  ggplot(aes(x=game_date, group=1)) +
  geom_point(aes(y=Playoff, colour="Playoff"), size=3) +
  geom_point(aes(y=RegularSeason, colour="RegularSeason"), size=3) +
  geom_smooth(aes(y=Playoff, colour="Playoff")) +
  geom_smooth(aes(y=RegularSeason, colour="RegularSeason")) + 
  labs(title="Evolución Temporal de la precisión", 
       subtitle="Playoff y temporada regulár",
       x="Season", y="Accuracy") +
  theme_bw() +
  theme(legend.title=element_blank(),
        legend.position="bottom",
        axis.text.x=element_text(angle=90, hjust=1),
        plot.title=element_text(hjust=0.5)) 

# Precision en funcion de los minutos y segundos restantes
l1 <- disparos_Train_2 %>%
  group_by(minutes_remaining) %>%
  summarise(Accuracy=mean(shot_made_flag)) %>%
  ggplot(aes(x=minutes_remaining, y=Accuracy)) + 
  geom_bar(aes(fill=Accuracy), stat="identity") +
  labs(title="Precisión en función de los minutos restantes", x="Minutos Restantes")  +
  theme_bw() +
  theme(legend.position="none",
        plot.title=element_text(hjust=0.5)) 

l2 <- disparos_Train_2 %>%
  group_by(seconds_remaining) %>%
  summarise(Accuracy=mean(shot_made_flag)) %>%
  ggplot(aes(x=seconds_remaining, y=Accuracy)) + 
  geom_bar(aes(fill=Accuracy), stat="identity") +
  labs(title="Precisión en función de los segundos restantes", x="Segundos Restantes")  +
  theme_bw() +
  theme(legend.position="none",
        plot.title=element_text(hjust=0.5)) 

grid.arrange(l1, l2, ncol=2)

# Precision en funcion del oponente
disparos_Train_2 %>%
  group_by(opponent) %>%
  summarise(Accuracy=mean(shot_made_flag))%>%
  ggplot(aes(x=reorder(opponent, -Accuracy), y=Accuracy)) + 
  geom_bar(aes(fill=Accuracy), stat="identity") +
  labs(title="Precisión según el oponente", x="oponente") +
  theme_bw() +
  theme(legend.position="bottom",
        legend.title=element_blank(),
        axis.text.x=element_text(angle=45, hjust=1),
        plot.title=element_text(hjust=0.5))

############## Eliminamos las variables loc_x y loc_y
disparos_Train_1 <- disparos_Train_1[,c("period","minutes_remaining","seconds_remaining"
                                        ,"game_date","playoffs","action_type"
                                        ,"shot_distance","shot_zone_area","shot_zone_basic"
                                        ,"opponent","shot_made_flag")]

disparos_Test <- disparos_Test[,c("period","minutes_remaining","seconds_remaining"
                                  ,"game_date","playoffs","action_type"
                                  ,"shot_distance","shot_zone_area","shot_zone_basic"
                                  ,"opponent","shot_made_flag")]

disparos_no_supervisados <- disparos_no_supervisados[,c("shot_id","period","minutes_remaining","seconds_remaining"
                                                        ,"game_date","playoffs","action_type"
                                                        ,"shot_distance","shot_zone_area","shot_zone_basic"
                                                        ,"opponent","shot_made_flag")]

str(disparos_Train_1)

# Configuramos el trControl 
trControl = trainControl(
  classProbs = TRUE,
  summaryFunction=mnLogLoss,
  method = "cv", 
  number = 5,
  verboseIter = TRUE)

# Espacio de hiperparametros a explorar
mincriterion <- 15:55
mincriterion <- mincriterion/4
mincriterion <- 2^mincriterion
mincriterion <- 1 - 1/mincriterion
mincriterion <- mincriterion^20


############ Entrenamos el modelo
set.seed(33)
model_ctree <- train(
  shot_made_flag ~ ., 
  disparos_Train_1,
  method = "ctree",
  trControl = trControl,
  tuneGrid = expand.grid(mincriterion = mincriterion),
  metric = c("logLoss")
)


########## Visualizamos la eficacia con cada hiperparametro
resultados <- model_ctree$results
resultados <- resultados[,c("mincriterion","logLoss")]
plot(resultados)

######### Visualizamos el mejor modelo
model_ctree$bestTune

####### Vemos el arbol de reglas
plot(model_ctree$finalModel,
     inner_panel=node_inner(model_ctree$finalModel,
                            abbreviate = FALSE,           
                            pval = TRUE,                
                            id = FALSE),
     terminal_panel=node_terminal(model_ctree$finalModel, 
                                  abbreviate = TRUE,
                                  digits = 1,                  
                                  fill = c("white"),          
                                  id = FALSE))



########### Vemos la eficacia del modelo en el conjunto test con las diferentes medidas
mini_set <- disparos_Test

# Matriz de confusión RIPPER
predicciones <- predict(model_ctree, mini_set)
predicciones <- as.factor(predicciones == "Acierta")
aciertos <- as.factor(mini_set[["shot_made_flag"]] == "Acierta")
confusion_matrix <- confusionMatrix(predicciones, aciertos)

# Medidas provabilisticas
pred_prob <- predict(model_ctree, mini_set, type="prob")
auc <- auc(roc(pred_prob[,1], mini_set$"shot_made_flag"))
aciertos_booean <- aciertos == TRUE
logLoss <- LogLoss(pred_prob[,2], aciertos_booean)

# Ordenamos nuestras medidas en un Data Frame
medidas <- data.frame("Accuracy" = as.numeric(confusion_matrix$overall[1]),
                      "Kappa" = as.numeric(confusion_matrix$overall[2]),
                      "Post-Pred-Value" = as.numeric(confusion_matrix$byClass[3]),
                      "Neg Pred Value" = as.numeric(confusion_matrix$byClass[4]),
                      "AUC" = auc,
                      "LogLoss" = logLoss)

medidas



########### Eficacia para subconjuntos del modelo ###############
# Función para calcular medidas en un sub-conjunto
calcularMedidas <- function(datos) {
  mini_conjunto <- datos
  
  # Matriz de confusión RIPPER
  predicciones <- predict(model_ctree, mini_conjunto)
  # predicciones <- as.factor(predicciones == "Acierta")
  # aciertos <- as.factor(mini_conjunto[["shot_made_flag"]] == "Acierta")
  aciertos <- mini_conjunto[["shot_made_flag"]]
  confusion_matrix <- confusionMatrix(predicciones, aciertos)
  
  # Medidas provabilisticas
  pred_prob <- predict(model_ctree, mini_conjunto, type="prob")
  print(length(predicciones))
  if (length(predicciones) < 20) {
    auc = "None"
  }else{
    auc <- auc(roc(pred_prob[,1], mini_conjunto$"shot_made_flag"))
    
  }
  
  aciertos_booean <- aciertos == TRUE
  logLoss <- LogLoss(pred_prob[,2], aciertos_booean)
  
  # Ordenamos nuestras medidas en un Data Frame
  medidas <- data.frame("Accuracy" = as.numeric(confusion_matrix$overall[1]),
                        "Kappa" = as.numeric(confusion_matrix$overall[2]),
                        "Post-Pred-Value" = as.numeric(confusion_matrix$byClass[3]),
                        "Neg Pred Value" = as.numeric(confusion_matrix$byClass[4]),
                        "AUC" = auc,
                        "LogLoss" = logLoss)
  return(medidas)
}




calcularMedidasVar <- function(datos,variable){
  for (nivel in levels(datos[,variable])){
    medidas <- calcularMedidas(datos[datos[,variable] == nivel,])
    print(paste("Resultados para" , variable , "=" , nivel))
    print(medidas)
  }
}


calcularMedidasVar(disparos_Test,"shot_zone_basic")
str(predict(model_ctree, disparos_Test))


# Evaluación en Kaggle
pred <- predict(model_ctree, disparos_no_supervisados, type="prob")
exportar <- data.frame(shot_id = disparos_no_supervisados$shot_id, shot_made_flag = pred)
esportar <- exportar[,c("shot_id","shot_made_flag.Acierta")]
names(esportar) <- c("shot_id","shot_made_flag")
esportar[,"shot_id"] <- as.character(esportar[,"shot_id"])
esportar[,"shot_made_flag"] <- as.character(esportar[,"shot_made_flag"])
write.csv(esportar,file = "C:/Users/PILAR/Documents/R/sumbission.csv", row.names = F, )
