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









