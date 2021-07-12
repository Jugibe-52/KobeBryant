# Evaluación en Kaggle
pred <- predict(model_ctree, disparos_no_supervisados, type="prob")
exportar <- data.frame(shot_id = disparos_no_supervisados$shot_id, shot_made_flag = pred)
esportar <- exportar[,c("shot_id","shot_made_flag.Acierta")]
names(esportar) <- c("shot_id","shot_made_flag")
esportar[,"shot_id"] <- as.character(esportar[,"shot_id"])
esportar[,"shot_made_flag"] <- as.character(esportar[,"shot_made_flag"])
write.csv(esportar,file = "C:/Users/PILAR/Documents/R/sumbission.csv", row.names = F, )