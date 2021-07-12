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
