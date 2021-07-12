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
