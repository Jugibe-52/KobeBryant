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