disparos_Train_0[,"shot_id"]
sum(disparos_Train_0[,"team_name"] != "Los Angeles Lakers")

sum(disparos_Train_0[,"team_id"] != 1610612747)

### Relaciones lineales
### Imagen utilizada
ggplot(disparos_Train_0, aes(lon,loc_x)) + geom_point()
###
### Imagen utilizada
ggplot(disparos_Train_0, aes(lat,loc_y)) + geom_point()
###
disparos_Train_0[,"game_event_id"]

# Vista de las entradas que tenemos según action_type
ggplot(disparos_Train_0%>%filter(combined_shot_type=="Bank Shot"), 
       aes(action_type, fill = factor(shot_made_flag))) + geom_bar()
ggplot(disparos_Train_0%>%filter(combined_shot_type=="Dunk"), 
       aes(action_type, fill = factor(shot_made_flag))) + geom_bar()
ggplot(disparos_Train_0%>%filter(combined_shot_type=="Hook Shot"), 
       aes(action_type, fill = factor(shot_made_flag))) + geom_bar()
ggplot(disparos_Train_0%>%filter(combined_shot_type=="Jump Shot"), 
       aes(action_type, fill = factor(shot_made_flag))) + geom_bar()

disparos_Train_0[,"game_date"]
disparos_Train_0[,"season"]

ggplot(disparos_Train_0, aes(loc_x,loc_y,colour = factor(shot_type))) +
  geom_point()



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







