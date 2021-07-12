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






