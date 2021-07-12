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


  
