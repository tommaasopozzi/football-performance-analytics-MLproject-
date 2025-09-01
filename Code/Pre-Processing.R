library(worldfootballR)
library(tidyverse)
library(ggplot2)

# Preparazione Dataset Calcio ---------------------------------------------

# Costruzione del dataset 
# Per non aspettare i lunghi tempi di scaricamento dei dati
# si consiglia di usare il dataset gi? pronto a rigo 27

big5_player_shooting <- fb_big5_advanced_season_stats(season_end_year= c(2024), stat_type= 'shooting', team_or_player= "player")
big5_player_defense <- fb_big5_advanced_season_stats(season_end_year= c(2024), stat_type= 'defense', team_or_player= "player")
big5_player_possession <- fb_big5_advanced_season_stats(season_end_year= c(2024), stat_type= 'possession', team_or_player= "player")


calcio2 = big5_player_defense %>%
  inner_join(y = big5_player_shooting) %>%
  inner_join(y = big5_player_possession) %>%
  filter(Mins_Per_90 > 12.40) 

# Ho considerato solo quei giocatori che 
# hanno giocato pi? della mediana dei minuti giocati


# write.csv(calcio2, file = 'calcio2.csv', row.names = F)

calcio = read.csv('calcio2.csv')
View(calcio)


# Preparazione Dataset Valori Di Mercato ----------------------------------

# Ottengo i valori di mercato dei giocatori dei top 5 
# campionati europei

# L'azione di Web scraping pu? portare a scaricare i dati ogni volta 
# che si fa girare il codice in modo diverso. Per questo vengono messi a 
# disposizione i dataset gi? pronti nella cartella della consegna.
# Per ottenere risultati conformi si consiglia di passare a rigo 123.

italy <- tm_player_market_values(country_name = "Italy", 2023)
germany <- tm_player_market_values(country_name = "Germany", 2023)
france <- tm_player_market_values(country_name = "France", 2023)
england <- tm_player_market_values(country_name = "England", 2023)
spain <- tm_player_market_values(country_name = "Spain", 2023)


dataset <- rbind(italy, germany, france, england, spain)
  
# Devo mettere a posto il dataset perch? la colonna player name ?
# sfalsata
  
d = dataset %>% 
  select(player_name, player_market_value_euro)

#Primi mille 
(d1 = d[1:1000, ])
ixs = seq(1, nrow(d1), by = 2) #Numeri dispari 
name1 = d1$player_name[ixs]
val1 = d1$player_name[-ixs]
npm1 = cbind(name1, val1) 

#Papu Gomez problema, lo rimuovo dal dataset 
#Paul Pogba Altro Problema, rimuovo anche  lui 


#Secondi mille 
(d2 = d[1001:2000, ])
ixs = seq(1, nrow(d2), by = 2) #Numeri dispari 
name2 = d2$player_name[ixs]
val2 = d2$player_name[-ixs]
npm2 = cbind(name2, val2)  

#Tim Kohler Problema
#Madi Monamay Problema 


#Terzi mille
(d3 = d[2001:3000, ])
ixs = seq(1, nrow(d3), by = 2) #Numeri dispari 
name3 = d3$player_name[ixs]
val3 = d3$player_name[-ixs]
npm3 = cbind(name3, val3) 

#"Ylies Aradj" problema
#"Max Wendt" problema


#Quarti mille 
(d4= d[3001:4000, ])
ixs = seq(1, nrow(d4), by = 2) #Numeri dispari 
name4 = d4$player_name[ixs]
val4 = d4$player_name[-ixs]
npm4 = cbind(name4, val4) 

# Nessun Problema

#Ultime Osservazioni 
(d5 = d[4001:5257, ])
ixs = seq(1, nrow(d5), by = 2) #Numeri dispari 
name5 = d5$player_name[ixs]
val5 = d5$player_name[-ixs]
npm5 = cbind(name5, val5) 
npm5[499:629, ] 

#Nessun Problema 

#Rimuovo i valori anomali 

player = d %>%
  select(player_name) %>%
  filter(player_name != "Ylies Aradj" & player_name != "Max Wendt" & player_name != "Madi Monamay" &
           player_name != "Paul Pogba" & player_name != "Papu G?mez" & player_name != "Tim K?hler")
ixs = seq(1, dim(player)[1], by = 2) #Numeri Pari 
play = player[ixs, ]
value = player[-ixs, ]
dt_fin = cbind(play, value)
dt_fin <- as.data.frame(dt_fin)

# write.csv(dt_fin, 'Value_Player.csv', row.names = F)


value <- read.csv('Value_Player.csv')


converti_prezzo <- function(prezzo) {
  prezzo <- gsub("???", "", prezzo)  # Rimuove il simbolo dell'euro, che non viene mai trascritto ed ? da sostituire all'interno della funzione
  if (grepl("m", prezzo)) {
    prezzo <- gsub("m", "", prezzo)
    return(as.numeric(prezzo) * 1e6)
  } else if (grepl("k", prezzo)) {
    prezzo <- gsub("k", "", prezzo)
    return(as.numeric(prezzo) * 1e3)
  } else {
    return(as.numeric(prezzo))
  }
}


value_fin = value %>%
  mutate(prezzo_numerico = sapply(value, converti_prezzo))

value_fin <- dplyr::select(value_fin, play, prezzo_numerico)

# write.csv(value_fin, 'Valori_fin.csv')


# Join Dataset Calcio e Valori --------------------------------------------

# Pre Processing ----------------------------------------------------------

#val = read.csv('Valori_fin.csv')
val <- value_fin
data <- read.csv('calcio2.csv')

colnames(val)[1] = 'Player'

unique(val$Player)
which(duplicated(val$Player))
#1067 1892 2496 sono i duplicati 

val[c(1067, 1892, 2496), ]
#   X    Player       prezzo_numerico
# 1067    Vitinha       5.0e+07
# 1892    Danilo        2.8e+07
# 2496    Ra?l Garc?a   4.0e+06

val[val$Player == 'Ra?l Garc?a', ]

data[data$Player == 'Ra?l Garc?a', ] 
 
# Non c'? Raul Garcia in data.
# Non mi importa lo rimuovo 

val = val %>% 
  filter(Player != 'Ra?l Garc?a')


val[val$Player == 'Danilo', ] 

# Si tratta di due diversi giocatori.
# Danilo del Nottingham Forest ha valore 28 Milioni
# Mentre quello delle Juve 10 Milioni
# Chiamo DaniloJ il Danilo della juve e DaniloN il Danilo del Nottingham 

val$Player[val$Player == 'Danilo'] = c('DaniloN', 'DaniloJ')

#Controllo che tutto sia stato ben modificato
val[val$Player == 'DaniloN', ]
val[val$Player == 'DaniloJ', ]

#Perfetto, ora faccio lo stesso in data
data[data$Player == 'Danilo', ]
data$Player[data$Player == 'Danilo'] = c('DaniloJ', 'DaniloN')
data[data$Player == 'DaniloN', ]
data[data$Player == 'DaniloJ', ]

#Non mi resta che sistemare Vitinha 

val[val$Player == 'Vitinha', ]
data[data$Player == 'Vitinha', ] 
# In data c'? solo un Vitinha quello del PSG, ossia quello che vale 50Milioni 
# Rimuovo l'altro 

val = val[-286, ] #Tolto il vitinha scarso 

#Posso ora eseguire il join tra i due dataset

val = val %>%
  select(Player, prezzo_numerico)
finale = data %>%
  left_join(val)
View(finale)

finale %>%
  select(Player, prezzo_numerico)

sum(is.na(finale$prezzo_numerico))

# write.csv(finale, 'Calcio_finale.csv', row.names = F)



# PRE-PROCESSING ----------------------------------------------------------


# Pre-Processing
# Combinando info da Data Mining e Stat.Computazionale
# Obiettivo grafici utili?

data <- read.csv("Calcio_finale.csv")


# Vogliamo modificare la variabile Pos, in modo tale che i soli ruoli
# possibili siano MF = Midfielder, DF = Defender, FW = Forward e GK = GoalKeeper

unique(data$Pos)
data$Pos[which(data$Pos == "MF,FW")] <- "MF"
data$Pos[which(data$Pos == "FW,MF")] <- "FW"
data$Pos[which(data$Pos == "DF,MF")] <- "DF"
data$Pos[which(data$Pos == "FW,DF")] <- "FW"
data$Pos[which(data$Pos == "MF,DF")] <- "MF"
data$Pos[which(data$Pos == "DF,FW")] <- "DF"


round(prop.table(table(data$Pos)), 2)
# Classi bilanciate? 

# Guardo se ci sono NA

dim(data)[1]*dim(data)[2]
anyNA(data)
which(is.na(data)) # ti dice l'elemento che manca
length(which(is.na(data))) # numero di NA totali

sapply(data, function(x) sum(is.na(x))) # NA per variabile

# Ci sono molte NA in queste variabili:

# SoT_percent_Standard
# G_per_Sh_Standard 
# G_per_SoT_Standard
# Dist_Standard
# npxG_per_Sh_Expected
# Cmp_percent_Long
# Tkl_percent_Challenges
# Tkld_percent_Take
# Succ_percent_Take
# prezzo_numerico

# Per tutte, tranne prezzo_numerico, ? un problema dovuto a 0 
# nei denominatori di alcune di queste statistiche

data[data$Sh_Standard == 0, ]$SoT_percent_Standard <- 0
data[data$SoT_Standard == 0, ]$G_per_Sh_Standard <- 0
data[data$SoT_Standard == 0, ]$G_per_SoT_Standard <- 0
data[data$SoT_Standard == 0, ]$npxG_per_Sh_Expected <- 0
data[is.na(data$Succ_percent_Take), ]$Succ_percent_Take <- 0
data[is.na(data$Tkld_percent_Take), ]$Tkld_percent_Take <- 0
data[is.na(data$Tkl_percent_Challenges), ]$Tkl_percent_Challenges <- 0
data[is.na(data$Dist_Standard), ]$Dist_Standard <- 0


sapply(data, function(x) sum(is.na(x))) # NA per variabile

write.csv(data, 'Dataset_finale.csv', row.names = F)

# Il problema di queste Na in prezzo_numerico lo devo risolvere.
# E' risolto nel file R Regression_with_PCA in cui 
# alla fine vado ad imputare tramite regressione e ottengo il dataset 
# Dataset_imp
