#Load Library
library(tidyverse)

#Load in Data
excretion2023 <- read.csv('Data/Raw_Data/Excretion2023_raw.csv')
excretion2024 <- read.csv('Data/Raw_Data/Excretion2024_raw.csv')


#2023
#Convert N (ug/L) to N (umol/min/mg tadpole)
excretion2023$N_umol_L <- ((excretion2023$N_ug_L/1000000)/14.01)*1000000
excretion2023$N_umol <- excretion2023$N_umol_L *0.08 #80ml Samples Taken
excretion2023$TN <- (excretion2023$N_umol / 120) /excretion2023$mass_mg #120 mins

#Convert P (ug/L) to P (umol/min/mg tadpole)
excretion2023$P_umol_L <- ((excretion2023$P_ug_L/1000000)/30.97)*1000000
excretion2023$P_umol <- excretion2023$P_umol_L *0.08 #80ml Samples Taken
excretion2023$TP <- (excretion2023$P_umol / 120) /excretion2023$mass_mg #120 mins

excretion2023$NP <- excretion2023$TN/excretion2023$TP

#2024
#Convert N (ug/L) to N (umol/min/mg tadpole)
excretion2024$N_umol_L <- ((excretion2024$N_ug_L/1000000)/14.01)*1000000
excretion2024$N_umol <- excretion2024$N_umol_L *0.08 #80ml Samples Taken
excretion2024$TN <- (excretion2024$N_umol / 120) /excretion2024$mass_mg #120 mins

#Convert P (ug/L) to P (umol/min/mg tadpole)
excretion2024$P_umol_L <- ((excretion2024$P_ug_L/1000000)/30.97)*1000000
excretion2024$P_umol <- excretion2024$P_umol_L *0.08 #80ml Samples Taken
excretion2024$TP <- (excretion2024$P_umol / 120) /excretion2024$mass_mg #120 mins

excretion2024$NP <- excretion2024$TN/excretion2024$TP

#Select Columns in 2023
excretion2023 <- excretion2023[,c(3,7,10,13,14)]
colnames(excretion2023) <- c("Pop", "logviralload", "NH4", "SRP", "NP")
#Create Long Data
excretion2023 <- excretion2023 %>% pivot_longer(cols = c("NH4":"NP"), names_to = 'nutrient', values_to = 'value')
#write.csv(excretion2023, "Data/Excretion2023.csv")

#Select Columns in 2024
excretion2024 <- excretion2024[,c(2,5,6,7,13,16,17)]
colnames(excretion2024) <- c("rv", "logviralload",'nacl', 'chloride', "NH4", "SRP", "NP")
#Create Long Data
excretion2024 <- excretion2024 %>% pivot_longer(cols = c("NH4":"NP"), names_to = 'nutrient', values_to = 'value')
#write.csv(excretion2024, "Data/Excretion2024.csv")


