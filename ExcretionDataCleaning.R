#Load Library
library(tidyverse)
library(readxl)
library(ggplot2)
library(survival)
library(coxphf)


#Load in Data
excretion2023 <- read.csv('Data/Raw_Data/Excretion2023_raw.csv')
excretion2024 <- read.csv('Data/Raw_Data/Excretion2024_raw.csv')
viral2024 <- read.csv('Data/Raw_Data/Excretion2024_QPCR_Raw.csv')
ttd2024 <- read.csv('Data/Raw_Data/TTD2024_Raw.csv')


################################
############ 2023 ##############
################################

#Convert N (ug/L) to N (umol/hr/g tadpole)
excretion2023$NH4_umol_L <- ((excretion2023$NH4_ugL/1000000)/14.01)*1000000
excretion2023$NH4_umol <- excretion2023$NH4_umol_L *0.08 #80ml Samples Taken
excretion2023$NH4 <- excretion2023$NH4_umol / 2 /excretion2023$Avg.Mass.Extra.Individuals..g. #120 mins or 2 hrs

#Convert P (ug/L) to P (umol/hr/g tadpole)
excretion2023$SRP_umol_L <- ((excretion2023$SRP_ugL/1000000)/30.97)*1000000
excretion2023$SRP_umol <- excretion2023$SRP_umol_L *0.08 #80ml Samples Taken
excretion2023$SRP <- (excretion2023$SRP_umol) / 2 / excretion2023$Avg.Mass.Extra.Individuals..g. #120 mins or 2 hrs

excretion2023$NP <- excretion2023$NH4/excretion2023$SRP

#qPCR SQ to tadpole viral load 
excretion2023$meanSQ <- rowMeans(excretion2023[,c(9,10)], na.rm = TRUE)
excretion2023$viralload <- (excretion2023$meanSQ * 200) / excretion2023$Nucleic.Acid_ng_uL

unique(excretion2023$Population)
excretion2023[excretion2023$Population %in% c('TRL '),'Population'] <- 'TRL'

excretion2023$death <- 1
excretion2023[excretion2023$TTD_Hr %in% c(289),'death'] <- 0
excretion2023$population <- excretion2023$Population
excretion2023[excretion2023$FV3 %in% c('N'),'Population'] <- 'Control'
excretion2023$Population <- as.factor(excretion2023$Population)
excretion2023$Population <- relevel(excretion2023$Population, ref = 'Control')

cox.results23 <- coxphf(Surv(TTD_Hr, death) ~ Population, data = excretion2023)

results23 <- data.frame(matrix(nrow = 6, ncol = 2))
results23[,1] <- substr(names(cox.results23$coefficients), 11, nchar(names(cox.results23$coefficients)))
results23[,2] <- exp(cox.results23$coefficients)

colnames(results23) <- c('Population', 'HazardRatio')

excretion2023 <- merge(x = excretion2023, y = results23, by.x = c('population'), 
              by.y = c('Population'), all = TRUE)

#Create Long Data
excretion2023 <- excretion2023 %>% pivot_longer(cols = c("NH4", "NP", "SRP"), names_to = 'nutrient', values_to = 'value')

excretion2023$scalenutrient <- NA
excretion2023[excretion2023$nutrient %in% c('NH4') & excretion2023$FV3 %in% c('Y'),'scalenutrient'] <- scale(excretion2023[excretion2023$nutrient %in% c('NH4') & excretion2023$FV3 %in% c('Y'),'value'])
excretion2023[excretion2023$nutrient %in% c('SRP') & excretion2023$FV3 %in% c('Y'),'scalenutrient'] <- scale(excretion2023[excretion2023$nutrient %in% c('SRP') & excretion2023$FV3 %in% c('Y'),'value'])
excretion2023[excretion2023$nutrient %in% c('NP') & excretion2023$FV3 %in% c('Y'),'scalenutrient'] <- scale(excretion2023[excretion2023$nutrient %in% c('NP') & excretion2023$FV3 %in% c('Y'),'value'])

excretion2023[excretion2023$nutrient %in% c('NH4') & excretion2023$FV3 %in% c('N'),'scalenutrient'] <- scale(excretion2023[excretion2023$nutrient %in% c('NH4') & excretion2023$FV3 %in% c('N'),'value'])
excretion2023[excretion2023$nutrient %in% c('SRP') & excretion2023$FV3 %in% c('N'),'scalenutrient'] <- scale(excretion2023[excretion2023$nutrient %in% c('SRP') & excretion2023$FV3 %in% c('N'),'value'])
excretion2023[excretion2023$nutrient %in% c('NP') & excretion2023$FV3 %in% c('N'),'scalenutrient'] <- scale(excretion2023[excretion2023$nutrient %in% c('NP') & excretion2023$FV3 %in% c('N'),'value'])

#write.csv(excretion2023, "Data/Excretion2023.csv")

################################
############ 2024 ##############
################################

#qPCR SQ to tadpole viral load 
viral2024$meanSQ <- rowMeans(viral2024[,c(9,10)], na.rm = TRUE)
viral2024$viralload <- (viral2024$meanSQ * 200) / viral2024$ng.uL

#Merge datasets
excretion2024 <- merge(x = excretion2024, y = viral2024, by.x = c('ind', 'pop', 'FV3'), 
              by.y = c('ind', 'pop', 'FV3'), all = TRUE)

mass2024 <- excretion2024[,c('ind', 'pop', 'FV3', 'mass_g.x')]
mass2024 <- mass2024 %>% group_by(pop, FV3) %>%
  summarise(mean_mass_g = mean(mass_g.x))

excretion2024 <- merge(x = excretion2024, y = mass2024, by.x = c('pop', 'FV3'), 
                       by.y = c('pop', 'FV3'), all = TRUE)

#2024
#Convert N (ug/L) to N (umol/hr/g tadpole)
excretion2024$NH4_umol_L <- ((excretion2024$NH4_ugL/1000000)/14.01)*1000000
excretion2024$NH4_umol <- excretion2024$NH4_umol_L *0.08 #80ml Samples Taken
excretion2024$NH4 <- (excretion2024$NH4_umol) / 2 / excretion2024$mass_g.x #120 mins

#Convert P (ug/L) to P (umol/hr/mg tadpole)
excretion2024$SRP_umol_L <- ((excretion2024$SRP_ugL/1000000)/30.97)*1000000
excretion2024$SRP_umol <- excretion2024$SRP_umol_L *0.08 #80ml Samples Taken
excretion2024$SRP <- (excretion2024$SRP_umol) / 2 /excretion2024$mass_g.x #120 mins

excretion2024$NP <- excretion2024$NH4/excretion2024$SRP

#Select Columns in 2024 
excretion2024 <- excretion2024[,c(1:4,8,10,11,18,22,25,26)]
colnames(excretion2024) <- c("pop", "FV3", "ind", "nacl", "svl", "mass_g",
                             'stage', 'viralload', "NH4", "SRP", "NP")


ttd2024[ttd2024$FV3 %in% ('CTRL'),'pop'] <- 'Control'
ttd2024$pop <- as.factor(ttd2024$pop)
ttd2024$pop <- relevel(ttd2024$pop, ref = 'Control')


cox.results24 <- coxph(Surv(death_hr, survival) ~ pop, data = ttd2024)
results24 <- data.frame(matrix(nrow = 45, ncol = 2))
results24[,2] <- exp(cox.results24$coefficients)
results24[,1] <- substr(names(cox.results24$coefficients), 4, nchar(names(cox.results24$coefficients)))

colnames(results24) <- c('Population', 'HazardRatio')

ttd2024 <- merge(x = ttd2024, y = results24, by.x = c('pop'), by.y = 'Population')

ttd2024 <- ttd2024 %>% filter(FV3 == 'RV') %>%
  group_by(pop) %>%
  summarise(num.death = sum(survival),
            N = n(),
            ttd.prop.survival = 1- (num.death/N),
            mean.ttd = mean(ifelse(survival != 0, death_hr, NA), na.rm = TRUE),
            HazardRatio = unique(HazardRatio))

#Merge in average time to death for the population to the excretion 
excretion2024 <- merge(x = excretion2024, y = ttd2024[,c(1,6)], by.x = c('pop'), by.y = c('pop'))

#Create Long Data
excretion2024 <- excretion2024 %>% pivot_longer(cols = c("NH4", "NP", "SRP"), names_to = 'nutrient', values_to = 'value')

excretion2024$scalenutrient <- NA
excretion2024[excretion2024$nutrient %in% c('NH4'),'scalenutrient'] <- scale(excretion2024[excretion2024$nutrient %in% c('NH4'),'value'])
excretion2024[excretion2024$nutrient %in% c('SRP'),'scalenutrient'] <- scale(excretion2024[excretion2024$nutrient %in% c('SRP'),'value'])
excretion2024[excretion2024$nutrient %in% c('NP'),'scalenutrient'] <- scale(excretion2024[excretion2024$nutrient %in% c('NP'),'value'])

#write.csv(excretion2024, "Data/Excretion2024.csv")
