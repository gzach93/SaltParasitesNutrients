#Load Library
library(tidyverse)
library(readxl)
library(ggplot2)
library(survival)
library(coxphf)


#Load in Data
excretion2023 <- read.csv('Data/Raw_Data/New_2023/Excretion2023_raw.csv')
excretion2024 <- read.csv('Data/Raw_Data/Excretion2024_raw.csv')
#mass2023 <- read.csv('Data/Raw_Data/Excretion2023_Mass.csv') #Data already in excretion csv
#viral2023 <- read.csv('Data/Raw_Data/Excretion2023_QPCR_Raw.csv') #Data already in excretion csv
viral2024 <- read.csv('Data/Raw_Data/Excretion2024_QPCR_Raw.csv')

#ttd2023 -- this data is already included in the excretion2023 dataset 
ttd2024 <- read.csv('Data/Raw_Data/TTD2024_Raw.csv')
viral2024_b <- read.csv('Data/Raw_Data/Excretion2024_b_QPCR_Raw.csv')
viral2024_b <- viral2024_b[,-c(14)]
excretion2024_b_design <- read.csv('Data/Raw_Data/Excretion2024_b_design.csv')

#controls_b <- read_xlsx('Data/Raw_Data/Excretion2024_b_controls_raw.xlsx')
#colnames(controls_b) <- c('ind', 'susc', 'eggmass', 'nacl', 'sampletime')

#set.seed(38)
#strat_sample <- controls_b %>%
 # group_by(eggmass, nacl, sampletime) %>%
  #sample_n(size=1)

################################
############ 2023 ##############
################################

#Merge 2023 Mass and Excretion Assay
#Find Mean Mass
#mass2023 <- mass2023 %>%
 # group_by(population, FV3) %>%
  #summarise(mean_mass_g = mean(mass_g))

#Remove Spaces after names
#mass2023[mass2023$population %in% c('Toad '),'population'] <- 'Toad'
#mass2023[mass2023$population %in% c('Pickerel '),'population'] <- 'Pickerel'
#mass2023[mass2023$population %in% c('SEW '),'population'] <- 'SEW'
excretion2023[excretion2023$population %in% c('Pickerel '), 'population'] <- 'Pickerel'

#Merge datasets
#excretion2023 <- merge(x = excretion2023, y = mass2023, by.x = c('population', 'FV3'), by.y = c('population', 'FV3'), all = TRUE)

#Convert N (ug/L) to N (umol/hr/g tadpole)
excretion2023$NH4_umol_L <- ((excretion2023$NH4_ugL/1000000)/14.01)*1000000
excretion2023$NH4_umol <- excretion2023$NH4_umol_L *0.08 #80ml Samples Taken
excretion2023$NH4 <- excretion2023$NH4_umol / 2 /excretion2023$Avg.Mass.Extra.Individuals..g. #120 mins or 2 hrs

#Convert P (ug/L) to P (umol/hr/g tadpole)
excretion2023$SRP_umol_L <- ((excretion2023$SRP_ugL/1000000)/30.97)*1000000
excretion2023$SRP_umol <- excretion2023$SRP_umol_L *0.08 #80ml Samples Taken
excretion2023$SRP <- (excretion2023$SRP_umol) / 2 / excretion2023$Avg.Mass.Extra.Individuals..g. #120 mins or 2 hrs

excretion2023$NP <- excretion2023$NH4/excretion2023$SRP

#Select Columns in 2023
#excretion2023 <- excretion2023[,c(1:10,12,15,16)]
#colnames(excretion2023) <- c("Pop", 'FV3', 'ind', 'species', 'death_hr', 'survival', 'mean_mass_g', "NH4", "SRP", "NP") #add units into names

#qPCR SQ to tadpole viral load 
excretion2023$meanSQ <- rowMeans(excretion2023[,c(14,15)], na.rm = TRUE)
excretion2023$viralload <- (excretion2023$meanSQ * 200) / excretion2023$Nucleic.Acid_ng_uL

#Cutoff of 35 cycles - None -- which means all samples are infected
excretion2023$meanCQ <- rowMeans(excretion2023[,c(12,13)], na.rm = TRUE)
excretion2023[which(excretion2023$meanCQ > 35),]

unique(excretion2023$Population)
excretion2023[excretion2023$Population %in% c('TRL '),'Population'] <- 'TRL'

excretion2023$death <- 1
excretion2023[excretion2023$TTD_Hr %in% c(289),'death'] <- 0
#viral2023[viral2023$population %in% c('Pickerel '), 'population'] <- 'Pickerel'
excretion2023$population <- excretion2023$Population
excretion2023[excretion2023$FV3 %in% c('N'),'Population'] <- 'Control'
excretion2023$Population <- as.factor(excretion2023$Population)
excretion2023$Population <- relevel(excretion2023$Population, ref = 'Control')

#excretion2023$FV3 <- as.factor(excretion2023$FV3)
#excretion2023$FV3 <- relevel(excretion2023$FV3, ref = 'N')


cox.results23 <- coxphf(Surv(TTD_Hr, death) ~ Population, data = excretion2023)

#cox.results23 <-  plyr::dlply(.data = excretion2023[excretion2023$Species %in% c('WF'),], .variables = c("Population"), .fun = function(x){
 #                  results <- coxphf(Surv(TTD_Hr, death) ~ FV3, data = x)})


results23 <- data.frame(matrix(nrow = 9, ncol = 2))
results23[,1] <- substr(names(cox.results23$coefficients), 11, nchar(names(cox.results23$coefficients)))
results23[,2] <- exp(cox.results23$coefficients)

#results23 <- data.frame(matrix(nrow = 6, ncol = 2))
#for(i in 1:length(cox.results23)){
 # results23[i,2] <- exp(cox.results23[[i]]$coefficients)
  #results23[i,1] <- names(cox.results23)[i]
#}
colnames(results23) <- c('Population', 'HazardRatio')

excretion2023 <- merge(x = excretion2023, y = results23, by.x = c('population'), 
              by.y = c('Population'), all = TRUE)

excretion2023$NH4 <- scale(excretion2023$NH4, scale = TRUE)
excretion2023$SRP <- scale(excretion2023$SRP, scale = TRUE)
excretion2023$NP <- scale(excretion2023$NP, scale = TRUE)

#Create Long Data
excretion2023 <- excretion2023 %>% pivot_longer(cols = c("NH4", "NP", "SRP"), names_to = 'nutrient', values_to = 'value')
#write.csv(excretion2023, "Data/Excretion2023.csv")

################################
############ 2024 ##############
################################

#qPCR SQ to tadpole viral load 
viral2024$meanSQ <- rowMeans(viral2024[,c(14,15)], na.rm = TRUE)
viral2024$viralload <- (viral2024$meanSQ * 200) / viral2024$ng.uL

#Cutoff of 35 cycles - 23 samples including two RV trts
viral2024$meanCQ <- rowMeans(viral2024[,c(12,13)], na.rm = TRUE)
viral2024[which(viral2024$meanCQ > 35),'viralload'] <- 0 #Assume uninfected

#Merge datasets
excretion2024 <- merge(x = excretion2024, y = viral2024, by.x = c('ind', 'pop', 'susc', 'FV3'), 
              by.y = c('ind', 'pop', 'susc', 'FV3'), all = TRUE)

mass2024 <- excretion2024[,c('ind', 'pop', 'susc', 'FV3', 'mass_g.x')]
mass2024 <- mass2024 %>% group_by(pop, FV3) %>%
  summarise(mean_mass_g = mean(mass_g.x))

excretion2024 <- merge(x = excretion2024, y = mass2024, by.x = c('pop', 'FV3'), 
                       by.y = c('pop', 'FV3'), all = TRUE)

#2024
#Convert N (ug/L) to N (umol/hr/g tadpole)
excretion2024$NH4_umol_L <- ((excretion2024$NH4_ugL/1000000)/14.01)*1000000
excretion2024$NH4_umol <- excretion2024$NH4_umol_L *0.08 #80ml Samples Taken
excretion2024$NH4 <- (excretion2024$NH4_umol) / 2 / excretion2024$mass_g.x #120 mins
#excretion2024$NH4_meanmass <- (excretion2024$NH4_umol * 1000) / 2 / excretion2024$mean_mass_g #120 mins

#Convert P (ug/L) to P (umol/hr/mg tadpole)
excretion2024$SRP_umol_L <- ((excretion2024$SRP_ugL/1000000)/30.97)*1000000
excretion2024$SRP_umol <- excretion2024$SRP_umol_L *0.08 #80ml Samples Taken
excretion2024$SRP <- (excretion2024$SRP_umol) / 2 /excretion2024$mass_g.x #120 mins
#excretion2024$SRP_meanmass <- (excretion2024$SRP_umol * 1000) / 2 /excretion2024$mean_mass_g #120 mins

excretion2024$NP <- excretion2024$NH4/excretion2024$SRP
#excretion2024$NP_meanmass <- excretion2024$NH4_meanmass/excretion2024$SRP_meanmass

##### Is nutrient rates different if mean tadpole mass is used rather than individual tadpole mass?
#ggplot() + geom_point(data = excretion2024, aes(x = NH4, y = NH4_meanmass)) +
 # theme_classic() +
  #geom_abline(intercept = 0, slope = 1)

#ggplot() + geom_point(data = excretion2024, aes(x = SRP, y = SRP_meanmass)) +
 # theme_classic() +
  #coord_cartesian(ylim = c(0,750), xlim = c(0,750)) + #remove 1 high point
  #geom_abline(intercept = 0, slope = 1)

#ggplot() + geom_point(data = excretion2024, aes(x = NP, y = NP_meanmass)) +
 # theme_classic() +
  #geom_abline(intercept = 0, slope = 1)

#Select Columns in 2024 -- only using the mean mass columns -- it does not seem to make a large difference and it is more similar to 2023
excretion2024 <- excretion2024[,c(1:6,9,10,14,25,30,33,34)]
colnames(excretion2024) <- c("pop", "FV3", "ind", "susc", "nacl", "chloride", "mass_g", 
                             "svl",'stage', 'viralload', "NH4", "SRP", "NP")

excretion2024$NH4 <- scale(excretion2024$NH4, scale = TRUE)
excretion2024$SRP <- scale(excretion2024$SRP, scale = TRUE)
excretion2024$NP <- scale(excretion2024$NP, scale = TRUE)

#Create Long Data
excretion2024 <- excretion2024 %>% pivot_longer(cols = c("NH4", "NP", 'SRP'), names_to = 'nutrient', values_to = 'value')

#Find Average time to death for each population and Hazard Ratio
#ttd2024$eggmass <- substr(ttd2024$pop, nchar(ttd2024$pop), nchar(ttd2024$pop))
#ttd2024$pop <- substr(ttd2024$pop, 1, nchar(ttd2024$pop) - 1)

ttd2024[ttd2024$FV3 %in% ('CTRL'),'pop'] <- 'Control'
ttd2024$pop <- as.factor(ttd2024$pop)
ttd2024$pop <- relevel(ttd2024$pop, ref = 'Control')

#ttd2024$FV3 <- as.factor(ttd2024$FV3)
#ttd2024$FV3 <- relevel(ttd2024$FV3, ref = 'CTRL')


cox.results24 <- coxph(Surv(death_hr, survival) ~ pop, data = ttd2024)
results24 <- data.frame(matrix(nrow = 45, ncol = 2))
results24[,2] <- exp(cox.results24$coefficients)
results24[,1] <- substr(names(cox.results24$coefficients), 4, nchar(names(cox.results24$coefficients)))

#cox.results24 <-  plyr::dlply(.data = ttd2024, .variables = c("pop"), .fun = function(x){
 # results <- coxphf(Surv(death_hr, survival) ~ FV3, data = x)})

#results24 <- data.frame(matrix(nrow = 45, ncol = 2))
#for(i in 1:length(cox.results24)){
 # results24[i,2] <- exp(cox.results24[[i]]$coefficients)
  #results24[i,1] <- names(cox.results24)[i]
#}
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
excretion2024 <- merge(x = excretion2024, y = ttd2024[,c(1,4,5,6)], by.x = c('pop'), by.y = c('pop'))

#write.csv(excretion2024, "Data/Excretion2024.csv")

################################
########### 2024 B #############
################################

#qPCR SQ to tadpole viral load 
viral2024_b$meanSQ <- rowMeans(viral2024_b[,c(12,13)], na.rm = TRUE)
viral2024_b$viralload <- (viral2024_b$meanSQ * 200) / viral2024_b$ng.uL

#Cutoff of 35 cycles - 87 samples (32 from first day of experiment ~ half of the 60 total)
viral2024_b$meanCQ <- rowMeans(viral2024_b[,c(10,11)], na.rm = TRUE)
View(viral2024_b[which(viral2024_b$meanCQ > 35),]) # <- 0 #Assume uninfected

#Merge datasets
viral2024_b <- merge(x = excretion2024_b_design, y = viral2024_b, by.x = c('ind', 'pop', 'NaCl'), 
                       by.y = c('ind', 'pop', 'NaCl'), all = TRUE)

viral2024_b$day <- 0
viral2024_b[viral2024_b$sample_day %in% c('Wednesday'), 'day'] <- 1
viral2024_b[viral2024_b$sample_day %in% c('Thursday'), 'day'] <- 2
viral2024_b[viral2024_b$sample_day %in% c('Friday'), 'day'] <- 3

#There are two higher than expected points -- 1 is way higher
ggplot() +
  geom_point(data = viral2024_b, aes(x = day, y = log(viralload), col = NaCl)) +
  theme_classic() 


mod1 <- nlme::lme(log(viralload) ~ NaCl * day, random = ~1|pop, data = viral2024_b[complete.cases(viral2024_b), ])
summary(mod1)


meanviral2024_b <- viral2024_b %>%
  group_by(NaCl, susc, day) %>%
  summarise(meanviralload = mean(viralload), #Can't do this
            n = n(), 
            sdviralload = sd(viralload),
            lower.ci = meanviralload - 1.96 * (sdviralload/sqrt(n)),
            upper.ci = meanviralload + 1.96 * (sdviralload/sqrt(n)))

ggplot() +
  geom_point(data = meanviral2024_b, aes(x = day, y = log(meanviralload), col = NaCl)) +
  geom_line(data = meanviral2024_b, aes(x = day, y = log(meanviralload), col = NaCl)) +
  theme_classic() +
  facet_grid(~susc)
