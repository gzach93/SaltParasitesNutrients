#Load in Libraries
library(ggplot2)
library(readxl)
library(tidyverse)
library(nimble)
library(ggpubr)
library(ggpubr)
library(MASS)
library(ggbeeswarm)
library(mgcv)
library(car)
library(ggforce)

#Laod in Data
#Excretion 2023 Data -- FV3 and Nutrients but no chloride
excretion2023 <- read.csv('Data/Excretion2023.csv')

excretion2023 <- excretion2023[excretion2023$Species %in% c('WF'),]
excretion2023[which(excretion2023$meanCQ > 35), 'viralload'] <- 0

#Excretion 2024 Data -- FV3, NaCl, and Nutrients
excretion2024 <- read.csv('Data/Excretion2024.csv')
excretion2024[excretion2024$susc %in% c("Susceptible "),"susc"] <- "Susceptible"
#Reorder Excretion 2024 Nutrients
excretion2024$nutrient <- factor(excretion2024$nutrient, levels = c('NH4', 'SRP', 'NP'))
#Remove one high P outlier
#excretion2024 <- excretion2024[-which(excretion2024$nutrient == 'SRP' & excretion2024$value > 2000),]



#Plaque Assay Data 
nacl <- read.csv('Data/PlaqueAssay.csv')
nacl <- nacl[nacl$salt %in% c('NaCl'),]
nacl <- nacl[complete.cases(nacl),]
#2024 Field Data
#field2024 <- read.csv('Data/Field2024_Viral_Chloride.csv')


####################################################
####################################################
###### Excretion without Virus or NaCl Free ########
####################################################
####################################################
ttd2023 <- excretion2023 %>% group_by(Population) %>%
  summarise(mean.ttd = mean(TTD_Hr),
            HazardRatio = unique(HazardRatio))

#excretion2023 <- merge(x = excretion2023, y = ttd2023[,c(1,2)], by.x = 'Population', by.y = 'Population')

ttd2024 <- excretion2024 %>% group_by(pop) %>%
  summarise(ttd = unique(mean.ttd),
            HazardRatio = unique(HazardRatio))

control23 <- excretion2023[which(excretion2023$meanCQ > 35),]

control23$scaleHR <- as.vector(scale(control23$HazardRatio, scale = TRUE))

excretion2024$scaleHR <- as.vector(scale(excretion2024$HazardRatio, scale = TRUE))
#excretion2024$scaleTTD <- as.vector(scale(excretion2024$mean.ttd, scale = TRUE))

salt.control24 <- excretion2024[which(excretion2024$FV3 == 'CTRL'),]#50 tadpoles
#salt.control24 <- excretion2024[which(excretion2024$viralload %in% c(0)),]#23 tadpoles
#salt.control24 <- salt.control24[salt.control24$viralload %in% c(0),]

#Make Concentration Numeric
salt.control24[salt.control24$nacl %in% c("0 g/L"),'nacl'] <- 0
salt.control24[salt.control24$nacl %in% c("0.25 g/L"),'nacl'] <- 0.25
salt.control24[salt.control24$nacl %in% c("0.5 g/L"),'nacl'] <- 0.5
salt.control24[salt.control24$nacl %in% c("0.75 g/L"),'nacl'] <- 0.75
salt.control24[salt.control24$nacl %in% c("1.0 g/L"),'nacl'] <- 1
salt.control24$nacl <- as.numeric(salt.control24$nacl)

#Tadpole Excretion without Salt or FV3
salt.control24 <- salt.control24[salt.control24$nacl %in% c(0),] #10 Tadpoles total or 7 tadpole uninfected

n.susc.ctrl.plot <- ggplot(data = salt.control24[salt.control24$nutrient %in% c('NH4'),], aes(y = value, x = susc)) +
  geom_violin() +
  theme_classic() +
  geom_quasirandom(dodge.width = 0.9, varwidth = TRUE) +
  xlab('') +
  ylab('Nitrogen');n.susc.ctrl.plot

p.susc.ctrl.plot <- ggplot(data = salt.control24[salt.control24$nutrient %in% c('SRP'),], aes(y = value, x = susc)) +
  geom_violin() +
  theme_classic() +
  geom_quasirandom(dodge.width = 0.9, varwidth = TRUE) +
  xlab('') +
  ylab('Phosphorus');p.susc.ctrl.plot

np.susc.ctrl.plot <- ggplot(data = salt.control24[salt.control24$nutrient %in% c('NP'),], aes(y = value, x = susc)) +
  geom_violin() +
  theme_classic() +
  geom_quasirandom(dodge.width = 0.9, varwidth = TRUE) +
  xlab('') +
  ylab('Nitrogen:Phosphorus Ratio');np.susc.ctrl.plot

ggarrange(n.susc.ctrl.plot, p.susc.ctrl.plot, np.susc.ctrl.plot,
          nrow = 1, common.legend = T, 
          legend = 'bottom')

n.susc.ctrl.plot <- ggplot(data = salt.control24[salt.control24$nutrient %in% c('NH4'),], aes(y = value, x = HazardRatio)) +
  geom_point() +
  theme_classic() +
  ggtitle('2024') +
  xlab('Hazard Ratio') +
  #xlab('Average Time to Death (Hrs)') +
  ylab('NH4');n.susc.ctrl.plot

p.susc.ctrl.plot <- ggplot(data = salt.control24[salt.control24$nutrient %in% c('SRP'),], aes(y = value, x = HazardRatio)) +
  geom_point() +
  theme_classic() +
  xlab('Hazard Ratio') +
  ggtitle('') +
  #xlab('Average Time to Death (Hrs)') +
  ylab('SRP');p.susc.ctrl.plot

np.susc.ctrl.plot <- ggplot(data = salt.control24[salt.control24$nutrient %in% c('NP'),], aes(y = value, x = HazardRatio)) +
  geom_point() +
  theme_classic() +
  ggtitle('') +
  xlab('Hazard Ratio') +
  #xlab('Average Time to Death (Hrs)') +
  ylab('Nitrogen:Phosphorus Ratio');np.susc.ctrl.plot

plot.24.control <- ggarrange(n.susc.ctrl.plot, p.susc.ctrl.plot, np.susc.ctrl.plot,
          nrow = 1, common.legend = T, 
          legend = 'bottom')


salt.control.mod.SRP <- lm(value ~ scaleHR, data = salt.control24[salt.control24$nutrient %in% c('SRP'),])
salt.control.mod.NH4 <- lm(value ~ scaleHR, data = salt.control24[salt.control24$nutrient %in% c('NH4'),])
salt.control.mod.NP <- lm(value ~ scaleHR, data = salt.control24[salt.control24$nutrient %in% c('NP'),])

summary(salt.control.mod.SRP)
summary(salt.control.mod.NH4)
summary(salt.control.mod.NP)

shapiro.test(salt.control.mod.NH4$residuals)
shapiro.test(salt.control.mod.SRP$residuals)
shapiro.test(salt.control.mod.NP$residuals)


salt.control.mod.SRP.23 <- lm(value ~ scaleHR, data = control23[control23$nutrient %in% c('SRP'),])
salt.control.mod.NH4.23 <- lm(value ~ scaleHR, data = control23[control23$nutrient %in% c('NH4'),])
salt.control.mod.NP.23 <- lm(value ~ scaleHR, data = control23[control23$nutrient %in% c('NP'),])

summary(salt.control.mod.SRP.23)
summary(salt.control.mod.NH4.23)
summary(salt.control.mod.NP.23)

n.susc.ctrl.plot.23 <- ggplot(data = control23[control23$nutrient %in% c('NH4'),], aes(y = value, x = scaleHR)) +
  geom_point() +
  theme_classic() +
  xlab('Hazard Ratio') +
  ggtitle('2023') +
  #xlab('Average Time to Death (Hrs)') +
  ylab('NH4');n.susc.ctrl.plot.23

p.susc.ctrl.plot.23 <- ggplot(data = control23[control23$nutrient %in% c('SRP'),], aes(y = value, x = scaleHR)) +
  geom_point() +
  theme_classic() +
  ggtitle('') +
  xlab('Hazard Ratio') +
  #xlab('Average Time to Death (Hrs)') +
  ylab('SRP');p.susc.ctrl.plot.23

np.susc.ctrl.plot.23 <- ggplot(data = control23[control23$nutrient %in% c('NP'),], aes(y = value, x = scaleHR)) +
  geom_point() +
  theme_classic() +
  ggtitle('') +
  xlab('Hazard Ratio') +
  #xlab('Average Time to Death (Hrs)') +
  ylab('Nitrogen:Phosphorus Ratio');np.susc.ctrl.plot.23

plot.23.control <- ggarrange(n.susc.ctrl.plot.23, p.susc.ctrl.plot.23, np.susc.ctrl.plot.23,
          nrow = 1, common.legend = T, 
          legend = 'bottom')

ggarrange(plot.23.control, plot.24.control,
          ncol = 1)

##############################################################
##############################################################
################# 2023 Excretion Assay #######################
##############################################################
##############################################################
excretion2023 <- excretion2023[excretion2023$FV3 %in% c('Y'),]

excretion2023$logviralload <- log10(excretion2023$viralload + 1)

cor(excretion2023$logviralload, excretion2023$HazardRatio,
    method = 'pearson', use = "complete.obs")

excretion2023$scalelogviralload <- as.vector(scale(excretion2023$logviralload, scale = TRUE))
excretion2023$scaleHR <- as.vector(scale(excretion2023$HazardRatio, scale = TRUE))
#excretion2023$scaleTTD <- as.vector(scale(excretion2023$mean.ttd, scale = TRUE))

#cor(excretion2023$scalelogviralload, excretion2023$scaleHR,
 #   method = 'pearson', use = "complete.obs")

#excretion2023$scaleTTD <- as.vector(scale(excretion2023$TTD_Hr, scale = TRUE))
#cor(scale(excretion2023$logviralload, scale = TRUE), scale(excretion2023$TTD_Hr, scale = TRUE),
 #   method = 'pearson', use = "complete.obs")


#excretion2023 <- excretion2023[excretion2023$FV3 %in% c('Y'),]

excretion2023$susc <- 'Susceptible'
#excretion2023[excretion2023$Population %in% c('SEW', 'MIN'), 'susc'] <- 'Intermediate'
excretion2023[excretion2023$Population %in% c('SQR', 'RR', 'MIN'), 'susc'] <- 'Tolerant'

#NH4 -
NH4.mod2023 <- lm(value ~ scalelogviralload*scaleHR, 
                  data = excretion2023[excretion2023$nutrient %in% c('NH4'),])# & excretion2023$Population %in% c('TRL', 'STB', 'SQR', 'RR'),])
summary(NH4.mod2023)
vif(NH4.mod2023, type = 'predictor')

#SRP - 
SRP.mod2023 <- lm(value  ~ scalelogviralload*scaleHR, data = excretion2023[excretion2023$nutrient %in% c('SRP'),])# & excretion2023$Population %in% c('SEW', 'MIN', 'SQR', 'RR'),])
summary(SRP.mod2023) 
vif(SRP.mod2023, type = 'predictor')

#N:P Ration - Not Significant Decrease
NP.mod2023 <- lm(value ~ scalelogviralload*scaleHR, data = excretion2023[excretion2023$nutrient %in% c('NP'),])# & excretion2023$Population %in% c('SEW', 'MIN', 'SQR', 'RR'),])
summary(NP.mod2023)
vif(NP.mod2023, type = 'predictor')

#Create Model Fit
no.salt.data23 <- data.frame(expand_grid('scalelogviralload' = seq(-3,2,by = .25), 'scaleHR' = c(-0.293319730, -1.065011194, -0.009168597, -1.130387585,  0.831330571,  1.666556535)))
NH4.pred <- cbind(no.salt.data23, data.frame(predict(NH4.mod2023, newdata = no.salt.data23, interval = c('confidence'))))
SRP.pred <- cbind(no.salt.data23, data.frame(predict(SRP.mod2023, newdata = no.salt.data23, interval = c('confidence'))))
NP.pred <- cbind(no.salt.data23, data.frame(predict(NP.mod2023, newdata = no.salt.data23, interval = c('confidence'))))
NH4.pred$nutrient <- 'NH4'
SRP.pred$nutrient <- 'SRP'
NP.pred$nutrient <- 'NP'
pred23 <- rbind(NH4.pred, SRP.pred, NP.pred)
#pred23$logviralload <- seq(1,3,by = .25)
excretion2023$nutrient <- factor(excretion2023$nutrient, levels = c('NH4', 'SRP', 'NP'))
pred23$nutrient <- factor(pred23$nutrient, levels = c('NH4', 'SRP', 'NP'))

#2023 Excretion Assay Plot
ggplot() +
  geom_point(data = excretion2023, aes(x = scalelogviralload, y = value, col = scaleHR)) +
  xlab('Scaled Log Viral Load') + theme_classic() + ylab('Nutrient Excretion') +
  facet_wrap(~nutrient, scales = "free") +
  geom_line(data = pred23, aes(x = scalelogviralload, y = fit, col = scaleHR, group = scaleHR), lwd = 1.5) +
  geom_ribbon(data = pred23, aes(x = scalelogviralload, ymax = upr, ymin = lwr, fill = scaleHR, group = scaleHR), 
              color = NA, alpha = .2) +
  ggtitle("Chloride Free") #+
  #scale_fill_manual(values = c("Susceptible" = "grey40", "Tolerant" = "forestgreen")) +
  #scale_color_manual(values = c("Susceptible" = "grey40", "Tolerant" = "forestgreen"))
  
#Linear Model Checks
shapiro.test(NH4.mod2023$residuals)
shapiro.test(SRP.mod2023$residuals)
shapiro.test(NP.mod2023$residuals)

plot(NH4.mod2023)
plot(SRP.mod2023)
plot(NP.mod2023)

##############################################################
##############################################################
################# 2024 Excretion Assay #######################
##############################################################
##############################################################
excretion2024$logviralload <- log10(excretion2024$viralload + 1)
  
cor(excretion2024$logviralload, excretion2024$HazardRatio,
    method = 'pearson', use = "complete.obs")
cor(excretion2024$logviralload, excretion2024$nacl,
    method = 'pearson', use = "complete.obs")
cor(excretion2024$HazardRatio, excretion2024$nacl,
    method = 'pearson', use = "complete.obs")

excretion2024$scalelogviralload <- as.vector(scale(excretion2024$logviralload, scale = TRUE))
excretion2024$scaleNaCl <- as.vector(scale(excretion2024$nacl, scale = TRUE))
excretion2024$scaleHR <- as.vector(scale(excretion2024$HazardRatio, scale = TRUE))

##### NH4 #####
#NH4 Salt Trt - Significant Increase with Viral Load, NaCl and interaction Not Significant
NH4.mod2024 <- lm(value ~ scalelogviralload * scaleNaCl * scaleHR, data = excretion2024[excretion2024$nutrient %in% c('NH4'),])# & excretion2024$nacl > 0,])
summary(NH4.mod2024)
vif(NH4.mod2024, type = 'predictor')

#NH4 Salt Free - Increase Not Significant 
NH4.mod2024.0 <- lm(value ~ scalelogviralload * scaleHR, data = excretion2024[excretion2024$nutrient %in% c('NH4') & excretion2024$chloride %in% c(0),])
summary(NH4.mod2024.0)
vif(NH4.mod2024.0, type = 'predictor')

#### SRP ####
#SRP Salt Trt - No Significance Relatioships
SRP.mod2024 <- lm(value ~ scalelogviralload * scaleNaCl * scaleHR, data = excretion2024[excretion2024$nutrient %in% c('SRP'),])# & excretion2024$nacl > 0,]) #& excretion2024$nacl %in% c(0.25, 0.50, 0.75, 1),])
summary(SRP.mod2024)
vif(SRP.mod2024, type = 'predictor')


#SRP Salt Free - Decrease Not Significant
SRP.mod2024.0 <- lm(value ~ scalelogviralload * scaleHR, data = excretion2024[excretion2024$nutrient %in% c('SRP') & excretion2024$chloride %in% c(0),])
summary(SRP.mod2024.0)
vif(SRP.mod2024.0, type = 'predictor')

#### NP Ratio ####
#NP Ratio - Increase with NaCl and Viral Load - Interaction Decrease - All Significant
NP.mod2024 <- lm(value ~ scalelogviralload * scaleNaCl * scaleHR, data = excretion2024[excretion2024$nutrient %in% c('NP'),])# & excretion2024$nacl > 0,]) #& excretion2024$nacl %in% c(0.25, 0.50, 0.75, 1),])
summary(NP.mod2024)
vif(NP.mod2024, type = 'predictor')

#NP Ratio - Increase Not Significant
NP.mod2024.0 <- lm(value ~ scalelogviralload * scaleHR, data = excretion2024[excretion2024$nutrient %in% c('NP') & excretion2024$chloride %in% c(0),])
summary(NP.mod2024.0)
vif(NP.mod2024.0, type = 'predictor')

#Model Fits - Salt Trts
newdata.nacl <- data.frame(expand.grid('scalelogviralload' = seq(-2,2.5,by = .25), 'scaleNaCl' = unique(excretion2024$scaleNaCl), 'scaleHR' = unique(excretion2024$scaleHR)))
NH4.pred24 <- cbind(newdata.nacl,data.frame(predict(NH4.mod2024, newdata = newdata.nacl, interval = c('confidence'))))
SRP.pred24 <- cbind(newdata.nacl, predict(SRP.mod2024, newdata = newdata.nacl, interval = c('confidence')))
NP.pred24 <- cbind(newdata.nacl, predict(NP.mod2024, newdata = newdata.nacl, interval = c('confidence')))
NH4.pred24$nutrient <- 'NH4'
SRP.pred24$nutrient <- 'SRP'
NP.pred24$nutrient <- 'NP'
pred24 <- rbind(NH4.pred24, SRP.pred24, NP.pred24)
#pred24$viralload <- seq(0,16,by = .25)
#pred24$nutrient <- factor(pred24$nutrient, levels = c('NH4', 'SRP', 'NP'))
#pred24$nacl <- rep(rep(c(250,500,750,1000), each = 65), times = 3)
#pred24$susc <- rep(rep(c('Susceptible', 'Tolerant'), each = 84), times = 3)

#Model Fits - Salt Free Trt
newdata.nosalt <- data.frame(expand.grid('scalelogviralload' = seq(0,7,by = .25), 'susc' = c('Susceptible', 'Tolerant')))
NH4.pred24.0 <- cbind(newdata.nosalt, predict(NH4.mod2024.0, newdata = newdata.nosalt, interval = c('confidence')))
SRP.pred24.0 <- cbind(newdata.nosalt, predict(SRP.mod2024.0, newdata = newdata.nosalt, interval = c('confidence')))
NP.pred24.0 <- cbind(newdata.nosalt, predict(NP.mod2024.0, newdata = newdata.nosalt, interval = c('confidence')))
NH4.pred24.0$nutrient <- 'NH4'
SRP.pred24.0$nutrient <- 'SRP'
NP.pred24.0$nutrient <- 'NP'
pred24.0 <- rbind(NH4.pred24.0, SRP.pred24.0, NP.pred24.0)
#pred24.0$viralload <- seq(1,6,by = .25)
#pred24.0$nutrient <- factor(pred24.0$nutrient, levels = c('NH4', 'SRP', 'NP'))
#pred24.0$susc <- rep(rep(c('Susceptible', 'Tolerant'), each = 21), times = 3)

#pred24$susc <- 'Susceptible'
#pred24[pred24$scaleHR %in% c(-0.9748989),'susc'] <- 'Tolerant'


#Salt Trt Plot
ggplot() +
  facet_wrap(~nutrient, scales = "free") +
  geom_line(data = pred24, aes(x = scalelogviralload, y = fit, color = scaleNaCl, group = interaction(scaleNaCl, scaleHR)), lwd = 1.5) +
  geom_ribbon(data = pred24, aes(x = scalelogviralload, ymax = upr, ymin = lwr, group = interaction(scaleNaCl, scaleHR), fill = scaleNaCl), 
              color = NA, alpha = .2) +
  geom_point(data = excretion2024, aes(x = scalelogviralload, y = value, color = scaleNaCl)) +
  xlab('Scaled Log Viral Load') + theme_classic() + ylab('Nutrient Excretion') +
  ggtitle("Chloride Added")

#Linear Model Checks
shapiro.test(NH4.mod2024$residuals)
shapiro.test(SRP.mod2024$residuals)
shapiro.test(NP.mod2024$residuals)
plot(NH4.mod2024)
plot(SRP.mod2024)
plot(NP.mod2024)

shapiro.test(NH4.mod2024.0$residuals)
shapiro.test(SRP.mod2024.0$residuals)
shapiro.test(NP.mod2024.0$residuals)
plot(NH4.mod2024.0)
plot(SRP.mod2024.0)
plot(NP.mod2024.0)

##############################################################
##############################################################
####### Does Viral Load differ by Tolerance? #################
##############################################################
##############################################################
ggplot(data = excretion2023[excretion2023$FV3 %in% c('Y'),], aes(x = susc, y = logviralload)) +
  geom_violin() +
  geom_sina() +
  theme_classic() +
  ggtitle('2023')
t.test(logviralload ~ susc, excretion2023[excretion2023$FV3 %in% c('Y'),])

ggplot(data = excretion2024[excretion2024$FV3 %in% c('RV'),], aes(x = susc, y = logviralload)) +
  geom_violin() +
  geom_sina() +
  theme_classic() +
  ggtitle('2024')

t.test(logviralload ~ susc, excretion2023[excretion2024$FV3 %in% c('RV'),])



##############################################################
##############################################################
################### Plaque Assay #############################
##############################################################
##############################################################

#Plaque Assay Plot
ggplot() +
  geom_point(data = nacl, aes(x = chloride, y = plaques)) +
  theme_classic() +
  xlab('Chloride Concetration (mg/L)') +
  ylab('Number of Plaques') +
  facet_grid(~salt, scales="free")

#Create Squared Term
#nacl$sq.chloride <- nacl$chloride^2

#Plaque Model - 
#plaque.mod <- glm.nb(plaques ~ chloride + sq.chloride, data = nacl)
#plaque.mod1 <- glm.nb(plaques ~ chloride, data = nacl)

#Linear vs Nonlinear - Nonlinear has lower AIC
#summary(plaque.mod) # Both Terms significant
#summary(plaque.mod1) #Significant increase
#AIC(plaque.mod)
#AIC(plaque.mod1)

#Model Checks
#(length(residuals(plaque.mod, type="pearson")[residuals(plaque.mod, type="pearson") > 2]) + length(residuals(plaque.mod, type="pearson")[residuals(plaque.mod, type="pearson") < -2]))/length(residuals(plaque.mod, type="pearson"))
#plot(residuals(plaque.mod, type = 'pearson') ~ plaque.mod$fitted.values)
#abline(h = 0)

#plot(residuals(plaque.mod1, type = 'pearson') ~ plaque.mod$fitted.values)
#abline(h = 0)


#Model Fits
#plaque.pred <- data.frame(predict(plaque.mod, newdata = data.frame('chloride' = seq(0,1900,by = 100),
 #                                                                  'sq.chloride' = seq(0,1900,by = 100)^2), 
  #  se.fit = TRUE, type = 'response'))

plaque.mod <- gamm(plaques ~ s(chloride, k = 3), data = nacl, family = poisson())
summary(plaque.mod$gam)

#Model Plot
ggplot() +
  geom_point(data = nacl, aes(x = chloride, y = plaques)) +
  geom_line(data = plaque.pred, aes(x = seq(0,1900,by = 100), y = fit), lwd = 1.5, col = 'darkgreen') +
  theme_classic() +
  geom_ribbon(data = plaque.pred, aes(x = seq(0,1900,by = 100), ymin = (fit - (2*se.fit)), ymax = (fit + (2*se.fit))),
              col = NA, fill = 'darkgreen', alpha = .2) +
  xlab('Chloride Concetration (mg/L)') +
  ylab('Number of Plaques') +
  ylim(c(0,25))

                              
##############################################################
##############################################################
################ In Vivo Virus vs NaCl #######################
##############################################################
##############################################################

#Base Plot
ggplot() +
  geom_point(data = excretion2024, aes(x = nacl, y = logviralload)) +
  ylab('Log Viral Load') + theme_classic() + xlab('NaCl (mg/L)')

#Model - Increase Not Significant
VL.mod2024 <- lm(logviralload ~ nacl, data = excretion2024)
summary(VL.mod2024)

#Model Fits
VL.pred24 <- data.frame(predict(VL.mod2024, newdata = data.frame('chloride' = seq(0,650,by = 50)), interval = c('confidence')))
VL.pred24$chloride <- seq(0, 650, by = 50)

#Model Fit Plot
ggplot() +
  geom_point(data = excretion2024, aes(x = chloride, y = logviralload)) +
  geom_line(data = VL.pred24, aes(x = chloride, y = fit), color = 'darkgreen', lwd = 1.5) +
  geom_ribbon(data = VL.pred24, aes(x = chloride, ymax = upr, ymin = lwr), fill = 'darkgreen', 
              color = NA, alpha = .2) +
  ylab('Log Viral Load') + theme_classic() + xlab('Chloride (mg/L)')

##############################################################
##############################################################
################ 2024 Field Viral Data #######################
##############################################################
##############################################################

#Convert to ppm or mg/L
field2024$SRP <- field2024$SRP..ppb./1000
field2024$NH4 <- field2024$NH3.N..ppb./1000
field2024$NP <- field2024$NH4/field2024$SRP
field2024$chloride2 <- field2024$Chloride..ppm.^2

#Summarize by site 
field2024 <- field2024 %>%
  group_by(site) %>%
  summarise(mean.chloride = unique(Chloride..ppm.),
            viral_load = mean(log.load.FV3, na.rm = TRUE),
            TN = unique(SRP),
            TP = unique(NH4),
            NP = unique(NP))
field2024$chloride2 <- field2024$mean.chloride^2


#Basic Viral vs NaCl Plot
ggplot() +
  geom_point(data = field2024, aes(y = viral_load, x = mean.chloride)) +
  theme_classic()

#Models Viral vs NaCl - Not Significant
field.viral <- lm(viral_load ~ mean.chloride, data = field2024)
field.viral2 <- lm(viral_load ~ mean.chloride + chloride2, data = field2024)

#Linear vs Nonlinear - Linear no relationship is better 
summary(field.viral)
summary(field.viral2)
AIC(field.viral)
AIC(field.viral2)

#Model Fits
VL.field.pred24 <- data.frame(predict(field.viral, 
                                      newdata = data.frame('mean.chloride' = seq(0,60,by = 10)), 
                                      interval = 'confidence', re.form = NA))
VL.field.pred24$chloride <- seq(0,60,by = 10)

#Model Fit Plot
ggplot() +
  geom_point(data = field2024, aes(y = viral_load, x = mean.chloride)) +
  geom_line(data = VL.field.pred24, aes(x = chloride, y = fit), lwd = 1.5) +
  geom_ribbon(data = VL.field.pred24, aes(x = chloride, ymax = upr, 
                                          ymin = lwr),
              color = NA, alpha = .2) +
    theme_classic()

##############################################
##############################################
########## Caterpillar Model Figure ##########
##############################################
##############################################

control.results <- rbind(data.frame(summary(salt.control.mod.NH4)[4]), 
                         data.frame(summary(salt.control.mod.SRP)[4]), 
                         data.frame(summary(salt.control.mod.NP)[4]))
control.results$nutrient <- rep(c('NH4', 'SRP', 'N:P Ratio'), each = 2)
control.results$trt <- 'Control' 
control.results$FV3 <- 'N'
control.results$NaCl <- 'N'


resultsNH4 <- rbind(data.frame(summary(NH4.mod2023)[4]),
                    data.frame(summary(NH4.mod2024)[4]))
resultsNH4$trt <- c(rep('FV3', times = 4), rep('FV3 + NaCl', times = 8))
resultsNH4$nutrient <- 'NH4'
resultsNH4$FV3 <- 'Y'
resultsNH4$NaCl <- c(rep('N', times = 4), rep('Y', times = 8))


resultsSRP <- rbind(data.frame(summary(SRP.mod2023)[4]),
                    data.frame(summary(SRP.mod2024)[4]))
resultsSRP$trt <- c(rep('FV3', times = 4), rep('FV3 + NaCl', times = 8))
resultsSRP$nutrient <- 'SRP'
resultsSRP$FV3 <- 'Y'
resultsSRP$NaCl <- c(rep('N', times = 4), rep('Y', times = 8))

resultsNP <- rbind(data.frame(summary(NP.mod2023)[4]),
                    data.frame(summary(NP.mod2024)[4]))
resultsNP$trt <- c(rep('FV3', times = 4), rep('FV3 + NaCl', times = 8))
resultsNP$nutrient <- 'N:P Ratio'
resultsNP$FV3 <- 'Y'
resultsNP$NaCl <- c(rep('N', times = 4), rep('Y', times = 8))

results <- rbind(resultsNH4, resultsSRP, resultsNP)

results$param <- gsub('[0-9]+', '', rownames(results))
#results[results$param %in% c('(Intercept)'),'param'] <- 'Intercept'

results$sig <- 'N'
results[which(results$coefficients.Pr...t.. < 0.05),'sig'] <- 'Y'

results$nutrient <- factor(results$nutrient, levels = c('NH4', 'SRP', 'N:P Ratio'))

results[results$param %in% c('scaleHR'),'param'] <- 'Hazard Ratio'
results[results$param %in% c('scalelogviralload'),'param'] <- 'Viral Load'
results[results$param %in% c('scalelogviralload:scaleHR'),'param'] <- 'Hazard Ratio * Viral Load'
results[results$param %in% c('scalelogviralload:scaleNaCl'),'param'] <- 'Viral Load * NaCl'
results[results$param %in% c('scalelogviralload:scaleNaCl:scaleHR'),'param'] <- 'Hazard Ratio * Viral Load * NaCl'
results[results$param %in% c('scaleNaCl'),'param'] <- 'NaCl'
results[results$param %in% c('scaleNaCl:scaleHR'),'param'] <- 'Hazard Ratio * NaCl'

results$param <- factor(results$param, levels = c('(Intercept)', 'Hazard Ratio', 'Viral Load',
                                                 'Hazard Ratio * Viral Load', 'NaCl',
                                                 'Hazard Ratio * NaCl',
                                                 'Viral Load * NaCl',
                                                 'Hazard Ratio * Viral Load * NaCl'))

ggplot() +
  geom_point(data = results[!(results$param %in% c('(Intercept)')),], 
             aes(y = param, x = coefficients.Estimate, col = trt, shape = sig),
             position = position_dodge(width = .5), size = 2) +
  geom_linerange(data = results[!(results$param %in% c('(Intercept)')),], 
                 aes(y = param, xmin = (coefficients.Estimate - 2*coefficients.Std..Error), 
                     xmax = (coefficients.Estimate + 2*coefficients.Std..Error), col = trt),
                 position = position_dodge(width = .5)) +
  theme_classic() +
  facet_wrap(~nutrient) +
  geom_vline(xintercept = 0) +
  coord_cartesian(xlim = c(-0.5,.5)) +
  scale_shape_manual(values=c(1,16)) +
  scale_color_manual(values = c('black', 'green3')) +
  ylab('') + xlab('Slope Estimates') +
  labs(color='', shape = '') +
  guides(shape = "none") +
  theme(legend.position = "bottom")

##############################################
##############################################
############ Interaction Figures #############
##############################################
##############################################
library(interactions)

#2023 Model Interactions
NH4.interaction23 <- sim_slopes(NH4.mod2023, pred = scalelogviralload, modx = scaleHR, 
                   johnson_neyman = TRUE,
                   modx.values = c(-1.130387585, -1.065011194, -0.293319730, -0.009168597, 0.831330571,
                                   1.666556535))
SRP.interaction23 <- sim_slopes(SRP.mod2023, pred = scalelogviralload, modx = scaleHR, 
                              johnson_neyman = TRUE,
                              modx.values = c(-1.130387585, -1.065011194, -0.293319730, -0.009168597, 0.831330571,
                                              1.666556535))
NP.interaction23 <- sim_slopes(NP.mod2023, pred = scalelogviralload, modx = scaleHR, 
                              johnson_neyman = TRUE,
                              modx.values = c(-1.130387585, -1.065011194, -0.293319730, -0.009168597, 0.831330571,
                                             1.666556535))
NH4.interaction23.plot <- plot(NH4.interaction23, colors = 'forestgreen') + ggtitle('NH4: No NaCl')
SRP.interaction23.plot <- plot(SRP.interaction23, colors = 'forestgreen') + ggtitle('SRP: No NaCl')
NP.interaction23.plot <- plot(NP.interaction23, colors = 'forestgreen') + ggtitle('NP: No NaCl')

ggpubr::ggarrange(NH4.interaction23.plot,
                  SRP.interaction23.plot,
                  NP.interaction23.plot,
                  nrow = 1)

#2024 Model Interactions
NH4.interaction24.VHR <- sim_slopes(NH4.mod2024, pred = scalelogviralload, modx = scaleHR, 
                                johnson_neyman = TRUE,
                                modx.values = c(-0.9748989, -0.7664222, 0.2070907, 1.6141350))
SRP.interaction24.VHR <- sim_slopes(SRP.mod2024, pred = scalelogviralload, modx = scaleHR, 
                                johnson_neyman = TRUE,
                                modx.values = c(-0.9748989, -0.7664222, 0.2070907, 1.6141350))
NP.interaction24.VHR <- sim_slopes(NP.mod2024, pred = scalelogviralload, modx = scaleHR, 
                               johnson_neyman = TRUE,
                               modx.values = c(-0.9748989, -0.7664222, 0.2070907, 1.6141350))

NH4.interaction24.plot.VHR <- plot(NH4.interaction24.VHR, colors = 'darkgoldenrod3') + ggtitle('NH4: NaCl')
SRP.interaction24.plot.VHR <- plot(SRP.interaction24.VHR, colors = 'darkgoldenrod3') + ggtitle('SRP: NaCl')
NP.interaction24.plot.VHR <- plot(NP.interaction24.VHR, colors = 'darkgoldenrod3') + ggtitle('NP: NaCl')

ggpubr::ggarrange(NH4.interaction24.plot.VHR,
                  SRP.interaction24.plot.VHR,
                  NP.interaction24.plot.VHR,
                  nrow = 1)

NH4.interaction24.VN <- sim_slopes(NH4.mod2024, pred = scalelogviralload, modx = scaleNaCl, 
                                    johnson_neyman = TRUE,
                                    modx.values = c(-1.4126413, -0.7063207,  0.0000000, 0.7063207, 1.4126413))
SRP.interaction24.VN <- sim_slopes(SRP.mod2024, pred = scalelogviralload, modx = scaleNaCl, 
                                    johnson_neyman = TRUE,
                                    modx.values = c(-1.4126413, -0.7063207,  0.0000000, 0.7063207, 1.4126413))
NP.interaction24.VN <- sim_slopes(NP.mod2024, pred = scalelogviralload, modx = scaleNaCl, 
                                   johnson_neyman = TRUE,
                                   modx.values = c(-1.4126413, -0.7063207,  0.0000000, 0.7063207, 1.4126413))

NH4.interaction24.plot.VN <- plot(NH4.interaction24.VN, colors = 'darkgoldenrod3') + ggtitle('NH4: NaCl')
SRP.interaction24.plot.VN <- plot(SRP.interaction24.VN, colors = 'darkgoldenrod3') + ggtitle('SRP: NaCl')
NP.interaction24.plot.VN <- plot(NP.interaction24.VN, colors = 'darkgoldenrod3') + ggtitle('NP: NaCl')

ggpubr::ggarrange(NH4.interaction24.plot.VN,
                  SRP.interaction24.plot.VN,
                  NP.interaction24.plot.VN,
                  nrow = 1)

threewayprediction.NH4 <- ggeffects::ggpredict(NH4.mod2024, terms = c("scalelogviralload", "scaleHR", "scaleNaCl [minmax]"))
threewayprediction.SRP <- ggeffects::ggpredict(SRP.mod2024, terms = c("scalelogviralload", "scaleHR", "scaleNaCl [minmax]"))
threewayprediction.NP <- ggeffects::ggpredict(NP.mod2024, terms = c("scalelogviralload", "scaleHR", "scaleNaCl [minmax]"))

facet.labels <- c(
  '-1.41'="No NaCl",
  '1.41'="1000mg/L NaCl"
)

threewayprediction.NH4.plot <- ggplot(threewayprediction.NH4, aes(x = x, y = predicted, group = group, color = as.numeric(group))) +
  geom_line(lwd = 1.25) +
  facet_wrap(~facet, labeller = as_labeller(facet.labels)) + # Facet by the third variable
  labs(
    x = "Scaled Log Viral Load",
    y = "Predicted NH4 Excretion Rate",
    color = "Scaled Hazard Ratio") +
  theme_classic() +
  scale_color_gradientn(#colours = rainbow(4),
                        colours = c("blue", "green", "yellow", "red"),
                        labels = c("Tolerant", "", "", "Susceptible"))

threewayprediction.SRP.plot <- ggplot(threewayprediction.SRP, aes(x = x, y = predicted, group = group, color = as.numeric(group))) +
  geom_line(lwd = 1.25) +
  facet_wrap(~facet, labeller = as_labeller(facet.labels)) + # Facet by the third variable
  labs(
    x = "Scaled Log Viral Load",
    y = "Predicted SRP Excretion Rate",
    color = "Scaled Hazard Ratio") +
  theme_classic() +
  theme(legend.position="none") +
  scale_color_gradientn(#colours = rainbow(4),
    colours = c("blue", "green", "yellow", "red"),
    labels = c("Tolerant", "", "", "Susceptible"))

threewayprediction.NP.plot <- ggplot(threewayprediction.NP, aes(x = x, y = predicted, group = group, color = as.numeric(group))) +
  geom_line(lwd = 1.25) +
  facet_wrap(~facet, labeller = as_labeller(facet.labels)) + # Facet by the third variable
  labs(
    x = "Scaled Log Viral Load",
    y = "Predicted NP Ratio",
    color = "Scaled Hazard Ratio") +
  theme_classic() +
  theme(legend.position="none") +
  scale_color_gradientn(#colours = rainbow(4),
    colours = c("blue", "green", "yellow", "red"),
    labels = c("Tolerant", "", "", "Susceptible"))

ggpubr::ggarrange(threewayprediction.NH4.plot, 
                  threewayprediction.SRP.plot,
                  threewayprediction.NP.plot,
                  nrow = 1, common.legend = TRUE, legend = 'bottom')

##############################################################
##############################################################
############## 2024 Field Nutrient Data ######################
##############################################################
##############################################################

# #Log transform Nutrient Data
# field2024$TN <- log(field2024$TN)
# field2024$TP <- log(field2024$TP)
# field2024$NP <- log(field2024$NP)
# 
# 
# #SRP - No Relationship
# field.SRP <- lm(TP~viral_load * mean.chloride, data = field2024)
# summary(field.SRP)
# 
# #Model Fits
# field.SRP.pred <- data.frame(predict(field.SRP, 
#                           newdata = data.frame(
#                           'viral_load' = field2024$viral_load,
#                            'mean.chloride' = field2024$mean.chloride),
#                           interval = 'confidence'))
# field.SRP.pred$logviral <- field2024$viral_load
# field.SRP.pred$name <- field2024$mean.chloride
# 
# #SRP Plot
# ggplot() +
#   geom_point(data = field2024, aes(x = viral_load, y = TP)) +
#   theme_classic() +
#   geom_line(data = field.SRP.pred, aes(x = logviral, y = fit))
# 
# 
# #NH4 - No Relationships
# field.NH4 <- lm(TN~viral_load * mean.chloride, data = field2024)
# summary(field.NH4)
# 
# #Model Fits
# field.NH4.pred <- data.frame(predict(field.NH4, 
#                                      newdata = data.frame(
#                                        'viral_load' = field2024$viral_load,
#                                        'mean.chloride' = field2024$mean.chloride),
#                                      interval = 'confidence'))
# field.NH4.pred$logviral <- field2024$viral_load
# field.NH4.pred$name <- field2024$mean.chloride
# 
# #Model Fit Plot
# ggplot() +
#   geom_point(data = field2024, aes(x = viral_load, y = TN)) +
#   theme_classic() +
#   geom_line(data = field.NH4.pred, aes(x = logviral, y = fit))
# 
# 
# #N:P Ratio - No Relationships
# field.NP <- lm(NP~viral_load * mean.chloride, data = field2024)
# summary(field.NP)
# 
# field.NP.pred <- data.frame(predict(field.NP, 
#                                      newdata = data.frame(
#                                        'viral_load' = field2024$viral_load,
#                                        'mean.chloride' = field2024$mean.chloride),
#                                      interval = 'confidence'))
# field.NP.pred$logviral <- field2024$viral_load
# field.NP.pred$name <- field2024$mean.chloride
# 
# #Model Fit Plot
# ggplot() +
#   geom_point(data = field2024, aes(x = viral_load, y = NP)) +
#   theme_classic() +
#   geom_line(data = field.NP.pred, aes(x = logviral, y = fit))
# 
# #### Model Checks #####
# shapiro.test(field.NH4$residuals)
# shapiro.test(field.SRP$residuals)
# shapiro.test(field.NP$residuals)
# plot(field.NH4)
# plot(field.SRP)
# plot(field.NP)
# 
# #Combine Model Fits
# field24.preds <- rbind(field.SRP.pred, field.NH4.pred,field.NP.pred)


##############################################
##############################################
#### Linear Model Viral Load Exps. Figure ####
##############################################
##############################################
#nutrient.labs <- c('Total N', 'Total P', 'N:P Ratio')
#names(nutrient.labs) <- c('NH4', 'SRP', 'NP')

excretion2024$salt_YN <- 'Y'
excretion2024[excretion2024$nacl %in% c(0),'salt_YN'] <- 'N'

excretion2023$salt_YN <- 'N'

pred24.0$nacl <- as.factor(0)
pred24$nacl <- as.factor(pred24$nacl)

ex24.plot <- ggplot() +
  xlab('Scaled Log Viral Load') + theme_classic() + ylab('Scaled Nutrient Excretion Rate') +
  facet_wrap(susc~nutrient, scales = "free") +
  geom_line(data = pred24, aes(x = scalelogviralload, y = fit, group = scaleNaCl, color = as.factor(scaleNaCl)), lwd = 1.5) +
  geom_ribbon(data = pred24, aes(x = scalelogviralload, ymax = upr, ymin = lwr, group = as.factor(scaleNaCl), fill = as.factor(scaleNaCl)), 
              color = NA, alpha = .2) +
  geom_point(data = excretion2024,#[which(excretion2024$nacl > 0),], 
             aes(x = scalelogviralload, y = value, color = as.factor(scaleNaCl),
                 shape = salt_YN)) +
  theme(strip.text.y = element_blank()) +
  #geom_line(data = pred24.0, aes(x = viralload, y = fit, color = nacl), linetype = 'dashed', alpha = .6, lwd = 1.5) +
  #geom_ribbon(data = pred24.0, aes(x = viralload, ymax = upr, ymin = lwr, fill = nacl), 
   #           color = NA, alpha = .1) +
  facet_grid(scaleHR~nutrient, scales = "free") + #labeller = labeller(nutrient = nutrient.labs)) +
  scale_color_manual(labels = c("-1.41264134002781" = '0', "-0.706320670013903" = '0.25', 
                                "0" = '0.5', "0.706320670013903" = '0.75', 
                                "1.41264134002781" = '1'), 
                     values = c('black','#440154FF', '#2A788EFF', '#7AD151FF', '#FDE725FF')) +
  scale_fill_manual(values = c('black','#440154FF', '#2A788EFF', '#7AD151FF', '#FDE725FF')) +
  #scale_color_viridis_d(aesthetics = c("colour", "fill")) + 
  scale_shape_manual(values = c(1,16), labels = c('No NaCl', 'NaCl')) +
  guides(color=guide_legend("Scaled NaCl"), fill = 'none');ex24.plot
  #theme(strip.background = element_blank(),
   # strip.text.x = element_blank(),
    #legend.title=element_blank())

ex23.plot <- ggplot() +
  geom_point(data = excretion2023, aes(x = scalelogviralload, y = value, color = nutrient, shape = FV3)) +
  xlab('Scaled Log Viral Load') + theme_classic() + ylab('Scaled Nutrient Excretion Rates') +
  #geom_line(data = pred23, aes(x = scalelogviralload, y = fit, color = nutrient), lwd = 1.5, alpha = .6, linetype = 'dashed') +
  #geom_ribbon(data = pred23, aes(x = scalelogviralload, ymax = upr, ymin = lwr, fill = nutrient), 
   #           color = NA, alpha = .1) +
  facet_wrap(~nutrient, scales = "free") + #labeller = labeller(nutrient = nutrient.labs)) +
  scale_color_manual(labels = c("Total N", "Total P", "N:P Ratio"),values = c('black', 'black', 'black')) +
  guides(color= 'none', fill = 'none', shape = 'none') +
  scale_shape_manual(values = c(1, 16), labels = c('No NaCl', "NaCl")) +
  scale_fill_manual(values = c('black', 'black', 'black')) +
  theme(legend.title=element_blank());ex23.plot

#Figure 2
ggarrange(ex23.plot,ex24.plot, 
          ncol = 1, common.legend = FALSE,
          legend = 'bottom',
          labels = c('A', 'B'),
          heights = c(.9,1))


##############################################
##############################################
######## Salt and Viral Load Figure ##########
##############################################
##############################################

#Figure 3
nacl.plot <- ggplot() +
  geom_point(data = nacl, aes(x = chloride, y = plaques)) +
  geom_line(data = plaque.pred, aes(x = seq(0,1900,by = 100), y = fit), lwd = 1.5, col = 'darkorange') +
  theme_classic() +
  geom_ribbon(data = plaque.pred, aes(x = seq(0,1900,by = 100), ymin = (fit - (2*se.fit)), ymax = (fit + (2*se.fit))),
              col = NA, fill = 'darkorange', alpha = .2) +
  xlab('') +
  ylab('Number of Plaques') +
  xlab('Chloride Concetration (mg/L)') +
  ylim(c(0,25));nacl.plot


viralex.plot <- ggplot() +
  geom_point(data = excretion2024, aes(x = chloride, y = logviralload)) +
  #geom_line(data = VL.pred24, aes(x = chloride, y = fit), color = 'darkorange', lwd = 1.5) +
  #geom_ribbon(data = VL.pred24, aes(x = chloride, ymax = upr, ymin = lwr), fill = 'darkorange', 
   #           color = NA, alpha = .2) +
  ylim(c(0,7.5)) +
  ylab('Log Viral Load') + theme_classic() + xlab('Chloride (mg/L)');viralex.plot


field.viralload <- ggplot() +
  geom_point(data = field2024, aes(y = viral_load, x = mean.chloride), position = position_dodge2(width = 1)) +
  guides(shape = 'none') +
  xlab('Chloride (mg/L)') +
  ylab('') +
  theme_classic();field.viralload

#Figure 
ggarrange(viralex.plot, field.viralload, nrow = 1,
          labels = c('A', 'B'))



##############################################
##############################################
######## Field 2024 Nutrient Figure ##########
##############################################
##############################################

field2024.long <- field2024

field2024.long <- field2024.long[,c(2:6)] %>%
  pivot_longer(cols = TN:NP,
               values_to = "value")

field2024.long[field2024.long$name %in% c('TN'),'name'] <- "Total N"
field2024.long[field2024.long$name %in% c('TP'),'name'] <- "Total P"
field2024.long[field2024.long$name %in% c('NP'),'name'] <- "N:P Ratio"

field24.nutrients.plot <- ggplot() +
  geom_point(data = field2024.long, aes(x = viral_load, y = value)) +
  theme_classic() +
  #scale_color_manual(values = c('#006600', '#660000','#000099')) +
  #scale_fill_manual(values = c('#006600', '#660000','#000099')) +
  #scale_fill_manual(values = c('#000099')) +
  facet_wrap(~factor(name, c('Total N', 'Total P', 'N:P Ratio')), scales = 'free') +
  xlab('Log Viral Load') + ylab('Nutrient') +
  #guides(color=guide_legend(""), fill = 'none', shape = 'none') +
  theme(legend.position = 'none');field24.nutrients.plot

field24.nutrient.plot.chloride <- ggplot() +
  geom_point(data = field2024.long, aes(x = mean.chloride, y = value)) +
  theme_classic() +
  #scale_color_manual(values = c('#006600', '#660000','#000099')) +
  facet_wrap(~factor(name, c('Total N', 'Total P', 'N:P Ratio')), scales = 'free') +
  xlab('Chloride (mg/L)') + ylab('Nutrient') +
  guides(color=guide_legend(""), fill = 'none', shape = 'none') +
  theme(legend.position = 'bottom') +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank());field24.nutrient.plot.chloride

#Figure 5
ggarrange(field24.nutrients.plot, field24.nutrient.plot.chloride, 
          ncol = 1,
          labels = c('A', 'B'))

