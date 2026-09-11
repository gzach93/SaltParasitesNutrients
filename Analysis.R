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
library(ggnewscale)
library(gg.gap)
library(lme4)
library(lmerTest)
library(merTools)

#Laod in Data
#Excretion 2023 Data -- FV3 and Nutrients but no chloride
excretion2023 <- read.csv('Data/Excretion2023.csv')
excretion2023[which(excretion2023$meanCQ > 35), 'viralload'] <- 0

#Excretion 2024 Data -- FV3, NaCl, and Nutrients
excretion2024 <- read.csv('Data/Excretion2024.csv')
#Reorder Excretion 2024 Nutrients
excretion2024$nutrient <- factor(excretion2024$nutrient, levels = c('NH4', 'SRP', 'NP'))

#### Base Nutrient Plots ####
#############################################################
#Version 1
NH4_23_plot <- ggplot() + geom_violin(data = excretion2023[excretion2023$nutrient %in% c('NH4'),], 
                       aes(y = value, x = nutrient)) +
  ylab(expression(atop(paste("NH"^4), paste(mu, mol ~ hr^-1 ~ g~of~tadpole^-1)))) +
  xlab('') + scale_color_viridis_c(option = "plasma") +
  geom_sina(data = excretion2023[excretion2023$nutrient %in% c('NH4'),], aes(y = value, x = nutrient, col = log(viralload + 1))) +
  theme_classic();NH4_23_plot

SRP_23_plot <- ggplot() + geom_violin(data = excretion2023[excretion2023$nutrient %in% c('SRP'),], 
                       aes(y = value, x = nutrient)) +
  ylab(expression(atop(paste("SRP"), paste(mu, mol ~ hr^-1 ~ g~of~tadpole^-1)))) +
  xlab('') + scale_color_viridis_c(option = "plasma") +
  geom_sina(data = excretion2023[excretion2023$nutrient %in% c('SRP'),], aes(y = value, x = nutrient, col = log(viralload + 1))) +
  theme_classic();SRP_23_plot

NP_23_plot <- ggplot() + geom_violin(data = excretion2023[excretion2023$nutrient %in% c('NP'),], 
                       aes(y = value, x = nutrient)) +
  ylab("N:P Ratio") +
  xlab('') + scale_color_viridis_c(option = "plasma") +
  geom_sina(data = excretion2023[excretion2023$nutrient %in% c('NP'),], aes(y = value, x = nutrient, col = log(viralload + 1))) +
  theme_classic();NP_23_plot

ggarrange(NH4_23_plot, SRP_23_plot, NP_23_plot,
          nrow = 1, common.legend = T,
          legend = 'bottom')

NH4_24_plot <- ggplot() + geom_violin(data = excretion2024[excretion2024$nutrient %in% c('NH4'),], 
                                      aes(y = value, x = nacl, group = nacl)) +
  ylab(expression(atop(paste("NH"^4), paste(mu, mol ~ hr^-1 ~ g~of~tadpole^-1)))) +
  xlab('') + scale_color_viridis_c(option = "plasma") +
  geom_sina(data = excretion2024[excretion2024$nutrient %in% c('NH4'),], aes(y = value, x = nacl, group = nacl,
                                                                             col = log(viralload + 1))) +
  theme_classic();NH4_24_plot

SRP_24_plot <- ggplot() + geom_violin(data = excretion2024[excretion2024$nutrient %in% c('SRP'),], 
                                      aes(y = value, x = nacl, group = nacl)) +
  ylab(expression(atop(paste("SRP"), paste(mu, mol ~ hr^-1 ~ g~of~tadpole^-1)))) +
  xlab('') + scale_color_viridis_c(option = "plasma") +
  geom_sina(data = excretion2024[excretion2024$nutrient %in% c('SRP'),], aes(y = value, x = nacl, group = nacl, col = log(viralload + 1))) +
  theme_classic();SRP_24_plot

NP_24_plot <- ggplot() + geom_violin(data = excretion2024[excretion2024$nutrient %in% c('NP'),], 
                                     aes(y = value, x = nacl, group = nacl)) +
  ylab("N:P Ratio") +
  xlab('') + scale_color_viridis_c(option = "plasma") +
  geom_sina(data = excretion2024[excretion2024$nutrient %in% c('NP'),], aes(y = value, x = nacl, group = nacl, col = log(viralload + 1))) +
  theme_classic();NP_24_plot

ggarrange(NH4_24_plot, SRP_24_plot, NP_24_plot,
          nrow = 1, common.legend = T,
          legend = 'bottom')
#############################################################
excretion2023$nacl <- 0
excretion2023$year <- 2023
excretion2024$year <- 2024

all.excretion <- rbind(excretion2024[,c('value', 'nacl', 'nutrient', 'viralload', 'year')], 
                       excretion2023[,c('nutrient', 'value', 'viralload', 'nacl', 'year')])

NH4_all_plot <- ggplot() + geom_violin(data = all.excretion[all.excretion$nutrient %in% c('NH4'),], 
                                      aes(y = value, x = nacl, group = interaction(year,nacl), color = as.factor(year)), alpha = .3, lwd = 1.5) +
  ylab(expression(atop(paste("NH"[4]), paste(mu, mol ~ hr^-1 ~ g~of~tadpole^-1)))) +
  scale_color_manual(values = c('#2C7FB8', '#D95F0E'), name = 'Year') +
  xlab('') + 
  ggnewscale::new_scale_color() +
  geom_sina(data = all.excretion[all.excretion$nutrient %in% c('NH4'),], aes(y = value, x = nacl, group = interaction(year,nacl),
                                                                             col = log(viralload + 1))) +
  scale_color_viridis_c(option = "viridis", name = 'Log Viral Load') + scale_x_continuous(breaks = sort(unique(all.excretion$nacl))) +
  theme_classic();NH4_all_plot

SRP_all_plot <- ggplot() + geom_violin(data = all.excretion[all.excretion$nutrient %in% c('SRP') & all.excretion$value < 0.75,], 
                                       aes(y = value, x = nacl, group = interaction(year,nacl), color = as.factor(year)), alpha = .3, lwd = 1.5) +
  ylab(expression(atop(paste("SRP"), paste(mu, mol ~ hr^-1 ~ g~of~tadpole^-1)))) +
  scale_color_manual(values = c('#2C7FB8', '#D95F0E'), name = 'Year') +
  xlab(expression(atop(paste("NaCl Concentration"), paste(mg ~ L^-1)))) +
  ggnewscale::new_scale_color() +
  geom_sina(data = all.excretion[all.excretion$nutrient %in% c('SRP') & all.excretion$value < 0.75,], aes(y = value, x = nacl, group = interaction(year,nacl),
                                                                             col = log(viralload + 1))) +
  scale_color_viridis_c(option = "viridis", name = 'Log Viral Load') + scale_x_continuous(breaks = sort(unique(all.excretion$nacl))) +
  theme_classic();SRP_all_plot

NP_all_plot <- ggplot() + geom_violin(data = all.excretion[all.excretion$nutrient %in% c('NP') & all.excretion$value < 40,], 
                                       aes(y = value, x = nacl, group = interaction(year,nacl), color = as.factor(year)), alpha = .3, lwd = 1.5) +
  ylab('N:P Ratio') +
  scale_color_manual(values = c('#2C7FB8', '#D95F0E'), name = 'Year') +
  xlab('') + 
  ggnewscale::new_scale_color() +
  geom_sina(data = all.excretion[all.excretion$nutrient %in% c('NP') & all.excretion$value < 40,], aes(y = value, x = nacl, group = interaction(year,nacl),
                                                                             col = log(viralload + 1))) +
  scale_color_viridis_c(option = "viridis", name = 'Log Viral Load') + scale_x_continuous(breaks = sort(unique(all.excretion$nacl))) +
  theme_classic();NP_all_plot


ggarrange(NH4_all_plot, SRP_all_plot, NP_all_plot,
          nrow = 1, common.legend = T,
          legend = 'bottom', align = 'h')

#ggsave("Figure1_Data.png",  bg = "white", width = 8, height = 4)


summary(excretion2023[excretion2023$nutrient %in% c('NH4'),'value'])
summary(excretion2024[excretion2024$nutrient %in% c('NH4'),'value'])

summary(excretion2023[excretion2023$nutrient %in% c('SRP'),'value'])
summary(excretion2024[excretion2024$nutrient %in% c('SRP'),'value'])

summary(excretion2023[excretion2023$nutrient %in% c('NP'),'value'])
summary(excretion2024[excretion2024$nutrient %in% c('NP'),'value'])

#########################################

ggplot() + geom_point(data = excretion2024, aes(y = mass_g, x = stage)) +
  theme_classic() + ylab('Mass (g)') + xlab('Gosner Stage')


ggplot() + geom_point(data = excretion2024[excretion2024$nutrient %in% c('NH4'),], 
                      aes(y = value, x = stage)) +
  theme_classic() + ylab(expression(atop(paste("NH"[4]), paste(mu, mol ~ hr^-1 ~ g~of~tadpole^-1)))) + 
  xlab('Gosner Stage')
NH4.dev.mod <- lm(value ~ stage, data = excretion2024[excretion2024$nutrient %in% c('NH4'),])
summary(NH4.dev.mod)

ggplot() + geom_point(data = excretion2024[excretion2024$nutrient %in% c('SRP'),], 
                      aes(y = value, x = stage)) + coord_cartesian(ylim =c(0,1)) +
  theme_classic() + ylab(expression(atop(paste("SRP"), paste(mu, mol ~ hr^-1 ~ g~of~tadpole^-1)))) + 
  xlab('Gosner Stage')
SRP.dev.mod <- lm(value ~ stage, data = excretion2024[excretion2024$nutrient %in% c('SRP'),])
summary(SRP.dev.mod)

ggplot() + geom_point(data = excretion2024[excretion2024$nutrient %in% c('NP'),], 
                      aes(y = value, x = stage)) +
  theme_classic() + ylab('N:P Ratio') + 
  xlab('Gosner Stage')
NP.dev.mod <- lm(value ~ stage, data = excretion2024[excretion2024$nutrient %in% c('NP'),])
summary(NP.dev.mod)

####################################################
####################################################
###### Excretion without Virus or NaCl Free ########
####################################################
####################################################
ttd2023 <- excretion2023 %>% group_by(Population) %>%
  reframe(mean.ttd = mean(TTD_Hr),
            HazardRatio = unique(HazardRatio))

ttd2024 <- excretion2024 %>% group_by(pop) %>%
  summarise(HazardRatio = unique(HazardRatio))

control23 <- excretion2023[which(excretion2023$meanCQ > 35),]

control23$scaleHR <- as.vector(scale(control23$HazardRatio, scale = TRUE))
control23$scalestage <- as.vector(scale(control23$Stage, scale = TRUE))

excretion2024$scaleHR <- as.vector(scale(excretion2024$HazardRatio, scale = TRUE))
excretion2024$scalestage <- as.vector(scale(excretion2024$stage, scale = TRUE))

salt.control24 <- excretion2024[which(excretion2024$FV3 == 'CTRL'),]

#Tadpole Excretion without Salt or FV3
salt.control24 <- salt.control24[salt.control24$nacl %in% c(0),] 

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




salt.control.mod.SRP <- lm(scalenutrient ~ scaleHR + scalestage, data = salt.control24[salt.control24$nutrient %in% c('SRP'),])
salt.control.mod.NH4 <- lm(scalenutrient ~ scaleHR  + scalestage, data = salt.control24[salt.control24$nutrient %in% c('NH4'),])
salt.control.mod.NP <- lm(scalenutrient ~ scaleHR + scalestage, data = salt.control24[salt.control24$nutrient %in% c('NP'),])

summary(salt.control.mod.SRP)
summary(salt.control.mod.NH4)
summary(salt.control.mod.NP)


salt.control.mod.SRP.23 <- lm(scalenutrient ~ scaleHR + scalestage, data = control23[control23$nutrient %in% c('SRP'),])
salt.control.mod.NH4.23 <- lm(scalenutrient ~ scaleHR + scalestage, data = control23[control23$nutrient %in% c('NH4'),])
salt.control.mod.NP.23 <- lm(scalenutrient ~ scaleHR + scalestage, data = control23[control23$nutrient %in% c('NP'),])

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

excretion2023$scaleStage <- as.vector(scale(excretion2023$Stage, scale = TRUE))

#NH4 - Not Significant
NH4.mod2023 <- lm(scalenutrient ~ scalelogviralload*scaleHR + scaleStage, 
                  data = excretion2023[excretion2023$nutrient %in% c('NH4'),])# & excretion2023$Population %in% c('TRL', 'STB', 'SQR', 'RR'),])
summary(NH4.mod2023)
vif(NH4.mod2023, type = 'predictor')

#SRP - Not Significant
SRP.mod2023 <- lm(scalenutrient  ~ scalelogviralload*scaleHR + scaleStage, data = excretion2023[excretion2023$nutrient %in% c('SRP'),])# & excretion2023$Population %in% c('SEW', 'MIN', 'SQR', 'RR'),])
summary(SRP.mod2023) 
vif(SRP.mod2023, type = 'predictor')

#N:P Ratio - Not Significant
NP.mod2023 <- lm(scalenutrient ~ scalelogviralload*scaleHR + scaleStage, data = excretion2023[excretion2023$nutrient %in% c('NP'),])# & excretion2023$Population %in% c('SEW', 'MIN', 'SQR', 'RR'),])
summary(NP.mod2023)
vif(NP.mod2023, type = 'predictor')

#Create Model Fit
no.salt.data23 <- data.frame(expand_grid('scalelogviralload' = seq(-3,2,by = .25), 
                                         'scaleStage' = 0.2740071,
                                         'scaleHR' = c(-1.13038758506352, -1.06501119382489, -0.293319730368694, -0.00916859701075909,  0.831330570814018,  1.66655653545385)))
NH4.pred <- cbind(no.salt.data23, data.frame(predict(NH4.mod2023, newdata = no.salt.data23, interval = "confidence")))
SRP.pred <- cbind(no.salt.data23, data.frame(predict(SRP.mod2023, newdata = no.salt.data23, interval = "confidence")))
NP.pred <- cbind(no.salt.data23, data.frame(predict(NP.mod2023, newdata = no.salt.data23, interval = "confidence")))
NH4.pred$nutrient <- 'NH4'
SRP.pred$nutrient <- 'SRP'
NP.pred$nutrient <- 'NP'
pred23 <- rbind(NH4.pred, SRP.pred, NP.pred)
#pred23$logviralload <- seq(1,3,by = .25)
excretion2023$nutrient <- factor(excretion2023$nutrient, levels = c('NH4', 'SRP', 'NP'))
pred23$nutrient <- factor(pred23$nutrient, levels = c('NH4', 'SRP', 'NP'))

#2023 Excretion Assay Plot
ggplot() +
  geom_point(data = excretion2023, aes(x = scalelogviralload, y = scalenutrient, col = scaleHR)) +
  xlab('Scaled Log Viral Load') + theme_classic() + 
  ylab(expression(paste("Nutrient Excretion Rate (", mu, mol ~ hr^-1 ~ g~of~tadpole^-1, ")"))) + 
  facet_wrap(~nutrient, scales = "free") +
  geom_line(data = pred23, aes(x = scalelogviralload, y = fit, col = scaleHR, group = scaleHR), lwd = 1.5) +
  geom_ribbon(data = pred23, aes(x = scalelogviralload, ymax = upr, ymin = lwr, fill = scaleHR, group = scaleHR), 
              color = NA, alpha = .2) #+
  #ggtitle("Chloride Free") 


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
excretion2024$scalestage <- as.vector(scale(excretion2024$stage, scale = TRUE))

##### NH4 #####
#NH4 Salt Trt 
NH4.mod2024 <- lm(scalenutrient ~ scalelogviralload * scaleNaCl * scaleHR + scalestage, data = excretion2024[excretion2024$nutrient %in% c('NH4'),])# & excretion2024$nacl > 0,])
summary(NH4.mod2024)
vif(NH4.mod2024, type = 'predictor')

#### SRP ####
#SRP Salt Trt 
SRP.mod2024 <- lm(scalenutrient ~ scalelogviralload * scaleNaCl * scaleHR + scalestage, data = excretion2024[excretion2024$nutrient %in% c('SRP'),])# & excretion2024$nacl > 0,]) #& excretion2024$nacl %in% c(0.25, 0.50, 0.75, 1),])
summary(SRP.mod2024)
vif(SRP.mod2024, type = 'predictor')

#### NP Ratio ####
#NP Ratio - Increase with NaCl and Viral Load - Interaction Decrease - All Significant
NP.mod2024 <- lm(scalenutrient ~ scalelogviralload * scaleNaCl * scaleHR + scalestage, data = excretion2024[excretion2024$nutrient %in% c('NP'),])# & excretion2024$nacl > 0,]) #& excretion2024$nacl %in% c(0.25, 0.50, 0.75, 1),])
summary(NP.mod2024)
vif(NP.mod2024, type = 'predictor')

#Model Fits - Salt Trts
newdata.nacl <- data.frame(expand.grid('scalelogviralload' = seq(-2,2.5,by = .25), 
                                       'scalestage' = 0.2740071,
                                       'scaleNaCl' = unique(excretion2024$scaleNaCl), 
                                       'scaleHR' = unique(excretion2024$scaleHR)))
NH4.pred24 <- cbind(newdata.nacl, data.frame(predict(NH4.mod2024, newdata = newdata.nacl, interval = 'confidence')))
SRP.pred24 <- cbind(newdata.nacl, data.frame(predict(SRP.mod2024, newdata = newdata.nacl, interval = 'confidence')))
NP.pred24 <- cbind(newdata.nacl, data.frame(predict(NP.mod2024, newdata = newdata.nacl, interval = 'confidence')))
NH4.pred24$nutrient <- 'NH4'
SRP.pred24$nutrient <- 'SRP'
NP.pred24$nutrient <- 'NP'
pred24 <- rbind(NH4.pred24, SRP.pred24, NP.pred24)
#pred24$viralload <- seq(0,16,by = .25)
#pred24$nutrient <- factor(pred24$nutrient, levels = c('NH4', 'SRP', 'NP'))
#pred24$nacl <- rep(rep(c(250,500,750,1000), each = 65), times = 3)
#pred24$susc <- rep(rep(c('Susceptible', 'Tolerant'), each = 84), times = 3)

#Salt Trt Plot
ggplot() +
  facet_grid(scaleHR~nutrient, scales = "free") +
  geom_line(data = pred24, aes(x = scalelogviralload, y = fit, color = scaleNaCl, group = interaction(scaleNaCl, scaleHR)), lwd = 1.5) +
  geom_ribbon(data = pred24, aes(x = scalelogviralload, ymax = upr, ymin = lwr, group = interaction(scaleNaCl, scaleHR), fill = scaleNaCl), 
              color = NA, alpha = .2) +
  geom_point(data = excretion2024, aes(x = scalelogviralload, y = scalenutrient, color = scaleNaCl)) +
  xlab('Scaled Log Viral Load') + theme_classic() + ylab('Nutrient Excretion') +
  ggtitle("Chloride Added")


##############################################
##############################################
########## Caterpillar Model Figure ##########
##############################################
##############################################

control.results <- rbind(data.frame(summary(salt.control.mod.NH4)[4]), 
                         data.frame(summary(salt.control.mod.SRP)[4]), 
                         data.frame(summary(salt.control.mod.NP)[4]))
control.results$nutrient <- rep(c('NH4', 'SRP', 'N:P Ratio'), each = 3)
control.results$trt <- 'Control' 
control.results$FV3 <- 'N'
control.results$NaCl <- 'N'


resultsNH4 <- rbind(data.frame(summary(NH4.mod2023)[4]),
                    data.frame(summary(NH4.mod2024)[4]))
resultsNH4$trt <- c(rep('FV3', times = 5), rep('FV3 + NaCl', times = 9))
resultsNH4$nutrient <- 'NH4'
resultsNH4$FV3 <- 'Y'
resultsNH4$NaCl <- c(rep('N', times = 5), rep('Y', times = 9))


resultsSRP <- rbind(data.frame(summary(SRP.mod2023)[4]),
                    data.frame(summary(SRP.mod2024)[4]))
resultsSRP$trt <- c(rep('FV3', times = 5), rep('FV3 + NaCl', times = 9))
resultsSRP$nutrient <- 'SRP'
resultsSRP$FV3 <- 'Y'
resultsSRP$NaCl <- c(rep('N', times = 5), rep('Y', times = 9))

resultsNP <- rbind(data.frame(summary(NP.mod2023)[4]),
                   data.frame(summary(NP.mod2024)[4]))
resultsNP$trt <- c(rep('FV3', times = 5), rep('FV3 + NaCl', times = 9))
resultsNP$nutrient <- 'N:P Ratio'
resultsNP$FV3 <- 'Y'
resultsNP$NaCl <- c(rep('N', times = 5), rep('Y', times = 9))

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
results[results$param %in% c('scalestage', 'scaleStage'),'param'] <- 'Gosner Stage'

results$param <- factor(results$param, levels = c('(Intercept)', 'Hazard Ratio', 'Viral Load', 'Gosner Stage',
                                                  'Hazard Ratio * Viral Load', 'NaCl',
                                                  'Hazard Ratio * NaCl',
                                                  'Viral Load * NaCl',
                                                  'Hazard Ratio * Viral Load * NaCl'))

slope.plot <- ggplot() +
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
  theme(legend.position = "bottom");slope.plot

#ggsave('Figure2_ModelSlopes.png', slope.plot)


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

NH4.plot.data <- excretion2024[excretion2024$nutrient %in% c('NH4') &
                                 excretion2024$nacl %in% c(0, 1000),]
NH4.plot.data[NH4.plot.data$scaleNaCl %in% c(min(unique(NH4.plot.data$scaleNaCl))),'scaleNaCl'] <- "-1.41"
NH4.plot.data[NH4.plot.data$scaleNaCl %in% c(max(unique(NH4.plot.data$scaleNaCl))),'scaleNaCl'] <- "1.41"
NH4.plot.data$scaleHR <- as.factor(NH4.plot.data$scaleHR)


threewayprediction.NH4.plot <- ggplot() +
  geom_point(data = NH4.plot.data,
             aes(x = scalelogviralload, y = scalenutrient, color = as.numeric(scaleHR))) +
  geom_line(data = threewayprediction.NH4, 
            aes(x = x, y = predicted, group = group, color = as.numeric(group)),
            lwd = 1) +
  facet_wrap(~facet, labeller = as_labeller(facet.labels)) + # Facet by the third variable
  labs(
    x = "Scaled Log Viral Load",
    #y = "Scaled Predicted NH4 Excretion Rate",
    color = "Scaled Hazard Ratio") +
  ylab(expression(paste("Scaled Predicted"~ NH^4 ~ "Excretion Rate (", mu, mol ~ hr^-1 ~ g~of~tadpole^-1, ")"))) + 
  theme_classic() +
  scale_color_gradientn(#colours = rainbow(4),
    colours = c("blue", "green", "orange", "red"),
    labels = c("Tolerant", "", "", "Susceptible"));threewayprediction.NH4.plot

colnames(threewayprediction.SRP)[7] <- "scaleNaCl"

SRP.plot.data <- excretion2024[excretion2024$nutrient %in% c('SRP') &
                                 excretion2024$nacl %in% c(0, 1000),]
SRP.plot.data[SRP.plot.data$scaleNaCl %in% c(min(unique(SRP.plot.data$scaleNaCl))),'scaleNaCl'] <- "-1.41"
SRP.plot.data[SRP.plot.data$scaleNaCl %in% c(max(unique(SRP.plot.data$scaleNaCl))),'scaleNaCl'] <- "1.41"
SRP.plot.data$scaleHR <- as.factor(SRP.plot.data$scaleHR)

threewayprediction.SRP.plot <- 
  ggplot() +
  geom_point(data = SRP.plot.data,
             aes(x = scalelogviralload, y = scalenutrient, color = as.numeric(scaleHR))) +
  geom_line(data = threewayprediction.SRP, aes(x = x, y = predicted, group = group, color = as.numeric(group)),
            lwd = 1) +
  facet_wrap(~scaleNaCl, labeller = as_labeller(facet.labels)) + # Facet by the third variable
  labs(
    x = "Scaled Log Viral Load",
    color = "Scaled Hazard Ratio") +
  ylab(expression(paste("Scaled Predicted SRP Excretion Rate (", mu, mol ~ hr^-1 ~ g~of~tadpole^-1, ")"))) + 
  theme_classic() +
  theme(legend.position="none") +
  scale_color_gradientn(#colours = rainbow(4),
    colours = c("blue", "green", "orange", "red"),
    labels = c("Tolerant", "", "", "Susceptible"));threewayprediction.SRP.plot

threewayprediction.NP.plot <- ggplot(threewayprediction.NP, aes(x = , y = predicted, group = group, color = as.numeric(group))) +
  geom_line(lwd = 1.25) +
  facet_wrap(~facet, labeller = as_labeller(facet.labels)) + # Facet by the third variable
  labs(
    x = "Scaled Log Viral Load",
    y = "Predicted NP Ratio",
    color = "Scaled Hazard Ratio") +
  theme_classic() +
  theme(legend.position="none") +
  scale_color_gradientn(#colours = rainbow(4),
    colours = c("blue", "green", "orange", "red"),
    labels = c("Tolerant", "", "", "Susceptible"))

Figure3 <- ggpubr::ggarrange(threewayprediction.NH4.plot, 
                  threewayprediction.SRP.plot,
                  #threewayprediction.NP.plot,
                  nrow = 1, common.legend = TRUE, legend = 'bottom');Figure3

#ggsave('Figure3_ModelFits.png', Figure3)


##############################################
##############################################
#### Linear Model Viral Load Exps. Figure ####
##############################################
##############################################
#nutrient.labs <- c(expression("0 (mg" ~ L^{-1} ~ ")"), 
 #                  expression("250 (mg" ~ L^{-1} * ")"),
  #                 expression("500 (mg" ~ L^{-1} * ")"),
   #                expression("750 (mg" ~ L^{-1} * ")"),
    #               expression("1000 (mg" ~ L^{-1} * ")"))

#names(nutrient.labs) <- c('0', '250', '500', '750', '1000')

#excretion2024$salt_YN <- 'Y'
#excretion2024[excretion2024$nacl %in% c(0),'salt_YN'] <- 'N'

#excretion2023$salt_YN <- 'N'

#pred24$nacl <- as.factor(pred24$nacl)

ex24.plot1 <- ggplot() +
  xlab('Scaled Log Viral Load') + theme_classic() + ylab('Scaled Nutrient Excretion Rate') +
  facet_wrap(susc~nutrient, scales = "free") +
  geom_line(data = pred24, aes(x = scalelogviralload, y = fit, group = scaleNaCl, color = as.factor(scaleNaCl)), lwd = 1.5) +
  geom_ribbon(data = pred24, aes(x = scalelogviralload, ymax = upr, ymin = lwr, group = as.factor(scaleNaCl), fill = as.factor(scaleNaCl)), 
              color = NA, alpha = .2) +
  geom_point(data = excretion2024,#[which(excretion2024$nacl > 0),], 
             aes(x = scalelogviralload, y = scalenutrient, color = as.factor(scaleNaCl))) +
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
  #scale_shape_manual(values = c(1,16), labels = c('No NaCl', 'NaCl')) +
  guides(color=guide_legend("Scaled NaCl"), fill = 'none');ex24.plot1
#theme(strip.background = element_blank(),
# strip.text.x = element_blank(),
#legend.title=element_blank())



pred24$logviralload <- pred24$scalelogviralload * sd(excretion2024$logviralload, na.rm = TRUE)  + mean(excretion2024$logviralload, na.rm = TRUE)
pred24$nacl <- pred24$scaleNaCl * sd(excretion2024$nacl, na.rm = TRUE) + mean(excretion2024$nacl, na.rm = TRUE)
pred24[pred24$nacl == min(pred24$nacl),'nacl'] <- 0 # Fixing rounding error
pred24$HR <- pred24$scaleHR * sd(excretion2024$HazardRatio, na.rm = TRUE) + mean(excretion2024$HazardRatio, na.rm = TRUE)

pred24$nutrient.value <- NA
pred24[pred24$nutrient %in% c('NH4'),'nutrient.value'] <- pred24[pred24$nutrient %in% c('NH4'),'fit'] * sd(excretion2024[excretion2024$nutrient %in% c('NH4'),'value'], na.rm = TRUE) + mean(excretion2024[excretion2024$nutrient %in% c('NH4'),'value'], na.rm = TRUE)
pred24[pred24$nutrient %in% c('SRP'),'nutrient.value'] <- pred24[pred24$nutrient %in% c('SRP'),'fit'] * sd(excretion2024[excretion2024$nutrient %in% c('SRP'),'value'], na.rm = TRUE) + mean(excretion2024[excretion2024$nutrient %in% c('SRP'),'value'], na.rm = TRUE)
pred24[pred24$nutrient %in% c('NP'),'nutrient.value'] <- pred24[pred24$nutrient %in% c('NP'),'fit'] * sd(excretion2024[excretion2024$nutrient %in% c('NP'),'value'], na.rm = TRUE) + mean(excretion2024[excretion2024$nutrient %in% c('NP'),'value'], na.rm = TRUE)

pred24$lwr.value <- NA
pred24[pred24$nutrient %in% c('NH4'),'lwr.value'] <- pred24[pred24$nutrient %in% c('NH4'),'lwr'] * sd(excretion2024[excretion2024$nutrient %in% c('NH4'),'value'], na.rm = TRUE) + mean(excretion2024[excretion2024$nutrient %in% c('NH4'),'value'], na.rm = TRUE)
pred24[pred24$nutrient %in% c('SRP'),'lwr.value'] <- pred24[pred24$nutrient %in% c('SRP'),'lwr'] * sd(excretion2024[excretion2024$nutrient %in% c('SRP'),'value'], na.rm = TRUE) + mean(excretion2024[excretion2024$nutrient %in% c('SRP'),'value'], na.rm = TRUE)
pred24[pred24$nutrient %in% c('NP'),'lwr.value'] <- pred24[pred24$nutrient %in% c('NP'),'lwr'] * sd(excretion2024[excretion2024$nutrient %in% c('NP'),'value'], na.rm = TRUE) + mean(excretion2024[excretion2024$nutrient %in% c('NP'),'value'], na.rm = TRUE)

pred24$upr.value <- NA
pred24[pred24$nutrient %in% c('NH4'),'upr.value'] <- pred24[pred24$nutrient %in% c('NH4'),'upr'] * sd(excretion2024[excretion2024$nutrient %in% c('NH4'),'value'], na.rm = TRUE) + mean(excretion2024[excretion2024$nutrient %in% c('NH4'),'value'], na.rm = TRUE)
pred24[pred24$nutrient %in% c('SRP'),'upr.value'] <- pred24[pred24$nutrient %in% c('SRP'),'upr'] * sd(excretion2024[excretion2024$nutrient %in% c('SRP'),'value'], na.rm = TRUE) + mean(excretion2024[excretion2024$nutrient %in% c('SRP'),'value'], na.rm = TRUE)
pred24[pred24$nutrient %in% c('NP'),'upr.value'] <- pred24[pred24$nutrient %in% c('NP'),'upr'] * sd(excretion2024[excretion2024$nutrient %in% c('NP'),'value'], na.rm = TRUE) + mean(excretion2024[excretion2024$nutrient %in% c('NP'),'value'], na.rm = TRUE)

ex24.plot2 <- ggplot() +
  xlab('Log Viral Load') + theme_classic() + ylab('Nutrient Excretion Rate') +
  facet_wrap(susc~nutrient, scales = "free") +
  geom_line(data = pred24, aes(x = logviralload, y = nutrient.value, group = nacl, color = as.factor(nacl)), lwd = 1.5) +
  geom_ribbon(data = pred24, aes(x = logviralload, ymax = upr.value, ymin = lwr.value, group = as.factor(nacl), fill = as.factor(nacl)), 
              color = NA, alpha = .2) +
  geom_point(data = excretion2024,#[which(excretion2024$nacl > 0),], 
             aes(x = logviralload, y = value, color = as.factor(nacl))) +
  theme(strip.text.y = element_blank()) +
  #geom_line(data = pred24.0, aes(x = viralload, y = fit, color = nacl), linetype = 'dashed', alpha = .6, lwd = 1.5) +
  #geom_ribbon(data = pred24.0, aes(x = viralload, ymax = upr, ymin = lwr, fill = nacl), 
  #           color = NA, alpha = .1) +
  facet_grid(scaleHR~nutrient, scales = "free") + #labeller = labeller(nutrient = nutrient.labs)) +
  scale_color_manual(#labels = c("-1.41264134002781" = '0', "-0.706320670013903" = '0.25', 
                      #          "0" = '0.5', "0.706320670013903" = '0.75', 
                       #         "1.41264134002781" = '1'), 
                     values = c('black','#440154FF', '#2A788EFF', '#7AD151FF', '#FDE725FF')) +
  scale_fill_manual(values = c('black','#440154FF', '#2A788EFF', '#7AD151FF', '#FDE725FF')) +
  #scale_color_viridis_d(aesthetics = c("colour", "fill")) + 
  #scale_shape_manual(values = c(1,16), labels = c('No NaCl', 'NaCl')) +
  guides(color=guide_legend("Scaled NaCl"), fill = 'none');ex24.plot2
#theme(strip.background = element_blank(),
# strip.text.x = element_blank(),
#legend.title=element_blank())


ex23.plot <- ggplot() +
  geom_point(data = excretion2023, aes(x = logviralload, y = value, color = as.factor(HazardRatio))) +
  xlab('Log Viral Load') + theme_classic() + 
  ylab(expression(atop(paste("Nutrient Excretion Rate"), paste(mu, mol ~ hr^-1 ~ g~of~tadpole^-1)))) +
  facet_wrap(~nutrient, scales = "free") + #labeller = labeller(nutrient = nutrient.labs)) +
  scale_color_manual(labels = c("267.135972567773" = "267.14", "282.553731154595" = "282.55", 
                                "464.542251194654" = "464.54", "531.553805263052" = "531.55", 
                                "729.769292787381" = "729.77", "926.741197236163" = "926.74"),
                     values = c('#440154FF', '#414487FF', '#2A788EFF', '#22A884FF', '#7AD151FF', '#FDE725FF')) +
  guides(colour = guide_legend(nrow = 1)) +
  theme(legend.position = 'bottom') + 
  labs(color = "Hazard Ratio");ex23.plot

#Figure 2
#ggarrange(ex23.plot,ex24.plot, 
 #         ncol = 1, common.legend = FALSE,
  #        legend = 'bottom',
   #       labels = c('A', 'B'),
    #      heights = c(.9,1))

#Supplemental Stats Tables
#write.csv(tidy(NH4.mod2023), '~/Desktop/NH42023.table.csv')
#write.csv(tidy(SRP.mod2023), '~/Desktop/SRP2023.table.csv')
#write.csv(tidy(NP.mod2023), '~/Desktop/NP2023.table.csv')

#write.csv(tidy(NH4.mod2024), '~/Desktop/NH42024.table.csv')
#write.csv(tidy(SRP.mod2024), '~/Desktop/SRP2024.table.csv')
#write.csv(tidy(NP.mod2024), '~/Desktop/NP2024.table.csv')


