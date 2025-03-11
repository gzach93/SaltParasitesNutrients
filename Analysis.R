#Load in Libraries
library(ggplot2)
library(readxl)
library(tidyverse)
library(nimble)
library(ggpubr)
library(ggpubr)
library(MASS)

#Laod in Data
#Excretion 2023 Data -- FV3 and Nutrients but no chloride
excretion2023 <- read.csv('Data/Excretion2023.csv')

#Excretion 2024 Data -- FV3, NaCl, and Nutrients
excretion2024 <- read.csv('Data/Excretion2024.csv')

#Plaque Assay Data 
nacl <- read.csv('Data/PlaqueAssay_NaCl.csv')

#2024 Field Data
field2024 <- read.csv('Data/Field2024_Viral_Chloride.csv')


####################################################
####################################################
########### 2024 Excretion Virus Free ##############
####################################################
####################################################

salt.control24 <- excretion2024[which(excretion2024$rv == 'CTRL'),]#50 tadpoles

#Make Concentration Numeric
salt.control24[salt.control24$nacl %in% c("0 g/L"),'nacl'] <- 0
salt.control24[salt.control24$nacl %in% c("0.25 g/L"),'nacl'] <- 0.25
salt.control24[salt.control24$nacl %in% c("0.5 g/L"),'nacl'] <- 0.5
salt.control24[salt.control24$nacl %in% c("0.75 g/L"),'nacl'] <- 0.75
salt.control24[salt.control24$nacl %in% c("1.0 g/L"),'nacl'] <- 1
salt.control24$nacl <- as.numeric(salt.control24$nacl)

salt.control.mod.SRP <- lm(value ~ nacl, data = salt.control24[salt.control24$nutrient %in% c('SRP'),])
salt.control.mod.NH4 <- lm(value ~ nacl, data = salt.control24[salt.control24$nutrient %in% c('NH4'),])
salt.control.mod.NP <- lm(value ~ nacl, data = salt.control24[salt.control24$nutrient %in% c('NP'),])

summary(salt.control.mod.SRP)
summary(salt.control.mod.NH4)
summary(salt.control.mod.NP)

cl.p.plot <- ggplot() +
  geom_point(data = salt.control24[salt.control24$nutrient %in% c('SRP'),], aes(y = value, x = nacl)) +
  theme_classic() +
  xlab('NaCl Concentration (mg/L)') +
  ylab('Total Phosphorus Excretion Rate');cl.p.plot

cl.n.plot <- ggplot() +
  geom_point(data = salt.control24[salt.control24$nutrient %in% c('NH4'),], aes(y = value, x = nacl)) +
  theme_classic() +
  xlab('NaCl Concentration (mg/L)') +
  ylab('Total Nitrogen Excretion Rate');cl.n.plot

cl.np.plot <- ggplot() +
  geom_point(data = salt.control24[salt.control24$nutrient %in% c('NP'),], aes(y = value, x = nacl)) +
  theme_classic() +
  xlab('NaCl Concentration (mg/L)') +
  ylab('Nitrogen:Phosphorus Ratio');cl.np.plot

ggarrange(cl.n.plot, cl.p.plot, cl.np.plot,
          nrow = 1)

##############################################################
##############################################################
################# 2023 Excretion Assay #######################
##############################################################
##############################################################

#NH4 - Not Significant Decrease
NH4.mod2023 <- lm(value ~ logviralload, data = excretion2023[excretion2023$nutrient %in% c('NH4'),])
summary(NH4.mod2023)

#SRP - Not Significant Increase
SRP.mod2023 <- lm(value ~ logviralload, data = excretion2023[excretion2023$nutrient %in% c('SRP'),])
summary(SRP.mod2023) #Not Significant increase

#N:P Ration - Not Significant Decrease
NP.mod2023 <- lm(value ~ logviralload, data = excretion2023[excretion2023$nutrient %in% c('NP'),])
summary(NP.mod2023)

#Create Model Fit
NH4.pred <- data.frame(predict(NH4.mod2023, newdata = data.frame('logviralload' = seq(1,3,by = .25)), interval = c('confidence')))
SRP.pred <- data.frame(predict(SRP.mod2023, newdata = data.frame('logviralload' = seq(1,3,by = .25)), interval = c('confidence')))
NP.pred <- data.frame(predict(NP.mod2023, newdata = data.frame('logviralload' = seq(1,3,by = .25)), interval = c('confidence')))
NH4.pred$nutrient <- 'NH4'
SRP.pred$nutrient <- 'SRP'
NP.pred$nutrient <- 'NP'
pred23 <- rbind(NH4.pred, SRP.pred, NP.pred)
pred23$logviralload <- seq(1,3,by = .25)
excretion2023$nutrient <- factor(excretion2023$nutrient, levels = c('NH4', 'SRP', 'NP'))
pred23$nutrient <- factor(pred23$nutrient, levels = c('NH4', 'SRP', 'NP'))

#2023 Excretion Assay Plot
ggplot() +
  geom_point(data = excretion2023, aes(x = logviralload, y = value)) +
  xlab('Log Viral Load') + theme_classic() + ylab('Nutrient Excretion') +
  facet_wrap(~nutrient, scales = "free") +
  geom_line(data = pred23, aes(x = logviralload, y = fit), lwd = 1.5) +
  geom_ribbon(data = pred23, aes(x = logviralload, ymax = upr, ymin = lwr), 
              color = NA, alpha = .2) +
  ggtitle("Chloride Free")

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

##### NH4 #####
#NH4 Salt Trt - Significant Increase with Viral Load, NaCl and interaction Not Significant
NH4.mod2024 <- lm(value ~ logviralload * nacl, data = excretion2024[excretion2024$nutrient %in% c('NH4') & excretion2024$nacl > 0,]) #& excretion2024$nacl %in% c(0.25, 0.50, 0.75, 1),])
summary(NH4.mod2024)

#NH4 Salt Free - Increase Not Significant 
NH4.mod2024.0 <- lm(value ~ logviralload, data = excretion2024[excretion2024$nutrient %in% c('NH4') & excretion2024$chloride %in% c(0),])
summary(NH4.mod2024.0)

#### SRP ####
#SRP Salt Trt - No Significance Relatioships
SRP.mod2024 <- lm(value ~ logviralload * nacl, data = excretion2024[excretion2024$nutrient %in% c('SRP') & excretion2024$nacl > 0,]) #& excretion2024$nacl %in% c(0.25, 0.50, 0.75, 1),])
summary(SRP.mod2024)

#SRP Salt Free - Decrease Not Significant
SRP.mod2024.0 <- lm(value ~ logviralload, data = excretion2024[excretion2024$nutrient %in% c('SRP') & excretion2024$chloride %in% c(0),])
summary(SRP.mod2024.0)

#### NP Ratio ####
#NP Ratio - Increase with NaCl and Viral Load - Interaction Decrease - All Significant
NP.mod2024 <- lm(value ~ logviralload * nacl, data = excretion2024[excretion2024$nutrient %in% c('NP') & excretion2024$nacl > 0,]) #& excretion2024$nacl %in% c(0.25, 0.50, 0.75, 1),])
summary(NP.mod2024)

#NP Ratio - Increase Not Significant
NP.mod2024.0 <- lm(value ~ logviralload, data = excretion2024[excretion2024$nutrient %in% c('NP') & excretion2024$chloride %in% c(0),])
summary(NP.mod2024.0)

#Model Fits - Salt Trts
NH4.pred24 <- data.frame(predict(NH4.mod2024, newdata = data.frame(expand.grid('logviralload' = seq(1,6,by = .25), 'nacl' = c(.25,.5,.75,1))), interval = c('confidence')))
SRP.pred24 <- data.frame(predict(SRP.mod2024, newdata = data.frame(expand.grid('logviralload' = seq(1,6,by = .25), 'nacl' = c(.25,.5,.75,1))), interval = c('confidence')))
NP.pred24 <- data.frame(predict(NP.mod2024, newdata = data.frame(expand.grid('logviralload' = seq(1,6,by = .25), 'nacl' = c(.25,.5,.75,1))), interval = c('confidence')))
NH4.pred24$nutrient <- 'NH4'
SRP.pred24$nutrient <- 'SRP'
NP.pred24$nutrient <- 'NP'
pred24 <- rbind(NH4.pred24, SRP.pred24, NP.pred24)
pred24$logviralload <- seq(1,6,by = .25)
pred24$nutrient <- factor(pred24$nutrient, levels = c('NH4', 'SRP', 'NP'))
pred24$nacl <- rep(rep(c(.25,.5,.75,1), each = 21), times = 3)

#Model Fits - Salt Free Trt
NH4.pred24.0 <- data.frame(predict(NH4.mod2024.0, newdata = data.frame('logviralload' = seq(1,6,by = .25)), interval = c('confidence')))
SRP.pred24.0 <- data.frame(predict(SRP.mod2024.0, newdata = data.frame('logviralload' = seq(1,6,by = .25)), interval = c('confidence')))
NP.pred24.0 <- data.frame(predict(NP.mod2024.0, newdata = data.frame('logviralload' = seq(1,6,by = .25)), interval = c('confidence')))
NH4.pred24.0$nutrient <- 'NH4'
SRP.pred24.0$nutrient <- 'SRP'
NP.pred24.0$nutrient <- 'NP'
pred24.0 <- rbind(NH4.pred24.0, SRP.pred24.0, NP.pred24.0)
pred24.0$logviralload <- seq(1,6,by = .25)
pred24.0$nutrient <- factor(pred24.0$nutrient, levels = c('NH4', 'SRP', 'NP'))

#Reorder Excretion 2024 Nutrients
excretion2024$nutrient <- factor(excretion2024$nutrient, levels = c('NH4', 'SRP', 'NP'))

#Salt Trt Plot
ggplot() +
  geom_point(data = excretion2024, aes(x = logviralload, y = value, color = nacl)) +
  xlab('Log Viral Load') + theme_classic() + ylab('Nutrient Excretion') +
  facet_wrap(~nutrient, scales = "free") +
  geom_line(data = pred24, aes(x = logviralload, y = fit, color = nacl, group = nacl), lwd = 1.5) +
  geom_ribbon(data = pred24, aes(x = logviralload, ymax = upr, ymin = lwr, group = nacl, fill = nacl), 
              color = NA, alpha = .2) +
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
nacl$sq.chloride <- nacl$chloride^2

#Plaque Model - 
plaque.mod <- glm.nb(plaques ~ chloride + sq.chloride, data = nacl)
plaque.mod1 <- glm.nb(plaques ~ chloride, data = nacl)

#Linear vs Nonlinear - Nonlinear has lower AIC
summary(plaque.mod) # Both Terms significant
summary(plaque.mod1) #Significant increase
AIC(plaque.mod)
AIC(plaque.mod1)

#Model Checks
(length(residuals(plaque.mod, type="pearson")[residuals(plaque.mod, type="pearson") > 2]) + length(residuals(plaque.mod, type="pearson")[residuals(plaque.mod, type="pearson") < -2]))/length(residuals(plaque.mod, type="pearson"))
plot(residuals(plaque.mod, type = 'pearson') ~ plaque.mod$fitted.values)
abline(h = 0)

#Model Fits
plaque.pred <- data.frame(predict(plaque.mod, newdata = data.frame('chloride' = seq(0,1900,by = 100),
                                                                   'sq.chloride' = seq(0,1900,by = 100)^2), 
                                  se.fit = TRUE, type = 'response'))
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
  geom_point(data = excretion2024, aes(x = chloride, y = logviralload)) +
  ylab('Log Viral Load') + theme_classic() + xlab('Chloride (mg/L)')

#Model - Increase Not Significant
VL.mod2024 <- lm(logviralload ~ chloride, data = excretion2024)
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

##############################################################
##############################################################
############## 2024 Field Nutrient Data ######################
##############################################################
##############################################################

#Log transform Nutrient Data
field2024$TN <- log(field2024$TN)
field2024$TP <- log(field2024$TP)
field2024$NP <- log(field2024$NP)


#SRP - No Relationship
field.SRP <- lm(TP~viral_load * mean.chloride, data = field2024)
summary(field.SRP)

#Model Fits
field.SRP.pred <- data.frame(predict(field.SRP, 
                          newdata = data.frame(
                          'viral_load' = field2024$viral_load,
                           'mean.chloride' = field2024$mean.chloride),
                          interval = 'confidence'))
field.SRP.pred$logviral <- field2024$viral_load
field.SRP.pred$name <- field2024$mean.chloride

#SRP Plot
ggplot() +
  geom_point(data = field2024, aes(x = viral_load, y = TP)) +
  theme_classic() +
  geom_line(data = field.SRP.pred, aes(x = logviral, y = fit))


#NH4 - No Relationships
field.NH4 <- lm(TN~viral_load * mean.chloride, data = field2024)
summary(field.NH4)

#Model Fits
field.NH4.pred <- data.frame(predict(field.NH4, 
                                     newdata = data.frame(
                                       'viral_load' = field2024$viral_load,
                                       'mean.chloride' = field2024$mean.chloride),
                                     interval = 'confidence'))
field.NH4.pred$logviral <- field2024$viral_load
field.NH4.pred$name <- field2024$mean.chloride

#Model Fit Plot
ggplot() +
  geom_point(data = field2024, aes(x = viral_load, y = TN)) +
  theme_classic() +
  geom_line(data = field.NH4.pred, aes(x = logviral, y = fit))


#N:P Ratio - No Relationships
field.NP <- lm(NP~viral_load * mean.chloride, data = field2024)
summary(field.NP)

field.NP.pred <- data.frame(predict(field.NP, 
                                     newdata = data.frame(
                                       'viral_load' = field2024$viral_load,
                                       'mean.chloride' = field2024$mean.chloride),
                                     interval = 'confidence'))
field.NP.pred$logviral <- field2024$viral_load
field.NP.pred$name <- field2024$mean.chloride

#Model Fit Plot
ggplot() +
  geom_point(data = field2024, aes(x = viral_load, y = NP)) +
  theme_classic() +
  geom_line(data = field.NP.pred, aes(x = logviral, y = fit))

#### Model Checks #####
shapiro.test(field.NH4$residuals)
shapiro.test(field.SRP$residuals)
shapiro.test(field.NP$residuals)
plot(field.NH4)
plot(field.SRP)
plot(field.NP)

#Combine Model Fits
field24.preds <- rbind(field.SRP.pred, field.NH4.pred,field.NP.pred)


##############################################
##############################################
#### Linear Model Viral Load Exps. Figure ####
##############################################
##############################################
nutrient.labs <- c('Total N', 'Total P', 'N:P Ratio')
names(nutrient.labs) <- c('NH4', 'SRP', 'NP')

excretion2024$salt_YN <- 'Y'
excretion2024[excretion2024$nacl %in% c(0),'salt_YN'] <- 'N'

excretion2023$salt_YN <- 'N'

pred24.0$nacl <- as.factor(0)
pred24$nacl <- as.factor(pred24$nacl)

ex24.plot <- ggplot() +
  geom_point(data = excretion2024,#[which(excretion2024$nacl > 0),], 
             aes(x = logviralload, y = value, color = as.factor(nacl),
                 shape = salt_YN)) +
  xlab('Log Viral Load') + theme_classic() + ylab('Nutrient Excretion') +
  facet_wrap(~nutrient, scales = "free") +
  geom_line(data = pred24, aes(x = logviralload, y = fit, group = nacl, color = nacl), lwd = 1.5) +
  geom_ribbon(data = pred24, aes(x = logviralload, ymax = upr, ymin = lwr, group = nacl, fill = nacl), 
              color = NA, alpha = .2) +
  geom_line(data = pred24.0, aes(x = logviralload, y = fit, color = nacl), linetype = 'dashed', alpha = .6, lwd = 1.5) +
  geom_ribbon(data = pred24.0, aes(x = logviralload, ymax = upr, ymin = lwr, fill = nacl), 
              color = NA, alpha = .1) +
  facet_wrap(~nutrient, scales = "free", labeller = labeller(nutrient = nutrient.labs)) +
  #scale_color_manual(labels = c('0', '0.25', '0.5', '0.75', '1'), values = c('black','#440154FF', '#2A788EFF', '#7AD151FF', '#FDE725FF')) +
  #scale_fill_manual(labels = c('0', '0.25', '0.5', '0.75', '1'), values = c('black','#440154FF', '#2A788EFF', '#7AD151FF', '#FDE725FF')) +
  scale_color_viridis_d(aesthetics = c("colour", "fill")) + 
  scale_shape_manual(values = c(1,16), labels = c('No NaCl', 'NaCl')) +
  guides(color=guide_legend("Nutrients"), fill = 'none') +
  theme(strip.background = element_blank(),
    strip.text.x = element_blank(),
    legend.title=element_blank());ex24.plot

ex23.plot <- ggplot() +
  geom_point(data = excretion2023, aes(x = logviralload, y = value, color = nutrient, shape = salt_YN)) +
  xlab('') + theme_classic() + ylab('Nutrient Excretion') +
  geom_line(data = pred23, aes(x = logviralload, y = fit, color = nutrient), lwd = 1.5, alpha = .6, linetype = 'dashed') +
  geom_ribbon(data = pred23, aes(x = logviralload, ymax = upr, ymin = lwr, fill = nutrient), 
              color = NA, alpha = .1) +
  facet_wrap(~nutrient, scales = "free", labeller = labeller(nutrient = nutrient.labs)) +
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

