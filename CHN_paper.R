setwd("/Users/cecilecharles/Desktop/paper/code")

library(lme4)
library(lmerTest)
library(car)
library(effects)
library(effectsize)
library(parameters)
library(emmeans)
library(ggplot2)
library(readxl)
library(dplyr)
library (ggpubr)
library(ggpmisc)

## LOAD DATA
excel_sheets("FullData.xlsx")
CHN<-read_excel("FullData.xlsx",sheet = "Soil")

#select data
CHN<-CHN[c(2:17),c(1:3,11:13)]
is.numeric(CHN) #be sure it's numeric
sapply(CHN, is.numeric)
CHN$Stage=as.factor(CHN$Stage) #convert stage as categorical

#create dataframe
C_data <- subset(CHN, select = c("Years","Stage","Corg  (%)"))
N_data<- subset(CHN, select = c("Years","Stage","Ntot (%)"))
CN_data<- subset(CHN, select = c("Years","Stage","C/N"))

## HISTOGRAMS and LOG datas
hist(C_data$`Corg  (%)`, 10, las =1, col = 'orange') #histogram with non transformed values
hist(N_data$`Ntot (%)`, 10, las =1, col = 'orange') #histogram with non transformed values
hist(CN_data$`C/N`, 10, las =1, col = 'orange') #histogram with non transformed values

#add log data
C_data$log_carb <- log(C_data$`Corg  (%)`)
hist(C_data$log_carb, 10, las =1, col = 'orange') #histogram with log transformed values
N_data$log_nit <- log(N_data$`Ntot (%)`)
hist(N_data$log_nit, 10, las =1, col = 'orange') #histogram with log transformed values
CN_data$log_CN <- log(CN_data$`C/N`)
hist(CN_data$log_CN, 10, las =1, col = 'orange') #histogram with log transformed values


### CARBON ANALYSIS
## MODEL 1 - predictor is numeric (years)
mod.flux2 = lm(log_carb ~ poly(Years,2),
               data = C_data)  # y= a+bx+cx2 
mod.flux1 = lm(log_carb ~ Years ,
               data = C_data)  # y= a+bx
mod.flux0 = lm(log_carb ~ 1 ,
               data = C_data)  # y= a

anova(mod.flux0,mod.flux1,mod.flux2) #look at which model has a significant p value
AIC(mod.flux2) #look for a low AIC
AIC(mod.flux1) 

# Anova type-II
Anova(mod.flux1) # is years significant (p-value<0.05) ?

summary(mod.flux1) #is years significant ?
cohens_f(mod.flux1) # look for a value > 0.2

modpars = model_parameters(mod.flux1, effects = "fixed", ci_method = "wald")
print(modpars) 
str(modpars) #look structure
modpars$Parameter
plot(mod.flux1) #look QQplot, residuals

## MODEL 2 - predictor is caregorical (stage)
mod.flux2 = lm(log_carb ~ poly(Stage,2),
               data = C_data)  # y= a+bx+cx2 
mod.flux1 = lm(log_carb ~ Stage ,
               data = C_data)  # y= a+bx
mod.flux0 = lm(log_carb ~ 1 ,
               data = C_data)  # y= a

anova(mod.flux0,mod.flux1,mod.flux2) #look at which model has a significant p value
AIC(mod.flux1) #look for a low AIC
AIC(mod.flux2)

# Anova type-II
Anova(mod.flux1) # is years significant (p-value<0.05) ?
summary(mod.flux1) #is each stage significant ?
C_data$Stage <- relevel(C_data$Stage, ref = "1") #relevel and change reference stage
cohens_f(mod.flux1) # effect size index 

## PLOT 
C_data$Stage <- as.factor(C_data$Stage)
couleurs2<-c("tomato2","olivedrab3","cyan2","orchid3")

pdf('figure_SOC_box.pdf', 6.5)
ggplot(C_data, aes(x = Years, y = `Corg  (%)`,color=Stage)) +
  geom_boxplot() +
  labs(x = "Years since deglaciation", y = "SOC [%]") +
  theme_gray()+
  scale_color_manual(values = couleurs2) +  
  ggtitle("Organic carbon according to glacier retreat")+
  theme(plot.title = element_text(size=14,color="black", face="bold",hjust = 0.5))+ 
  theme(axis.text = element_text(size=10), axis.title = element_text(size=12))+
  scale_y_log10()+annotation_logticks(sides="l")

dev.off()


### NITROGEN ANALYSIS
## MODEL 1 - predictor is numeric (years)
mod.flux2 = lm(log_nit ~ poly(Years,2),
               data = N_data)  # y= a+bx+cx2 
mod.flux1 = lm(log_nit ~ Years ,
               data = N_data)  # y= a+bx
mod.flux0 = lm(log_nit ~ 1 ,
               data = N_data)  # y= a

anova(mod.flux0,mod.flux1,mod.flux2) #look at which model has a significant p value
AIC(mod.flux2) #look for a low AIC
AIC(mod.flux1) 

# Anova type-II
Anova(mod.flux1) # is years significant (p-value<0.05) ?

summary(mod.flux1) #is years significant ?
cohens_f(mod.flux1) # look for a value > 0.2

modpars = model_parameters(mod.flux1, effects = "fixed", ci_method = "wald")
print(modpars) 
str(modpars) #look structure
modpars$Parameter
plot(mod.flux1) #look QQplot, residuals

## MODEL 2 - predictor is caregorical (stage)
mod.flux2 = lm(log_nit ~ poly(Stage,2),
               data = N_data)  # y= a+bx+cx2 
mod.flux1 = lm(log_nit ~ Stage ,
               data = N_data)  # y= a+bx
mod.flux0 = lm(log_nit ~ 1 ,
               data = N_data)  # y= a

anova(mod.flux0,mod.flux1,mod.flux2) #look at which model has a significant p value
AIC(mod.flux1) #look for a low AIC
AIC(mod.flux2)

# Anova type-II
Anova(mod.flux1) # is years significant (p-value<0.05) ?
summary(mod.flux1) #is each stage significant ?
N_data$Stage <- relevel(N_data$Stage, ref = "1") #relevel and change reference stage
cohens_f(mod.flux1) # effect size index 

## PLOT 
N_data$Stage <- as.factor(N_data$Stage)
couleurs2<-c("tomato2","olivedrab3","cyan2","orchid3")

pdf('figure_Ntot_box.pdf', 6.5)
ggplot(N_data, aes(x = Years, y = `Ntot (%)`,color=Stage)) +
  geom_boxplot() +
  labs(x = "Years since deglaciation", y = "Ntot [%]") +
  theme_gray()+
  scale_color_manual(values = couleurs2) +  
  ggtitle("Total nitrogen according to glacier retreat")+
  theme(plot.title = element_text(size=14,color="black", face="bold",hjust = 0.5))+ 
  theme(axis.text = element_text(size=10), axis.title = element_text(size=12))+
  scale_y_log10()+annotation_logticks(sides="l")
dev.off()

### C/N ratio ANALYSIS
## MODEL 1 - predictor is numeric (years)
mod.flux2 = lm(log_CN ~ poly(Years,2),
               data = CN_data)  # y= a+bx+cx2 
mod.flux1 = lm(log_CN ~ Years ,
               data = CN_data)  # y= a+bx
mod.flux0 = lm(log_CN ~ 1 ,
               data = CN_data)  # y= a

anova(mod.flux0,mod.flux1,mod.flux2) #look at which model has a significant p value
AIC(mod.flux2) #look for a low AIC
AIC(mod.flux1) 

# Anova type-II
Anova(mod.flux1) # is years significant (p-value<0.05) ?

summary(mod.flux1) #is years significant ?
cohens_f(mod.flux1) # look for a value > 0.2

modpars = model_parameters(mod.flux1, effects = "fixed", ci_method = "wald")
print(modpars) 
str(modpars) #look structure
modpars$Parameter
plot(mod.flux1) #look QQplot, residuals

## MODEL 2 - predictor is caregorical (stage)
mod.flux2 = lm(log_CN ~ poly(Stage,2),
               data = CN_data)  # y= a+bx+cx2 
mod.flux1 = lm(log_CN ~ Stage ,
               data = CN_data)  # y= a+bx
mod.flux0 = lm(log_CN ~ 1 ,
               data = CN_data)  # y= a

anova(mod.flux0,mod.flux1,mod.flux2) #look at which model has a significant p value
AIC(mod.flux1) #look for a low AIC
AIC(mod.flux2)

# Anova type-II
Anova(mod.flux1) # is years significant (p-value<0.05) ?
summary(mod.flux1) #is each stage significant ?
N_data$Stage <- relevel(N_data$Stage, ref = "1") #relevel and change reference stage
cohens_f(mod.flux1) # effect size index 

## PLOT 
CN_data$Stage <- as.factor(CN_data$Stage)
couleurs2<-c("tomato2","olivedrab3","cyan2","orchid3")

pdf('figure_CN_box.pdf', 6.5)
ggplot(CN_data, aes(x = Years, y = `C/N`,color=Stage)) +
  geom_boxplot() +
  labs(x = "Years since deglaciation", y = "C/N [%]") +
  theme_gray()+
  scale_color_manual(values = couleurs2) +  
  ggtitle("C/N ratio according to glacier retreat")+
  theme(plot.title = element_text(size=14,color="black", face="bold",hjust = 0.5))+ 
  theme(axis.text = element_text(size=10), axis.title = element_text(size=12))+
  scale_y_log10()+annotation_logticks(sides="l")

dev.off()
