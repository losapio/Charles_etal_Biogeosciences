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
pH_H2O<-read_excel("FullData.xlsx",sheet = "Soil")

#select data
pH_H2O<-pH_H2O[c(2:17),c(1:3,14)]
is.numeric(pH_H2O) #be sure it's numeric
sapply(pH_H2O, is.numeric)
pH_H2O$Stage=as.factor(pH_H2O$Stage) #convert stage as categorical


## HISTOGRAMM
hist(pH_H2O$`pH (H2O)`, 10, las =1, col = 'orange') #histogram with non transformed values

### pH ANALYSIS
## MODEL 1 - predictor is numeric (years)
mod.flux2 = lm(`pH (H2O)` ~ poly(Years,2),
               data = pH_H2O)  # y= a+bx+cx2 
mod.flux1 = lm(`pH (H2O)` ~ Years ,
               data = pH_H2O)  # y= a+bx
mod.flux0 = lm(`pH (H2O)` ~ 1 ,
               data = pH_H2O)  # y= a

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
mod.flux2 = lm(`pH (H2O)` ~ poly(Stage,2),
               data = pH_H2O)  # y= a+bx+cx2 
mod.flux1 = lm(`pH (H2O)` ~ Stage ,
               data = pH_H2O)  # y= a+bx
mod.flux0 = lm(`pH (H2O)` ~ 1 ,
               data = pH_H2O)  # y= a

anova(mod.flux0,mod.flux1,mod.flux2) #look at which model has a significant p value
AIC(mod.flux1) #look for a low AIC
AIC(mod.flux2)

# Anova type-II
Anova(mod.flux1) # is years significant (p-value<0.05) ?
summary(mod.flux1) #is each stage significant ?
pH_H2O$Stage <- relevel(pH_H2O$Stage, ref = "1") #relevel and change reference stage
cohens_f(mod.flux1) # effect size index 

## PLOT 
pH_H2O$Stage <- as.factor(pH_H2O$Stage)
couleurs2<-c("tomato2","olivedrab3","cyan2","orchid3")

pdf('figure_pH_box.pdf', 6.5)
ggplot(pH_H2O, aes(x = Years, y = `pH (H2O)`,color=Stage)) +
  geom_boxplot() +
  labs(x = "Years since deglaciation", y = "pH") +
  theme_gray()+
  scale_color_manual(values = couleurs2) +  
  ggtitle("pH according to glacier retreat")+
  theme(plot.title = element_text(size=14,color="black", face="bold",hjust = 0.5))+ 
  theme(axis.text = element_text(size=10), axis.title = element_text(size=12))

dev.off()



