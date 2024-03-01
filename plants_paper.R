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
library(tidyr)

## LOAD data
excel_sheets("FullData.xlsx")
plants <- read_excel("FullData.xlsx",sheet = "Plants")

sapply(plants,is.numeric)
plants$Stage<-as.factor(plants$Stage)


# histogramms
hist(plants$RICHNESS, 10, las =1, col = 'orange')  
hist(plants$DIVERSITY, 10, las =1, col = 'orange') 
hist(plants$pH_landolt,10, las =1, col = 'orange')
hist(plants$`graminoids [%]`, 10, las =1, col = 'orange')
hist(plants$`forbs [%]`, 10, las =1, col = 'orange')
hist(plants$`shrub [%]`, 10, las =1, col = 'orange')
hist(plants$`subshrub [%]`, 10, las =1, col = 'orange')
hist(plants$`tree [%]`, 10, las =1, col = 'orange')

#arranging the table
long_table <- plants %>%
  select(Years, Stage, Plot, `graminoids [%]`, `forbs [%]`,`shrub [%]` , `subshrub [%]` , `tree [%]`) %>%
  rename(graminoids = `graminoids [%]`, forbs = `forbs [%]`,shrubs=`shrub [%]` ,subshrubs= `subshrub [%]` ,trees= `tree [%]`) %>%
  pivot_longer(cols = c(graminoids, forbs,shrubs,subshrubs,trees), 
               names_to = "FamilyPlant", 
               values_to = "Percentage")

sapply(long_table,is.numeric)


### PLANTS GROUPS 
#create sub-tables
graminoids_data <- long_table %>%
  filter(FamilyPlant == "graminoids")
shrub_data <- long_table %>%
  filter(FamilyPlant == "shrubs")
subshrub_data <- long_table %>%
  filter(FamilyPlant == "subshrubs")
forbs_data <- long_table %>%
  filter(FamilyPlant == "forbs")
tree_data <- long_table %>%
  filter(FamilyPlant == "trees")

### group plants ANALYSIS
## MODEL 1 - predictor is numeric (years)
#do the same for each family plant, here is with graminoids
mod.flux2 = lm(Percentage ~ poly(Years,2),
               data = graminoids_data)  # y= a+bx+cx2 
mod.flux1 = lm(Percentage ~ Years ,
               data = graminoids_data)  # y= a+bx
mod.flux0 = lm(Percentage ~ 1 ,
               data = graminoids_data)  # y= a

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
#do the same for each family plant, here is with graminoids
mod.flux2 = lm(Percentage ~ poly(Stage,2),
               data = graminoids_data)  # y= a+bx+cx2 
mod.flux1 = lm(Percentage ~ Stage ,
               data = graminoids_data)  # y= a+bx
mod.flux0 = lm(Percentage ~ 1 ,
               data = graminoids_data)  # y= a

anova(mod.flux0,mod.flux1,mod.flux2) #look at which model has a significant p value
AIC(mod.flux1) #look for a low AIC
AIC(mod.flux2)

# Anova type-II
Anova(mod.flux1) # is years significant (p-value<0.05) ?
summary(mod.flux1) #is each stage significant ?
graminoids_data$Stage <- relevel(graminoids_data$Stage, ref = "1") #relevel and change reference stage
cohens_f(mod.flux1) # effect size index 


## all plants together
long_table$Stage<-as.factor(long_table$Stage)
long_table$FamilyPlant<-as.factor(long_table$FamilyPlant)
#test if it depends on years but also on family plant -
mod.flux1 = lm(long_table$Percentage ~ Years+FamilyPlant ,
               data = long_table)  # y= a+bx
anova(mod.flux1)
summary(mod.flux1)


## boxplot
couleurs <- c("cyan4", "hotpink4", "firebrick2", "gold2","orange2")

pdf('boxplot_familyplants.pdf', 7,5)
ggplot(long_table, aes(x = Stage, y = Percentage,color=FamilyPlant)) +
  geom_boxplot() +
  labs(x = "Years since deglaciation", y = "Presence [%]") +
  theme_gray()+
  scale_color_manual(values = couleurs) +  # Utilisez les couleurs personnalisées
  ggtitle("Percentage of group plants according to glacier retreat")+
  theme(plot.title = element_text(size=14,color="black", face="bold",hjust = 0.5))+ #theme pour le titre
  theme(axis.text = element_text(size=10), axis.title = element_text(size=12))+
  theme(legend.box = "horizontal", legend.background = element_rect(fill="white",linetype="solid", colour="black", linewidth=0.3), legend.title = element_blank(), legend.text=element_text(size=9))   

dev.off()


## RICHNESS VS YEARS
couleurs2<-c("tomato2","olivedrab3","cyan2","orchid3")
plants$Stage<-as.factor(plants$Stage)

## MODEL 1 - predictor is numeric (years)
mod.flux2 = lm(RICHNESS ~ poly(Years,2),
               data = plants)  # y= a+bx+cx2 
mod.flux1 = lm(RICHNESS ~ Years ,
               data = plants)  # y= a+bx
mod.flux0 = lm(RICHNESS ~ 1 ,
               data = plants)  # y= a

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
mod.flux2 = lm(RICHNESS ~ poly(Stage,2),
               data = plants)  # y= a+bx+cx2 
mod.flux1 = lm(RICHNESS ~ Stage ,
               data = plants)  # y= a+bx
mod.flux0 = lm(RICHNESS ~ 1 ,
               data = plants)  # y= a

anova(mod.flux0,mod.flux1,mod.flux2) #look at which model has a significant p value
AIC(mod.flux1) #look for a low AIC
AIC(mod.flux2)

# Anova type-II
Anova(mod.flux1) # is years significant (p-value<0.05) ?
summary(mod.flux1) #is each stage significant ?
plants$Stage <- relevel(plants$Stage, ref = "1") #relevel and change reference stage
cohens_f(mod.flux1) # effect size index 

## PLOT 
pdf('figure_richness.pdf', 6.5)
ggplot(plants, aes(x = Years,
                   y = RICHNESS)) +
  geom_jitter(width = 3) +
  geom_smooth(method = lm, formula = y ~ poly(x,2)) +
  theme_classic()+
  ggtitle("plants RICHNESS")+
  xlab("years since deglaciation")+ylab("richness")+
  theme(plot.title = element_text(color="black", face="bold"))

dev.off()

pdf('figure_richness_box.pdf', 6.5)
ggplot(plants, aes(x = Years, y =RICHNESS,color=Stage)) +
  geom_boxplot() +
  labs(x = "Years since deglaciation", y = "Richness") +
  theme_gray()+
  scale_color_manual(values = couleurs2) +  # Utilisez les couleurs personnalisées
  ggtitle("Specific richness according to glacier retreat")+
  theme(plot.title = element_text(size=18,color="black", face="bold",hjust = 0.5))+ #theme pour le titre
  theme(axis.text = element_text(size=10), axis.title = element_text(size=12))

dev.off()


## DIVERSITY vs YEARS
plants$Stage <- as.factor(plants$Stage)

## MODEL 1 - predictor is numeric (years)
mod.flux2 = lm(DIVERSITY ~ poly(Years,2),
               data = plants)  # y= a+bx+cx2 
mod.flux1 = lm(DIVERSITY ~ Years ,
               data = plants)  # y= a+bx
mod.flux0 = lm(DIVERSITY ~ 1 ,
               data = plants)  # y= a

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
mod.flux2 = lm(DIVERSITY ~ poly(Stage,2),
               data = plants)  # y= a+bx+cx2 
mod.flux1 = lm(DIVERSITY ~ Stage ,
               data = plants)  # y= a+bx
mod.flux0 = lm(DIVERSITY ~ 1 ,
               data = plants)  # y= a

anova(mod.flux0,mod.flux1,mod.flux2) #look at which model has a significant p value
AIC(mod.flux1) #look for a low AIC
AIC(mod.flux2)

# Anova type-II
Anova(mod.flux1) # is years significant (p-value<0.05) ?
summary(mod.flux1) #is each stage significant ?
plants$Stage <- relevel(plants$Stage, ref = "1") #relevel and change reference stage
cohens_f(mod.flux1) # effect size index 

## PLOT 
pdf('figure_diversity_box.pdf', 6.5)
ggplot(plants, aes(x = Years, y =DIVERSITY,color=Stage)) +
  geom_boxplot() +
  labs(x = "Years since deglaciation", y = "Shannon diversity index") +
  theme_gray()+
  scale_color_manual(values = couleurs2) +  # Utilisez les couleurs personnalisées
  ggtitle("Plant diversity according to glacier retreat")+
  theme(plot.title = element_text(size=20,color="black", face="bold",hjust = 0.5))+ #theme pour le titre
  theme(axis.text = element_text(size=15), axis.title = element_text(size=17))

dev.off()

pdf('figure_DIVERSITY.pdf', 6.5)
ggplot(plants, aes(x = Years,
                   y = DIVERSITY)) +
  geom_smooth(method = lm, formula = y ~ poly(x,2), color="black") +
  geom_jitter(size=2,width = 3, aes(color=case_when(
    Stage ==1 ~ "stage 1",
    Stage == 2 ~ "stage 2",
    Stage == 3 ~ "stage 3",
    Stage == 4 ~ "stage 4"))) +
  theme_bw()+
  ggtitle("Plant diversity according to glacier retreat")+
  xlab("Years since deglaciation")+ylab("Shannon diversity index")+
  theme(plot.title = element_text(size=14,color="black", face="bold",hjust = 0.5))+
  theme(axis.text = element_text(size=10), axis.title = element_text(size=12))+
  theme(legend.position = c(0.9, 0.75),legend.box = "horizontal", legend.background = element_rect(fill="azure",linetype="solid", colour="black", size=0.3), legend.title = element_blank(), legend.text=element_text(size=10)) 

dev.off()

### pH landolt vs YEARS 
plants$Stage <- as.factor(plants$Stage)

## MODEL 1 - predictor is numeric (years)
mod.flux2 = lm(pH_landolt ~ poly(Years,2),
               data = plants)  # y= a+bx+cx2 
mod.flux1 = lm(pH_landolt ~ Years ,
               data = plants)  # y= a+bx
mod.flux0 = lm(pH_landolt ~ 1 ,
               data = plants)  # y= a

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
mod.flux2 = lm(pH_landolt ~ poly(Stage,2),
               data = plants)  # y= a+bx+cx2 
mod.flux1 = lm(pH_landolt ~ Stage ,
               data = plants)  # y= a+bx
mod.flux0 = lm(pH_landolt ~ 1 ,
               data = plants)  # y= a

anova(mod.flux0,mod.flux1,mod.flux2) #look at which model has a significant p value
AIC(mod.flux1) #look for a low AIC
AIC(mod.flux2)

# Anova type-II
Anova(mod.flux1) # is years significant (p-value<0.05) ?
summary(mod.flux1) #is each stage significant ?
plants$Stage <- relevel(plants$Stage, ref = "1") #relevel and change reference stage
cohens_f(mod.flux1) # effect size index 

#boxplot
couleurs2<-c("tomato2","olivedrab3","cyan2","orchid3")

pdf('figure_pH_landolt_box.pdf', 6.5)
ggplot(plants, aes(x = Years, y =pH_landolt,color=Stage)) +
  geom_boxplot() +
  labs(x = "Years since deglaciation", y = "Reaction value") +
  theme_gray()+
  scale_color_manual(values = couleurs2) +  # Utilisez les couleurs personnalisées
  ggtitle("Plant reaction value according to glacier retreat")+
  theme(plot.title = element_text(size=20,color="black", face="bold",hjust = 0.5))+ #theme pour le titre
  theme(axis.text = element_text(size=15), axis.title = element_text(size=17))

dev.off()


### Coverage vs years
plants$Stage <- as.factor(plants$Stage)

## MODEL 1 - predictor is numeric (years)
mod.flux2 = lm(coverage_sum ~ poly(Years,2),
               data = plants)  # y= a+bx+cx2 
mod.flux1 = lm(coverage_sum ~ Years ,
               data = plants)  # y= a+bx
mod.flux0 = lm(coverage_sum ~ 1 ,
               data = plants)  # y= a

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
mod.flux2 = lm(coverage_sum ~ poly(Stage,2),
               data = plants)  # y= a+bx+cx2 
mod.flux1 = lm(coverage_sum ~ Stage ,
               data = plants)  # y= a+bx
mod.flux0 = lm(coverage_sum ~ 1 ,
               data = plants)  # y= a

anova(mod.flux0,mod.flux1,mod.flux2) #look at which model has a significant p value
AIC(mod.flux1) #look for a low AIC
AIC(mod.flux2)

# Anova type-II
Anova(mod.flux1) # is years significant (p-value<0.05) ?
summary(mod.flux1) #is each stage significant ?
plants$Stage <- relevel(plants$Stage, ref = "1") #relevel and change reference stage
cohens_f(mod.flux1) # effect size index 
