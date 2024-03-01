setwd("/Users/cecilecharles/Desktop/paper/code")
## analysis of MAJOR elements

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

## LOAD DATA
excel_sheets("FullData.xlsx")
XRF<-read_excel("FullData.xlsx",sheet = "Full")

excel_sheets("XRF_CécileC.xlsx")
read_excel("XRF_CécileC.xlsx",sheet = "XRF_corr")
XRF <- read_excel("XRF_CécileC.xlsx",sheet = "XRF_corr")

#faire un tableau avec %age des LOI enlevés et le reste est adapté 
XRF <- XRF[,c(1:3,23:32) ]
XRF <- XRF %>%
  pivot_longer(cols = c(`Al %`, `Ca %`,`K %`,`Mg %`,`Mn %`,`Na %`,`P %`,`Si %`,`Ti %`,`Fe %`),  # Sélectionnez les colonnes à pivoter
               names_to = "element",  # Nom pour la nouvelle colonne des anciens noms de colonne
               values_to = "values")  # Nom pour la nouvelle colonne des valeurs

XRF$Years<-as.numeric(XRF$Years)
XRF$values<-as.numeric(XRF$values)

## representation graphique
couleursSI = c("orangered")
couleursAL = c("darkcyan")
couleursFE = c("tan1")
couleursTI = c("darkseagreen")
couleursP2 = c("palevioletred")
couleursMN = c("steelblue")
couleurs4 = c("blue")
couleurs3 = c("red")
couleurs2 = c("brown")
couleurs1 = c("lightblue")

selected_elements <- c("Si %")
filtered_data <- XRF %>%
  filter(element %in% selected_elements)
mean1 = mean(filtered_data$values)

SiO = ggplot(filtered_data, aes(fill = element, y = values, x = Stage)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.3) +
  geom_hline(aes(yintercept=mean1),linetype = "dashed", color = "black")+
  ggtitle("SiO2") +
  theme(
    plot.title = element_text(size = 14, color = "black", face = "bold", hjust = 0.5),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    legend.position = "none",  # Enlever la légende
  ) +
  ylab("Oxide [%]")+
  facet_grid(element ~ ., scales = "free")+
  scale_fill_manual(values = couleursSI)

selected_elements <- c("Al %")
filtered_data1 <- XRF %>%
  filter(element %in% selected_elements)
mean2 = mean(filtered_data1$values)

Al2O3 = ggplot(filtered_data1, aes(fill = element, y = values, x = Stage)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.3) +
  geom_hline(aes(yintercept=mean2),linetype = "dashed", color = "black")+
  ggtitle("Al2O3") +
  theme(
    plot.title = element_text(size = 14, color = "black", face = "bold", hjust = 0.5),axis.text = element_text(size = 10),axis.title = element_text(size = 12),legend.position = "none",  # Enlever la légende
  ) +
  ylab("Oxide [%]")+
  facet_grid(element ~ ., scales = "free")+
  scale_fill_manual(values = couleursAL)

selected_elements <- c("Fe %")
filtered_data2 <- XRF %>%
  filter(element %in% selected_elements)
mean3 = mean(filtered_data2$values)

Fe2O3 = ggplot(filtered_data2, aes(fill = element, y = values, x = Stage)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.3) +
  geom_hline(aes(yintercept=mean3),linetype = "dashed", color = "black")+
  ggtitle("Fe2O3") +
  theme(
    plot.title = element_text(size = 14, color = "black", face = "bold", hjust = 0.5),axis.text = element_text(size = 10),axis.title = element_text(size = 12),legend.position = "none",  # Enlever la légende
  ) +
  ylab("Oxide [%]")+
  facet_grid(element ~ ., scales = "free")+
  scale_fill_manual(values = couleursFE)

selected_elements <- c("Ca %")
filtered_data3 <- XRF %>%
  filter(element %in% selected_elements)
mean4 = mean(filtered_data3$values)

CaO = ggplot(filtered_data3, aes(fill = element, y = values, x = Stage)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.3) +
  geom_hline(aes(yintercept=mean4),linetype = "dashed", color = "black")+
  ggtitle("CaO") +
  theme(
    plot.title = element_text(size = 14, color = "black", face = "bold", hjust = 0.5),axis.text = element_text(size = 10),axis.title = element_text(size = 12),legend.position = "none",  # Enlever la légende
  ) +
  ylab("Oxide [%]")+
  facet_grid(element ~ ., scales = "free")+
  scale_fill_manual(values = couleurs1)

selected_elements <- c("K %")
filtered_data4 <- XRF %>%
  filter(element %in% selected_elements)
mean5 = mean(filtered_data4$values)

K2O = ggplot(filtered_data4, aes(fill = element, y = values, x = Stage)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.3) +
  geom_hline(aes(yintercept=mean5),linetype = "dashed", color = "black")+
  ggtitle("K2O") +
  theme(
    plot.title = element_text(size = 14, color = "black", face = "bold", hjust = 0.5),axis.text = element_text(size = 10),axis.title = element_text(size = 12),legend.position = "none",  # Enlever la légende
  ) +
  ylab("Oxide [%]")+
  facet_grid(element ~ ., scales = "free")+
  scale_fill_manual(values = couleurs2)

selected_elements <- c("Mg %")
filtered_data5 <- XRF %>%
  filter(element %in% selected_elements)
mean6 = mean(filtered_data5$values)

MgO = ggplot(filtered_data5, aes(fill = element, y = values, x = Stage)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.3) +
  geom_hline(aes(yintercept=mean6),linetype = "dashed", color = "black")+
  ggtitle("MgO") +
  theme(
    plot.title = element_text(size = 14, color = "black", face = "bold", hjust = 0.5),axis.text = element_text(size = 10),axis.title = element_text(size = 12),legend.position = "none",  # Enlever la légende
  ) +
  ylab("Oxide [%]")+
  facet_grid(element ~ ., scales = "free")+
  scale_fill_manual(values = couleurs3)

selected_elements <- c("Na %")
filtered_data6 <- XRF %>%
  filter(element %in% selected_elements)
mean7 = mean(filtered_data6$values)

Na2O = ggplot(filtered_data6, aes(fill = element, y = values, x = Stage)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.3) +
  geom_hline(aes(yintercept=mean7),linetype = "dashed", color = "black")+
  ggtitle("Na2O") +
  theme(
    plot.title = element_text(size = 14, color = "black", face = "bold", hjust = 0.5),axis.text = element_text(size = 10),axis.title = element_text(size = 12),legend.position = "none",  # Enlever la légende
  ) +
  ylab("Oxide [%]")+
  facet_grid(element ~ ., scales = "free")+
  scale_fill_manual(values = couleurs4)

selected_elements <- c("Ti %")
filtered_data7 <- XRF %>%
  filter(element %in% selected_elements)
mean8 = mean(filtered_data7$values)

TiO2 = ggplot(filtered_data7, aes(fill = element, y = values, x = Stage)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.3) +
  geom_hline(aes(yintercept=mean8),linetype = "dashed", color = "black")+
  ggtitle("TiO2") +
  theme(
    plot.title = element_text(size = 14, color = "black", face = "bold", hjust = 0.5),axis.text = element_text(size = 10),axis.title = element_text(size = 12),legend.position = "none",  # Enlever la légende
  ) +
  ylab("Oxide [%]")+
  facet_grid(element ~ ., scales = "free")+
  scale_fill_manual(values = couleursTI)

selected_elements <- c("P %")
filtered_data8 <- XRF %>%
  filter(element %in% selected_elements)
mean9 = mean(filtered_data8$values)

P2O5 = ggplot(filtered_data8, aes(fill = element, y = values, x = Stage)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.3) +
  geom_hline(aes(yintercept=mean9),linetype = "dashed", color = "black")+
  ggtitle("P2O5") +
  theme(
    plot.title = element_text(size = 14, color = "black", face = "bold", hjust = 0.5),axis.text = element_text(size = 10),axis.title = element_text(size = 12),legend.position = "none",  # Enlever la légende
  ) +
  ylab("Oxide [%]")+
  facet_grid(element ~ ., scales = "free")+
  scale_fill_manual(values = couleursP2)

selected_elements <- c("Mn %")
filtered_data9 <- XRF%>%
  filter(element %in% selected_elements)
mean10 = mean(filtered_data9$values)

MnO = ggplot(filtered_data9, aes(fill = element, y = values, x = Stage)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.3) +
  geom_hline(aes(yintercept=mean10),linetype = "dashed", color = "black")+
  ggtitle("MnO") +
  theme(
    plot.title = element_text(size = 14, color = "black", face = "bold", hjust = 0.5),axis.text = element_text(size = 10),axis.title = element_text(size = 12),legend.position = "none",  # Enlever la légende
  ) +
  ylab("Oxide [%]")+
  facet_grid(element ~ ., scales = "free")+
  scale_fill_manual(values = couleursMN)

pdf('oxydes.pdf', 8.5)
library(ggpubr)
figurefinale <- ggarrange(SiO,Al2O3,CaO,MgO,Fe2O3,Na2O,K2O,TiO2,P2O5,MnO,ncol = 2, nrow = 5)
annotate_figure(figurefinale, top = text_grob("Major elements according to glacier retreat", 
                                              color = "black", face = "bold", size = 24))

dev.off()


## REGRESSION
XRF$Stage=as.factor(XRF$Stage)
#histogramm
hist(XRF$values, 20, las =1, col = 'orange')

### ALL ELEMENT ANALYSIS 
## MODEL 1 - predictor is numeric (years)
mod.flux2 = lm(values ~ poly(Years,2),
               data = XRF)  # y= a+bx+cx2 
mod.flux1 = lm(values ~ Years+element ,
               data = XRF)  # y= a+bx
mod.flux0 = lm(values ~ 1 ,
               data = XRF)  # y= a

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
mod.flux2 = lm(values ~ poly(Stage,2),
               data = XRF)  # y= a+bx+cx2 
mod.flux1 = lm(values ~ Stage+element ,
               data = XRF)  # y= a+bx
mod.flux0 = lm(values ~ 1 ,
               data = XRF)  # y= a

anova(mod.flux0,mod.flux1,mod.flux2) #look at which model has a significant p value
AIC(mod.flux1) #look for a low AIC
AIC(mod.flux2)

# Anova type-II
Anova(mod.flux1) # is years significant (p-value<0.05) ?
summary(mod.flux1) #is each stage significant ?
XRF$Stage <- relevel(XRF$Stage, ref = "1") #relevel and change reference stage
cohens_f(mod.flux1) # effect size index 


#GRAPH XRF
pdf('figure_XRF_lab.pdf', 7,5)
ggplot(XRF, aes(x = Years,
                      y = (values),
                      colour = element)) +
  scale_y_log10()+ 
  geom_jitter(width = 3, alpha=0.6) +
  geom_smooth(method = lm, formula = y ~ x, fill="gray82") +
  theme_classic()+
  ggtitle("Major elements according to glacier retreat")+
  theme(plot.title = element_text(size=14,color="black", face="bold",hjust = 0.5))+
  xlab("Years since deglaciation")+
  ylab("Oxide [%]")+
  theme(axis.text = element_text(size=10), axis.title = element_text(size=12))
dev.off()

