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

## LOAD DATA
excel_sheets("FullData.xlsx")
ICP<-read_excel("FullData.xlsx",sheet = "Soil")

#select data
ICP<-ICP[c(2:17),c(1:3,16:22)]
is.numeric(ICP) #be sure it's numeric
sapply(ICP, is.numeric)
names(ICP)[names(ICP) == "Al(available nutrients)"] <- "Al(nut)" #rename the column
ICP$`Al(nut)`=as.numeric(ICP$`Al(nut)`)
ICP$Stage=as.factor(ICP$Stage)

#HISTOGRAMS
hist(ICP$`Al(nut)`, 10, las =1, col = 'orange') #histogram 
hist(ICP$`Ca(nut)`, 20, las =1, col = 'orange')
hist(ICP$`K(nut)`, 20, las =1, col = 'orange')
hist(ICP$`Mg(nut)`,20, las =1, col = 'orange')
hist(ICP$`Mn(nut)`, 10, las =1, col = 'orange')
hist(ICP$`Na(nut)`, 20, las =1, col = 'orange')
hist(ICP$`P(nut)`, 20, las =1, col = 'orange')

### EACH ELEMENT ANALYSIS (Here with Al then change)
## MODEL 1 - predictor is numeric (years)
mod.flux2 = lm(`Al(nut)` ~ poly(Years,2),
               data = ICP)  # y= a+bx+cx2 
mod.flux1 = lm(`Al(nut)` ~ Years ,
               data = ICP)  # y= a+bx
mod.flux0 = lm(`Al(nut)` ~ 1 ,
               data = ICP)  # y= a

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
mod.flux2 = lm(`Al(nut)` ~ poly(Stage,2),
               data = ICP)  # y= a+bx+cx2 
mod.flux1 = lm(`Al(nut)` ~ Stage ,
               data = ICP)  # y= a+bx
mod.flux0 = lm(`Al(nut)` ~ 1 ,
               data = ICP)  # y= a

anova(mod.flux0,mod.flux1,mod.flux2) #look at which model has a significant p value
AIC(mod.flux1) #look for a low AIC
AIC(mod.flux2)

# Anova type-II
Anova(mod.flux1) # is years significant (p-value<0.05) ?
summary(mod.flux1) #is each stage significant ?
ICP$Stage <- relevel(ICP$Stage, ref = "1") #relevel and change reference stage
cohens_f(mod.flux1) # effect size index 


## ALL ELEMENTS ANALYSIS
#arranging the table
long_ICP <- ICP %>%
  select(Years, Stage, Plot, `Al(nut)`, `Ca(nut)`,`K(nut)` , `Mg(nut)` , `Mn(nut)`,`Na(nut)`,`P(nut)`) %>%
  pivot_longer(cols = c(`Al(nut)`, `Ca(nut)`,`K(nut)` , `Mg(nut)` , `Mn(nut)`,`Na(nut)`,`P(nut)`), 
               names_to = "Element", 
               values_to = "Percentage")

sapply(long_ICP,is.numeric)

#histogram 
hist(long_ICP$Percentage, 10, las =1, col = 'orange')

#test if it depends on years but also on Element
mod.flux1 = lm(long_ICP$Percentage ~ Years+Element ,
               data = long_ICP)  
anova(mod.flux1)
summary(mod.flux1)


## plot all elements
pdf('figure_ICP.pdf', 7,5)
ggplot(long_ICP, aes(x = Years,
                y = (Percentage),
                colour = Element)) +
  geom_jitter(width = 3,alpha=0.6) +
  geom_smooth(method = lm, formula = y ~ x,fill="gray88") +
  scale_y_log10()+    #add log scale
  theme_bw()+
  ggtitle("Available elements according to glacier retreat")+
  xlab("Years since deglaciation")+ ylab("Concentration [mg/kg soil]")+
  scale_fill_discrete(breaks = unique(long_ICP$Percentage)) +
  theme(plot.title = element_text(size=14,color="black", face="bold",hjust = 0.5))+
  theme(axis.text = element_text(size=10), axis.title = element_text(size=12))+
  theme(legend.box = "horizontal", legend.background = element_rect(fill="white",linetype="solid", colour="black", size=0.3), legend.text=element_text(size=10)) +
  labs(colour="Elements")

dev.off()

## boxplot
#colors
couleurs1 = c("orangered")
couleurs2 = c("darkcyan")
couleurs3 = c("tan1")
couleurs4 = c("darkseagreen")
couleurs5 = c("palevioletred")
couleurs6 = c("steelblue")
couleurs7 = c("blue")

selected_elements <- c("Al(nut)")
filtered_data <- long_ICP %>%
  filter(Element %in% selected_elements)
mean1 = mean(filtered_data$Percentage)
Al = ggplot(filtered_data, aes(fill = Element, y = Percentage, x = Stage)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.3) +
  geom_hline(aes(yintercept=mean1),linetype = "dashed", color = "black")+
  ggtitle("Al") +
  theme(
    plot.title = element_text(size = 14, color = "black", face = "bold", hjust = 0.5),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    legend.position = "none",  
  ) +
  ylab("[mg/kg soil]")+
  facet_grid(Element ~ ., scales = "free")+
  scale_fill_manual(values = couleurs1)

selected_elements <- c("Ca(nut)")
filtered_data <- long_ICP %>%
  filter(Element %in% selected_elements)
mean2 = mean(filtered_data$Percentage)
Ca = ggplot(filtered_data, aes(fill = Element, y = Percentage, x = Stage)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.3) +
  geom_hline(aes(yintercept=mean2),linetype = "dashed", color = "black")+
  ggtitle("Ca") +
  theme(
    plot.title = element_text(size = 14, color = "black", face = "bold", hjust = 0.5),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    legend.position = "none",  
  ) +
  ylab("[mg/kg soil]")+
  facet_grid(Element ~ ., scales = "free")+
  scale_fill_manual(values = couleurs2)

selected_elements <- c("K(nut)")
filtered_data <- long_ICP %>%
  filter(Element %in% selected_elements)
mean3 = mean(filtered_data$Percentage)
K = ggplot(filtered_data, aes(fill = Element, y = Percentage, x = Stage)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.3) +
  geom_hline(aes(yintercept=mean3),linetype = "dashed", color = "black")+
  ggtitle("K") +
  theme(
    plot.title = element_text(size = 14, color = "black", face = "bold", hjust = 0.5),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    legend.position = "none",  
  ) +
  ylab("[mg/kg soil]")+
  facet_grid(Element ~ ., scales = "free")+
  scale_fill_manual(values = couleurs3)

selected_elements <- c("Mg(nut)")
filtered_data <- long_ICP %>%
  filter(Element %in% selected_elements)
mean4 = mean(filtered_data$Percentage)
Mg = ggplot(filtered_data, aes(fill = Element, y = Percentage, x = Stage)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.3) +
  geom_hline(aes(yintercept=mean4),linetype = "dashed", color = "black")+
  ggtitle("Mg") +
  theme(
    plot.title = element_text(size = 14, color = "black", face = "bold", hjust = 0.5),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    legend.position = "none",  
  ) +
  ylab("[mg/kg soil]")+
  facet_grid(Element ~ ., scales = "free")+
  scale_fill_manual(values = couleurs4)

selected_elements <- c("Mn(nut)")
filtered_data <- long_ICP %>%
  filter(Element %in% selected_elements)
mean5 = mean(filtered_data$Percentage)
Mn = ggplot(filtered_data, aes(fill = Element, y = Percentage, x = Stage)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.3) +
  geom_hline(aes(yintercept=mean5),linetype = "dashed", color = "black")+
  ggtitle("Mn") +
  theme(
    plot.title = element_text(size = 14, color = "black", face = "bold", hjust = 0.5),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    legend.position = "none",  
  ) +
  ylab("[mg/kg soil]")+
  facet_grid(Element ~ ., scales = "free")+
  scale_fill_manual(values = couleurs5)

selected_elements <- c("Na(nut)")
filtered_data <- long_ICP %>%
  filter(Element %in% selected_elements)
mean6 = mean(filtered_data$Percentage)
Na = ggplot(filtered_data, aes(fill = Element, y = Percentage, x = Stage)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.3) +
  geom_hline(aes(yintercept=mean6),linetype = "dashed", color = "black")+
  ggtitle("Na") +
  theme(
    plot.title = element_text(size = 14, color = "black", face = "bold", hjust = 0.5),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    legend.position = "none",  
  ) +
  ylab("[mg/kg soil]")+
  facet_grid(Element ~ ., scales = "free")+
  scale_fill_manual(values = couleurs6)

selected_elements <- c("P(nut)")
filtered_data <- long_ICP %>%
  filter(Element %in% selected_elements)
mean7 = mean(filtered_data$Percentage)
P = ggplot(filtered_data, aes(fill = Element, y = Percentage, x = Stage)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.3) +
  geom_hline(aes(yintercept=mean7),linetype = "dashed", color = "black")+
  ggtitle("P") +
  theme(
    plot.title = element_text(size = 14, color = "black", face = "bold", hjust = 0.5),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    legend.position = "none",  
  ) +
  ylab("[mg/kg soil]")+
  facet_grid(Element ~ ., scales = "free")+
  scale_fill_manual(values = couleurs7)

pdf('elements.pdf', 8.5)
library(ggpubr)
figurefinale <- ggarrange(Al,Ca,K,Mg,Mn,Na,P,ncol = 2, nrow = 4)
annotate_figure(figurefinale, top = text_grob("Available elements according to glacier retreat", 
                                              color = "black", face = "bold", size = 24))

dev.off()
