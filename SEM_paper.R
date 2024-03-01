setwd("/Users/cecilecharles/Desktop/paper/code")

library(lme4)
library(lmerTest)
library(emmeans)
library(ggplot2)
library(readxl)
library(dplyr)
library(vegan)
library(lavaan)
library(performance)
library(lavaanPlot)

## STEP 1 - import and select datas
excel_sheets("FullData.xlsx")
df2<-read_excel("FullData.xlsx",sheet = "Plants")
df3<-read_excel("FullData.xlsx",sheet = "Soil")

df2<-df2[,c(1,5)] #years and diversity
df3<-df3[c(2:17),c(11,14)] # carbon and pH

df <- cbind(df2,df3)#group data
df<-as.data.frame(df)

# name datas
pH<- scale(df$`pH (H2O)`)
Carbon<-scale(df$`Corg  (%)`)
Diversity<-scale(df$DIVERSITY)
Years<-scale(df2b$Years)

mytable <- data.frame(pH,Carbon,Diversity,Years) 
is.numeric(mytable)
sapply(mytable, is.numeric)

# Specify path model
model0<-
  '
  # regression
    Carbon~pH+Diversity+Years #carbon is explained by glacier retreat + vegetation diversity
    
    Diversity~Years #vegetation diversity is explained by years since deglaciation 
  
    pH~ Years # pH is explained by glacier retreat

'

fit0 <- sem(model0, data = mytable)# Estimate model
summary(fit0, standardized = TRUE)
model_performance(fit0)

labels <- list(Years="Glacier retreat", Carbon="Organic carbon", Diversity= "Plant diversity")
pl<-lavaanPlot(model = fit0,labels=labels,
               node_options = list(shape = "box", fontname = "Helvetica"),
               edge_options = list(color = "grey"), coefs = TRUE)

embed_plot_pdf(pl, "SEM.pdf")
