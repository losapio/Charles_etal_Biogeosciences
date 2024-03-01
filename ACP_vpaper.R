setwd("/Users/cecilecharles/Desktop/soil data")

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

library(vegan)
library(lavaan)
library(FactoMineR)

library(performance)
library(lavaanPlot)
library(ellipse)
library(factoextra)

## Etape 1 - importer donnees puis les sélectionner
excel_sheets("FullData.xlsx")
df2<-read_excel("FullData.xlsx",sheet = "Plants")
df3<-read_excel("FullData.xlsx",sheet = "Soil")

#on prend toutes les variables 

# sélectionner les données de diversité et pH_landolt pour la feuille Plants
df2a<-df2[,c(1,2,4:11)]
names(df2a)[names(df2a) == "DIVERSITY"] <- "Plant diversity" #rename the column
names(df2a)[names(df2a) == "RICHNESS"] <- "Richness" #rename the column
names(df2a)[names(df2a) == "pH_landolt_pond"] <- "Reaction value" #rename the column
names(df2a)[names(df2a) == "subshrub [%]"] <- "dwarfshrub [%]" #rename the column

# sélectionner les données de Ca,Al,P,K, Corg,C/N pH pour la feuille soil
df3<-df3[c(2:17),c(11:13,14,16:32,35:37)]
names(df3)[names(df3) == "Corg  (%)"] <- "Organic carbon" #rename the column
names(df3)[names(df3) == "Ntot  (%)"] <- "Total nitrogen" #rename the column
names(df3)[names(df3) == "pH (H2O)"] <- "pH" #rename the column
names(df3)[names(df3) == "Al (maj.element)"] <- "Al2O3" #rename the column
names(df3)[names(df3) == "Ca(el)"] <- "CaO" #rename the column
names(df3)[names(df3) == "K(el)"] <- "K2O" #rename the column
names(df3)[names(df3) == "Mg(el)"] <- "MgO" #rename the column
names(df3)[names(df3) == "Mn(el)"] <- "MnO" #rename the column
names(df3)[names(df3) == "Fe"] <- "Fe2O3" #rename the column
names(df3)[names(df3) == "Na(el)"] <- "Na2O" #rename the column
names(df3)[names(df3) == "P(el)"] <- "P2O5" #rename the column
names(df3)[names(df3) == "Si"] <- "SiO2" #rename the column
names(df3)[names(df3) == "Ti"] <- "TiO2" #rename the column
names(df3)[names(df3) == "Al(available nutrients)"] <- "Al(av.)" #rename the column
names(df3)[names(df3) == "Ca(nut)"] <- "Ca(av.)" #rename the column
names(df3)[names(df3) == "K(nut)"] <- "K(av.)" #rename the column
names(df3)[names(df3) == "Mg(nut)"] <- "Mg(av.)" #rename the column
names(df3)[names(df3) == "Mn(nut)"] <- "Mn(av.)" #rename the column
names(df3)[names(df3) == "Na(nut)"] <- "Na(av.)" #rename the column
names(df3)[names(df3) == "P(nut)"] <- "P(av.)" #rename the column
names(df3)[names(df3) == "SILT"] <- "Silt" #rename the column
names(df3)[names(df3) == "SAND"] <- "Sand" #rename the column
names(df3)[names(df3) == "CLAY"] <- "Clay" #rename the column

#regrouper les tableaux
df <- cbind(df2a,df3)
df<-as.data.frame(df)

## Etape 2 - normaliser les données
is.numeric(df)
sapply(df, is.numeric)
df$`Al(av.)`<-as.numeric(df$`Al(av.)`)#convertir en numeric
df$`Al2O3`<-as.numeric(df$`Al2O3`)
df_std<-as.matrix(scale(df[])) #standardiser les donnes
head(df_std)
round(apply(df_std, 2, mean)) #moyenne
round(apply(df_std, 2, sd)) #standard deviation

##PCA
#add years and stages as quanti and quali
pca_00<-PCA(df_std, scale.unit = TRUE, ncp = 5, ind.sup = NULL,
    quanti.sup=1, quali.sup=2, graph=TRUE, axes = c(1,2))



#autre représentation plus lisible
pdf('figure_PCA_paper.pdf',8.5)
fviz_pca_var(pca_00, col.var="cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, alpha.var = "contrib",arrowsize=0.4,labelsize=3,
             ggtheme = theme_minimal() + 
               theme(plot.title = element_text(hjust = 0.5)),
             title="PCA graph of variables") # Avoid text overlapping

dev.off()

#plus
eigentable = round(pca_00$var$contrib[,1:2],2)

write.csv(eigentable, "eigentable_paper.csv")
