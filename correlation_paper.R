setwd("/Users/cecilecharles/Desktop/paper/code")

library(corrplot)
library(readxl)
library(dplyr)

## LOAD DATA
excel_sheets("FullData.xlsx")
pf2<-read_excel("FullData.xlsx",sheet = "Plants")
pf3<-read_excel("FullData.xlsx",sheet = "Soil")

# select datas on plant sheet
pf2<-pf2[,c(4:11)]
# select data on soil sheet 
pf3<-pf3[c(2:17),c(1,11:14,16:32,35:37)]

pf <- cbind(pf2,pf3)
pf<-as.data.frame(pf)

#rename columns
pf <- pf %>%
  rename(CaO = `Ca(el)`)
pf <- pf %>%
  rename(K2O = `K(el)`)
pf <- pf %>%
  rename(MgO = `Mg(el)`)
pf <- pf %>%
  rename(Na2O = `Na(el)`)
pf <- pf %>%
  rename(P2O5 = `P(el)`)
pf <- pf %>%
  rename(MnO = `Mn(el)`)
pf <- pf %>%
  rename(Al2O3 = `Al (maj.element)`)
pf <- pf %>%
  rename(SiO2 = Si)
pf <- pf %>%
  rename(TiO2 = Ti)
pf <- pf %>%
  rename(Fe2O3 = Fe)
pf <- pf %>%
  rename(`Ca(av.)` = `Ca(nut)`)
pf <- pf %>%
  rename(`K(av.)` = `K(nut)`)
pf <- pf %>%
  rename(`Mg(av.)` = `Mg(nut)`)
pf <- pf %>%
  rename(`Mn(av.)` = `Mn(nut)`)
pf <- pf %>%
  rename(`Na(av.)` = `Na(nut)`)
pf <- pf %>%
  rename(`P(av.)` = `P(nut)`)
pf <- pf %>%
  rename(`Al(av.)` = `Al(available nutrients)`)
pf <- pf %>%
  rename(ReactionValue = pH_landolt)

is.numeric(pf)
sapply(pf, is.numeric)
pf$Years<-as.numeric(pf$Years)
pf$`Al2O3`<-as.numeric(pf$`Al2O3`)
pf$`Al(av.)`<-as.numeric(pf$`Al(av.)`)
pf$SAND<-as.numeric(pf$SAND)
pf$SILT<-as.numeric(pf$SILT)
pf$CLAY<-as.numeric(pf$CLAY)

# Calculation of the pearson correlation matrix
correlation_matrix <- cor(pf)
print(correlation_matrix)

#correlogram
pdf("correlation_vf.pdf", 12)
corrplot(correlation_matrix, type = "upper", order = "original",  tl.cex = 0.5,
         col=colorRampPalette(c("navyblue", "white", "firebrick3"))(100),
         tl.col = "black", tl.srt = 45,addCoef.col = 1,number.cex = 0.4)
dev.off()
