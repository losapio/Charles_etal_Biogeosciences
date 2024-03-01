setwd("/Users/cecilecharles/Desktop/paper/code")

## LOAD data
library(readxl)
## LOAD DATA
excel_sheets("FullData.xlsx")
Granulo<-read_excel("FullData.xlsx",sheet = "Full")

#select data
Granulo<-Granulo[,c(1:3,42:44)]

##analysis of texture with triangle
#need packages
library(aqp)
library(soilDB)
library(soiltexture)
library(scales)

# adjust names for plotting with TT.plot()
# names must be SAND, SILT, CLAY
names(Granulo)<-toupper(names(Granulo))

# test of bogus data
Granulo$sum<-rowSums(Granulo[,c('SAND', 'SILT', 'CLAY')])

# > 5% deviation from 100%
idex <- which(abs(Granulo$sum - 100) > 5)

# check errors
Granulo[idex, ]

# check: OK
head(Granulo)

# split by generalized horizon name into a list of data.frames
granulo.list<-split(Granulo,Granulo$STAGE)

record.count<-rev(sapply(granulo.list,nrow))

par(mar = c(4.5, 3, 3, 1))
dotchart(record.count, pch = 21, bg = 'royalblue', pt.cex = 1.5, xlab = 'Number of records', main = 'Loafercreek\nRecords per Generalized Horizon Label')


### texture triangle
TT.plot(
  class.sys = "USDA-NCSS.TT",    # use "our" texture triangle
  main = 'USDA-NRCS / NCSS',          # title
  cex.lab = 0.75,                 # scaling of label text
  cex.axis = 0.75,                # scaling of axis
  frame.bg.col = 'white',         # background color
  class.lab.col = 'black',        # color for texture class labels
  lwd.axis = 1.5,
  lwd.lab = 2,
  arrows.show=TRUE
)

#save figure
pdf('figure_Granulo.pdf', 6,5) 

#init figure and save geometry
TT <- soiltexture::TT.plot(
  class.sys = "USDA-NCSS.TT",    # use "our" texture triangle
  main = 'Texture\n16 plots',          # title
  tri.sum.tst = FALSE,            # do not test for exact sum(sand, silt, clay) == 100
  cex.lab = 0.75,                 # scaling of label text
  cex.axis = 0.75,                # scaling of axis
  frame.bg.col = 'white',         # background color
  class.lab.col = 'black',        # color for texture class labels
  lwd.axis = 1.5,
  lwd.lab = 2,
  arrows.show=TRUE
)

#convert in data.frame
Granulo<-data.frame(Granulo)

# add data
# screen coordinates are available for later use
#add all points
xy <- TT.points(tri.data = Granulo, geo = TT, tri.sum.tst = FALSE,tri.pos.tst = FALSE, cex=0.5, col=alpha('blue', 0.5))

dev.off()


#init figure and save geometry
pdf('figure_Granulometry.pdf', 6,5) 
TT <- soiltexture::TT.plot(
  class.sys = "USDA-NCSS.TT",    # use "our" texture triangle
  main = 'Soil texture\ according to stages',          # title
  tri.sum.tst = FALSE,            # do not test for exact sum(sand, silt, clay) == 100
  cex.lab = 0.75,                 # scaling of label text
  cex.axis = 0.75,                # scaling of axis
  frame.bg.col = 'white',         # background color
  class.lab.col = 'black',        # color for texture class labels
  lwd.axis = 1.5,
  lwd.lab = 2,
  arrows.show=TRUE,
)      

# Select Rows by list of names
Stage1<-Granulo[1:4,]
xy <- TT.points(tri.data = Stage1, geo = TT, tri.sum.tst = FALSE,tri.pos.tst = FALSE, cex=0.5, col=alpha('tomato2', 0.8))
Stage2<-Granulo[5:8,]   
xy <- TT.points(tri.data = Stage2, geo = TT, tri.sum.tst = FALSE,tri.pos.tst = FALSE, cex=0.5, col=alpha('olivedrab3', 0.8))
Stage3<-Granulo[9:12,]
xy <- TT.points(tri.data = Stage3, geo = TT, tri.sum.tst = FALSE,tri.pos.tst = FALSE, cex=0.5, col=alpha('cyan3', 0.8))
Stage4<-Granulo[13:16,]
xy <- TT.points(tri.data = Stage4, geo = TT, tri.sum.tst = FALSE,tri.pos.tst = FALSE, cex=0.5, col=alpha('orchid2', 0.8))

legend(x = 0.01, y = 80, legend = c("Stage 1", "Stage 2", "Stage 3", "Stage 4"),
       col = c(alpha('tomato2', 0.8), alpha('olivedrab3', 0.8), alpha('cyan3', 0.8), alpha('orchid2', 0.8)),
       pch = 16, cex = 0.6, text.font = 1)

dev.off()

#----------------------------------------- 
## REGRESSION
is.numeric(Granulo)
sapply(Granulo, is.numeric)
Granulo$STAGE<-as.factor(Granulo$STAGE)

## MODEL 1 - predictor is numeric (years)
mod.flux2 = lm(SILT ~ poly(YEARS,2),
               data = Granulo)  # y= a+bx+cx2 
mod.flux1 = lm(SILT ~ YEARS ,
               data = Granulo)  # y= a+bx
mod.flux0 = lm(SILT ~ 1 ,
               data = Granulo)  # y= a

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
mod.flux2 = lm(SILT ~ poly(YEARS,2),
               data = Granulo)  # y= a+bx+cx2 
mod.flux1 = lm(SILT ~ YEARS ,
               data = Granulo)  # y= a+bx
mod.flux0 = lm(SILT ~ 1 ,
               data = Granulo)  # y= a

anova(mod.flux0,mod.flux1,mod.flux2) #look at which model has a significant p value
AIC(mod.flux1) #look for a low AIC
AIC(mod.flux2)

# Anova type-II
Anova(mod.flux1) # is years significant (p-value<0.05) ?
summary(mod.flux1) #is each stage significant ?
cohens_f(mod.flux1) # effect size index 

#plot
couleurs2<-c("tomato2","olivedrab3","cyan2","orchid3")
Granulo$ordered_years <- factor(Granulo$YEARS, levels = c("23", "60", "110", "142"))

pdf('boxplot_SILT_years.pdf', 7,5)
ggplot(Granulo, aes(x = ordered_years, y = SILT, color=STAGE)) +
  geom_boxplot() +
  labs(x = "Years", y = "Silt [%]") +
  theme_gray()+
  scale_color_manual(values = couleurs2) +  # Utilisez les couleurs personnalisées
  ggtitle("Silt according to glacier retreat")+
  theme(plot.title = element_text(size=14,color="black", face="bold",hjust = 0.5))+ #theme pour le titre
  theme(axis.text = element_text(size=10), axis.title = element_text(size=12))+
  guides(color = FALSE)

dev.off()