# importer les packages

library(dplyr)
library(plm)

# importer la base de donner
options(scipen=999)
setwd("C:/Users/Surface/Downloads")
Donnees=read.csv("pannel2.csv",header = T, sep= ";",dec=",")
head(Donnees,13)
View(Donnees)

# statistique descriptive

stata_descript=Donnees%>%
  group_by(Country)%>%
  summarise(count= n(),
            min=min(PIB),
            max=max(PIB),
            median=median(PIB),
            mean=mean(PIB),
            variance=var(PIB),
            sd=sd(PIB))

round(print(stata_descript,n=32),2)

#creation de la base panel en R

pdata=pdata.frame(Donnees,index=c("Country","Year"))

head(pdata,13)

# Analyse de l'effet temps et de l'effet individuel

#HO: no significant individual and time effect

#Lagrange Multiplier test - two-ways effects
#(Gourieroux, holly and monfort)

plmtest(PIB~Chomage+Inflation,data=pdata, effect="twoways", type="ghm")

#Lagrange Multiplier test - (king and wu)

plmtest(PIB~Chomage+Inflation,data=pdata, mode1="twoways", type="kw")

#analyse les effets individiuels

plmtest(PIB~Chomage+Inflation,data=pdata, effect="individual", type="kw")

#test for timing effect

plmtest(PIB~Chomage+Inflation,data=pdata, mode1="time", type="kw")

#controlling time dimension

modT=lm(PIB~Chomage+Inflation + factor(Year)-1,data=pdata)
summary(modT)

#least squares Dummy Variable Estimation
#(controling for indivual effect)

#pour voir l'effet du chomage et de l'inflation sur chaque pays

fe_model_dumy=lm(PIB~Chomage+Inflation + factor(Country)-1,data=pdata)
summary(fe_model_dumy)

#Controle des deux effects/Two ways method

fixed_Two=plm(PIB~Chomage+Inflation,data=pdata,model = "within",effect="twoways")
summary(fixed_Two)