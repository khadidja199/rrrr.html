# rrrr.html
exam
author: "khadidja amadou"
date: "15/03/2021"
output: html_document
---

```{r setup, include=FALSE}
library(knitr)
library(tigerstats)
knitr::opts_chunk$set(echo = TRUE)
```

###telecharger la base de données cigar
data("Cigar")

## la regression linéaire simple de la vente du nombre de paquets de cigarettes par habitant 
##en fonction du prix et du revenu disponible par habitant.
pooling_lm=lm(sales ~ price+ndi,data=Cigar)
summary(pooling_lm)

##la regression sur les données de panel
pooling=plm(sales ~ price+ndi,data=Cigar)
summary(pooling)

##les opérateurs

###   between individuel
between_i=plm(sales ~ price+ ndi,data=Cigar, effect=c("individual"),model=c("between"))
summary(between_i)

##  between temporel
between_t= plm(sales ~ price+ ndi,data=Cigar, effect=c("time"),model=c("between"))
summary(between_t)

##within individuel
within_i= plm(sales ~ price+ ndi,data=Cigar, effect=c("individual"),model=c("within"))
summary(within_i)

##within temporel
within_t= plm(sales ~ price+ ndi,data=Cigar, effect=c("time"),model=c("within"))
summary(within_t)

##within total
within_it=plm(sales ~ price+ ndi,data=Cigar, effect=c("twoways"),model=c("within"))
summary(within_it)

##GLS individuel
GLS_i= plm(sales ~ price+ ndi,data=Cigar, effect=c("individual"),model=c("pooling"))
summary(GLS_i)

##GLS temporel
GLS_t= plm(sales ~ price+ ndi,data=Cigar, effect=c("time"),model=c("pooling"))
summary(GLS_t)

##GLS total
GLS_it= plm(sales ~ price+ ndi,data=Cigar, effect=c("twoways"),model=c("pooling"))
summary(GLS_it)


###Ftest pour les effets temporels
pFtest(sales~price+ndi,data=Cigar,effect="time")
### Ftest pour les effets individuels-temporels
pFtest(sales~price+ndi,data=Cigar,effect="time")
residuals(within_i)
##test de Hausman
phtest(within_i,GLS_it,data=Cigar)


###
pooling=plm(taxs ~ price+inc
