#import packages
library(foreign)
library(lubridate)
library(tidyverse)
library(plm)
library(zoo)
library(texreg)
library(lmtest)

#read data
guns<-read_dta("/Users/sahanaa/Downloads/guns.dta")
head(guns)
summary(guns)
str(guns)

#correlation matrix
cormat <- round(cor(guns),2)
head(cormat)

#column names
names(guns)

#Panel data models for violence, robbery and murder
guns_panel<-pdata.frame(guns,index = c("stateid","year"))


form=log(vio)~log(incarc_rate)+pb1064+pw1064+pm1029+log(pop)+log(avginc)+log(density)+shall
form_year=log(vio)~log(incarc_rate)+pb1064+pw1064+pm1029+log(pop)+log(avginc)+log(density)+shall+factor(year)


form1=log(rob)~log(incarc_rate)+pb1064+pw1064+pm1029+log(pop)+log(avginc)+log(density)+shall
form1_year=log(rob)~log(incarc_rate)+pb1064+pw1064+pm1029+log(pop)+log(avginc)+log(density)+shall+factor(year)

form2=log(mur)~log(incarc_rate)+pb1064+pw1064+pm1029+log(pop)+log(avginc)+log(density)+shall
form2_year=log(mur)~log(incarc_rate)+pb1064+pw1064+pm1029+log(pop)+log(avginc)+log(density)+shall+factor(year)

options(digits=3,scipen=0)


#Pooled OLS model
plmpooled <- plm(form2, data=guns_panel, model = "pooling",)
summary(plmpooled, vcov=function(x) vcovHC(x, method="arellano",
                                           type="HC1", cluster="group"), digits = max(3L, getOption("digits") - 3L))
Pooled_OLS_CRE<-coeftest(plmpooled, vcov=function(x) vcovHC(x, method="arellano",
                                                            type="HC1", cluster="group"))

#Fixed Effect Model
plmwithin <- plm(form2, data=guns_panel, model = "within")
summary(plmwithin, vcov=function(x) vcovHC(x, method="arellano",
                                           type="HC1", cluster="group"))

Entity_FE_CRE<-coeftest(plmwithin, vcov=function(x) vcovHC(x, method="arellano",
                                                           type="HC1", cluster="group"))

#Fixed Two way Effect model

plmTimeWithin <- plm(form, data=guns_panel, model = "within", effect="twoways")

summary(plmTimeWithin, ,vcov=function(x) vcovHC(x, method="arellano",
                                                type="HC1", cluster="group"))

Time_Entity_FE_CRE<-coeftest(plmTimeWithin, vcov=function(x) vcovHC(x, method="arellano",
                                                                    type="HC1", cluster="group"))

summary(fixef(plmTimeWithin, effect = "time",vcov=function(x) vcovHC(x, method="arellano",
                                                             type="HC1", cluster="group")))

#Ftest for Time variables
pFtest(plmTimeWithin,plmwithin)

#Random Effect Model
plmrandom <- plm(form2, data = guns_panel, model = "random")
summary(plmrandom)


#hausman test
phtest(plmwithin, plmrandom)


#comparison of Fixed effects and Pooped OLS
screenreg(list(Pooled_OLS = Pooled_OLS_CRE,Entity_FE=Entity_FE_CRE, Entity_Time_FE=Time_Entity_FE_CRE),single.row=TRUE,
          digits = 3, omit.coef = "(Intercept)")

#plots
plotreg(Time_Entity_FE_CRE,signif.light="#66FF66",signif.medium="#339933",signif.dark="#339900",cex = 1.5,lwd.zerobar = 2.5, lwd.inner = 3, lwd.outer = 1, insignif.light = "#fbc9b9", insignif.medium = "#f7523a", insignif.dark = "#bd0017",custom.model.names ="Entity Time FE")
plotreg(Entity_FE_CRE,signif.light="#66FF66",signif.medium="#339933",signif.dark="#339900",cex = 1.5,lwd.zerobar = 2.5, lwd.inner = 3, lwd.outer = 1, insignif.light = "#fbc9b9", insignif.medium = "#f7523a", insignif.dark = "#bd0017",custom.model.names ="Entity FE")
plotreg(Pooled_OLS_CRE,signif.light="#66FF66",signif.medium="#339933",signif.dark="#339900",cex = 1.5,lwd.zerobar = 2.5, lwd.inner = 3, lwd.outer = 1, insignif.light = "#fbc9b9", insignif.medium = "#f7523a", insignif.dark = "#bd0017",custom.model.names ="Pooled OLS", omit.coef = "(Intercept)")


