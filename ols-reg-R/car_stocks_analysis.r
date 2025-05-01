#
set.seed(1)
# installing packages
install.packages("car") 
install.packages("tidyverse")
# loading packages
library(readxl) # package: excel data
library(car)    # package: regression and F test
library(stats)  # package: statistical calculation
library(tidyverse)

#load the data 
carsdata1=read_excel("carsdata1.xlsx", 
                        col_types = c("text", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric"))
# running regression
myregtesla=lm(ERTSLA~MKT, data=carsdata1)
myregvolkswagen=lm(ERVOW3~MKT, data=carsdata1)
myregrenault=lm(ERRNO~MKT, data=carsdata1)
myregford=lm(ERFord~MKT, data=carsdata1)
myregkia=lm(ER000270KS~MKT, data=carsdata1)
# running regression with interaction term
myregteslacovid=lm(ERTSLA~MKT+WindowCovid*MKT, data=carsdata1)
myregvolkswagencovid=lm(ERVOW3~MKT+WindowCovid*MKT, data=carsdata1)
myregrenaultcovid=lm(ERRNO~MKT+WindowCovid*MKT, data=carsdata1)
myregfordcovid=lm(ERFord~MKT+WindowCovid*MKT, data=carsdata1)
myregkiacovid=lm(ER000270KS~MKT+WindowCovid*MKT, data=carsdata1)

#summary of regression non-covid vs covid for 5 different stocks
summary(myregtesla)
summary(myregteslacovid)

summary(myregvolkswagen)
summary(myregvolkswagencovid)

summary(myregrenault)
summary(myregrenaultcovid)

summary(myregford)
summary(myregfordcovid)

summary(myregkia)
summary(myregkiacovid)

#producing a scatter plot with expected stock returns on Y axis and market returns on X axis
#to visualize linear relationship between the dependent and independent variables
plot(ERTSLA~MKT, data=carsdata1)
abline(a = 0.25907 , b = 1.34367)
title(main="Tesla stock vs market return")

plot(ERVOW3~MKT, data=carsdata1)
abline(a =0.02271, b = 0.75675)
title(main="Volkswagen stock vs market return")

plot(ERRNO~MKT, data=carsdata1)
abline(a =-0.11250 , b = 0.98559)
title(main="Renault stock vs market return")

plot(ERFord~MKT, data=carsdata1)
abline(a =0.00533 , b = 1.05770)
title(main="Ford stock vs market return")

plot(ER000270KS~MKT, data=carsdata1)
abline(a =0.07869, b = 0.11962)
title(main="Kia stock vs market return")

errorstesla=residuals(myregtesla)

plot(errorstesla)

hist(errorstesla, breaks=50, density=70, col="blue")

install.packages("moments")
library(moments)

#checking for kurtosis, skewness variance and mean of our distributions of errors
kurtosis(errorstesla)
skewness(errorstesla)
var(errorstesla)
mean(errorstesla)

errorsvolkswagen=residuals(myregvolkswagen)

plot(errorsvolkswagen)

hist(errorsvolkswagen, breaks=50, density=70, col="red")

kurtosis(errorsvolkswagen)
skewness(errorsvolkswagen)
var(errorsvolkswagen)
mean(errorsvolkswagen)

errorsrenault=residuals(myregrenault)

plot(errorsrenault)

hist(errorsrenault, breaks=50, density=70, col="green")

kurtosis(errorsrenault)
skewness(errorsrenault)
var(errorsrenault)
mean(errorsrenault)

errorsford=residuals(myregford)

plot(errorsford)

hist(errorsford, breaks=50, density=70, col="orange")

kurtosis(errorsford)
skewness(errorsford)
var(errorsford)
mean(errorsford)

errorskia=residuals(myregkia)

plot(errorskia)

hist(errorskia, breaks=50, density=70, col="purple")

kurtosis(errorskia)
skewness(errorskia)
var(errorskia)
mean(errorskia)

linearHypothesis(myregteslacovid, c("WindowCovid", "MKT:WindowCovid"))

#install texreg package to export regression data
install.packages("texreg")
library(texreg)

#expor regressions to a word file in order to present results in an orderly way
htmlreg(list(myregtesla,myregvolkswagen,myregrenault,myregford,myregkia), file="regsum.doc",
  caption="Summary of all regressions",
  caption.above=TRUE, custom.model.names=c("TSLA","VOL","RNO","Ford","KIA"), digits=3)

htmlreg(list(myregteslacovid,myregvolkswagencovid,myregrenaultcovid,myregfordcovid,myregkiacovid), file="regsumcov.doc",
        caption="Summary of all regressions incl. Covid-19 factor",
        caption.above=TRUE, custom.model.names=c("TSLA","VOL","RNO","Ford","KIA"), digits=3)

#install package for bp test
install.packages("lmtest")

#load lmtest library
library(lmtest)

#running Breusch-pagan tests to check for heteroskedasticity
bptest(myregtesla, varformula = NULL, studentize = TRUE, data = carsdata1)
bptest(myregvolkswagen, varformula = NULL, studentize = TRUE, data = carsdata1)
bptest(myregrenault, varformula = NULL, studentize = TRUE, data = carsdata1)
bptest(myregford, varformula = NULL, studentize = TRUE, data = carsdata1)
bptest(myregkia, varformula = NULL, studentize = TRUE, data = carsdata1)





