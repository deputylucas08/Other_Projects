library(urca)
library(vars)
library(mFilter)
library(tseries)
library(forecast)
library(tidyverse)
library(zoo)

install.packages("forecast")
install.packages("mFilter")

VARINF <- read.csv(file.choose())

VARINF$loggdp=log(VARINF$gdp)
head(VARINF)

VARINF$logwti=log(VARINF$wti)
head(VARINF)

write_csv(VARINF, file = "VARINFLOG.csv")

VARINFLOG <- read.csv(file.choose())

ir <- ts(VARINFLOG$ir, start = c(1986,1), frequency = 4)
fx <- ts(VARINFLOG$fx, start = c(1986,1), frequency = 4)
cci <- ts(VARINFLOG$cci, start = c(1986,1), frequency = 4)
loggdp <- ts(VARINFLOG$loggdp, start = c(1986,1), frequency = 4)
logwti <- ts(VARINFLOG$logwti, start = c(1986,1), frequency = 4)
cpi <- ts(VARINFLOG$cpi, start = c(1986,1), frequency = 4)
bci <- ts(VARINFLOG$bci, start = c(1986,1), frequency = 4)

attach(VARINFLOG)
par(mfrow=c(3,2))
p1 <- autoplot(cbind(cpi,ir))
p2 <- autoplot(cbind(cpi,fx))
p3 <- autoplot(cbind(cpi,cci))
p4 <- autoplot(cbind(cpi,loggdp))
p5 <- autoplot(cbind(cpi,logwti))
p6 <- autoplot(cbind(cpi,bci))
p1 + p2 +p3 +p4 +p5 +p6
data(VARINFLOG)
autoplot(VARINFLOG)

library(patchwork)

#adf test

adf.test(cpi, k=4)
adf.test(ir, k=4)
adf.test(fx, k=4)
adf.test(cci, k=4)
adf.test(loggdp, k=4)
adf.test(logwti, k=4)
adf.test(bci, k=4)

#difference the series

dcpi <- diff(cpi)
dir <- diff(ir)
dfx <- diff(fx)
dcci <- diff(cci)
dloggdp <- diff(loggdp)
dlogwti <- diff(logwti)
dbci <- diff(bci)

#adf test for differenced variables

adf.test(dcpi, k=4)
adf.test(dir, k=4)
adf.test(dfx, k=4)
adf.test(dcci, k=4)
adf.test(dloggdp, k=4)
adf.test(dlogwti, k=4)
adf.test(dbci, k=4)

library(urca)
install.packages('TSstudio')
library(TSstudio)


b1 <- ggAcf(cpi)+ggtitle("ACF for inflation")
b2 <- ggPacf(cpi)+ggtitle("PACF for inflation")
b1+b2
c1 <- ggAcf(dcpi)+ggtitle("ACF for differenced inflation")
c2 <- ggPacf(dcpi)+ggtitle("PACF for differenced inflation")
c1+c2

#bind level variables for johansen test

bindlevel1 <- cbind(logwti,ir,cci,bci,fx,cpi,loggdp)

#lag selection criteria

lagselect <- VARselect(bindlevel1, lag.max = 6, type = "const")
lagselect$selection

#bind differenced variables
binddiff <- cbind(dlogwti,dir,dcci,dbci,dfx,dcpi,dloggdp)

#lag selection criteria

lagselect2 <- VARselect(binddiff, lag.max = 6, type = "const")
lagselect2$selection

#Johansen test
jtest <- ca.jo(bindlevel1,type = "trace", ecdet= "const", K = 4)
summary(jtest)

#building VAR

ModelVar <- VAR(binddiff, p = 4, type ="const", season = NULL, exog = NULL)
summary(ModelVar)

options(scipen = 100, digits = 4)

#estimate var equation

VARestimate1 <- window(ts.union(dlogwti,dir,dcci,dbci,dfx,dcpi,dloggdp),
                       start = c(1986,1), frequency = 4)
VARcoef<- VAR(y = VARestimate1, p = 4)
VARcoef

#serial correlation

SerialC <- serial.test(ModelVar, lags.pt = 13, type = "PT.asymptotic")
SerialC

#heteroskedasticity

ArchT <- arch.test(ModelVar, lags.multi=13, multivariate.only = TRUE)
ArchT

#normal dist of residuals

NormT <- normality.test(ModelVar, multivariate.only=TRUE)
NormT

#stability test
library(ggplot2)

Stab1 <- stability(ModelVar, type = "OLS-CUSUM")
plot(Stab1)

par("mar")
par(mar=c(1,1,1,1))

#granger

GrangerInf <- causality(ModelVar, cause ="dcpi")
GrangerInf

install.packages("dynlm")
library(dynlm)

VAR_EQ1 <- dynlm(dcpi ~ L(dcpi, 1:4) + L(dlogwti, 1:4))

VAR_EQ2 <- dynlm(dcpi ~ L(dcpi, 1:4) + L(dir, 1:4))

VAR_EQ3 <- dynlm(dcpi ~ L(dcpi, 1:4) + L(dcci, 1:4))

VAR_EQ4 <- dynlm(dcpi ~ L(dcpi, 1:4) + L(dbci, 1:4))

VAR_EQ5 <- dynlm(dcpi ~ L(dcpi, 1:4) + L(dfx, 1:4))

VAR_EQ6 <- dynlm(dcpi ~ L(dcpi, 1:4) + L(dloggdp, 1:4))

VAR_EQ7 <- dynlm(dcpi ~ L(dlogwti, 1:4) + L(dcpi, 1:4))

VAR_EQ8 <- dynlm(dcpi ~ L(dir, 1:4) + L(dcpi, 1:4))

VAR_EQ9 <- dynlm(dcpi ~ L(dcci, 1:4) + L(dcpi, 1:4))

VAR_EQ10 <- dynlm(dcpi ~ L(dbci, 1:4) + L(dcpi, 1:4))

VAR_EQ11 <- dynlm(dcpi ~ L(dfx, 1:4) + L(dcpi, 1:4))

VAR_EQ12 <- dynlm(dcpi ~ L(dloggdp, 1:4) + L(dcpi, 1:4))


names(VAR_EQ1$coefficients) <- c("Intercept","dcpi_t-1", 
                                 "dcpi_t-2","dcpi_t-3","dcpi_t-4",
                                 "dlogwti_t-1", "dlogwti_t-2",
                                 "dlogwti_t-3", "dlogwti_t-4")
names(VAR_EQ2$coefficients) <- c("Intercept","dcpi_t-1", 
                                 "dcpi_t-2","dcpi_t-3","dcpi_t-4",
                                 "dir_t-1", "dir_t-2",
                                 "dir_t-3", "dir_t-4")
names(VAR_EQ3$coefficients) <- c("Intercept","dcpi_t-1", 
                                 "dcpi_t-2","dcpi_t-3","dcpi_t-4",
                                 "dcci_t-1", "dcci_t-2",
                                 "dcci_t-3", "dcci_t-4")
names(VAR_EQ4$coefficients) <- c("Intercept","dcpi_t-1", 
                                 "dcpi_t-2","dcpi_t-3","dcpi_t-4",
                                 "dbci_t-1", "dbci_t-2",
                                 "dbci_t-3", "dbci_t-4")
names(VAR_EQ5$coefficients) <- c("Intercept","dcpi_t-1", 
                                 "dcpi_t-2","dcpi_t-3","dcpi_t-4",
                                 "dfx_t-1", "dfx_t-2",
                                 "dfx_t-3", "dfx_t-4")
names(VAR_EQ6$coefficients) <- c("Intercept","dcpi_t-1", 
                                 "dcpi_t-2","dcpi_t-3","dcpi_t-4",
                                 "dloggdp_t-1", "dloggdp_t-2",
                                 "dloggdp_t-3", "dloggdp_t-4")
names(VAR_EQ7$coefficients) <- c("Intercept",
                                 "dlogwti_t-1", "dlogwti_t-2",
                                 "dlogwti_t-3", "dlogwti_t-4","dcpi_t-1", 
                                 "dcpi_t-2","dcpi_t-3","dcpi_t-4")
names(VAR_EQ8$coefficients) <- c("Intercept","dir_t-1", "dir_t-2",
                                 "dir_t-3", "dir_t-4","dcpi_t-1", 
                                 "dcpi_t-2","dcpi_t-3","dcpi_t-4")
                                 
names(VAR_EQ9$coefficients) <- c("Intercept","dcci_t-1","dcci_t-2",
                                 "dcci_t-3", "dcci_t-4","dcpi_t-1", 
                                 "dcpi_t-2","dcpi_t-3","dcpi_t-4"
                                 )
names(VAR_EQ10$coefficients) <- c("Intercept","dbci_t-1","dbci_t-2",
                                  "dbci_t-3", "dbci_t-4","dcpi_t-1", 
                                 "dcpi_t-2","dcpi_t-3","dcpi_t-4"
                                 )
names(VAR_EQ11$coefficients) <- c("Intercept","dfx_t-1","dfx_t-2",
                                  "dfx_t-3", "dfx_t-4","dcpi_t-1", 
                                 "dcpi_t-2","dcpi_t-3","dcpi_t-4"
                                 )
names(VAR_EQ12$coefficients) <- c("Intercept","dloggdp_t-1","dloggdp_t-2",
                                  "dloggdp_t-3", "dloggdp_t-4","dcpi_t-1", 
                                 "dcpi_t-2","dcpi_t-3","dcpi_t-4"
                                 )

linearHypothesis(VAR_EQ1, 
                 hypothesis.matrix = c("dlogwti_t-1", "dlogwti_t-2",
                                    "dlogwti_t-3", "dlogwti_t-4"), 
                 vcov. = sandwich)
linearHypothesis(VAR_EQ7, 
                 hypothesis.matrix = c("dcpi_t-1", 
                                       "dcpi_t-2","dcpi_t-3","dcpi_t-4"), 
                 vcov. = sandwich)
linearHypothesis(VAR_EQ8, 
                 hypothesis.matrix = c("dcpi_t-1", 
                                       "dcpi_t-2","dcpi_t-3","dcpi_t-4"), 
                 vcov. = sandwich)
linearHypothesis(VAR_EQ9, 
                 hypothesis.matrix = c("dcpi_t-1", 
                                       "dcpi_t-2","dcpi_t-3","dcpi_t-4"), 
                 vcov. = sandwich)
linearHypothesis(VAR_EQ10, 
                 hypothesis.matrix = c("dcpi_t-1", 
                                       "dcpi_t-2","dcpi_t-3","dcpi_t-4"), 
                 vcov. = sandwich)
linearHypothesis(VAR_EQ11, 
                 hypothesis.matrix = c("dcpi_t-1", 
                                       "dcpi_t-2","dcpi_t-3","dcpi_t-4"), 
                 vcov. = sandwich)
linearHypothesis(VAR_EQ12, 
                 hypothesis.matrix = c("dcpi_t-1", 
                                       "dcpi_t-2","dcpi_t-3","dcpi_t-4"), 
                 vcov. = sandwich)

library(car)

#IRFs

IRFOil <- irf(ModelVar, impulse ="dlogwti", response ="dcpi", n.ahead = 20,
              boot = TRUE)
plot(IRFOil,ylab= "Inflation", main = "Oil shock")

IRFinterest <- irf(ModelVar, impulse ="dir", response ="dcpi", n.ahead = 20,
              boot = TRUE)
qw2 <- plot(IRFinterest,ylab= "Inflation", main = "Interest Rate Shock")

IRFcci <- irf(ModelVar, impulse ="dcci", response ="dcpi", n.ahead = 20,
                   boot = TRUE)
qw3 <- plot(IRFcci,ylab= "Inflation", main = "CCI Shock")

IRFbci <- irf(ModelVar, impulse ="dbci", response ="dcpi", n.ahead = 20,
              boot = TRUE)
qw4 <- plot(IRFbci,ylab= "Inflation", main = "BCI Shock")

IRFfx <- irf(ModelVar, impulse ="dfx", response ="dcpi", n.ahead = 20,
              boot = TRUE)
qw5 <- plot(IRFfx,ylab= "Inflation", main = "Exchange rate Shock")



library(survival)
library(UsingR)
install.packages(UsingR)

par(mfrow=c(5,1))
plot(IRFinterest,ylab= "Inflation", main = "Interest Rate Shock")
plot(IRFcci,ylab= "Inflation", main = "CCI Shock")
plot(IRFbci,ylab= "Inflation", main = "BCI Shock")
plot(IRFfx,ylab= "Inflation", main = "Exchange rate Shock")

#VAR forecasting
library(car)

forecasting <- predict(ModelVar, n.ahead = 4, ci=0.95)
fanchart(forecasting, names = "dcpi")

plot(forecasting, names = "dcpi")

f1 <-plot(cpi)
f2 <-plot(dcpi)
f1+f2
