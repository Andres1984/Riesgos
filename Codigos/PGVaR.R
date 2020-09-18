###### Cálculo de VaR

library(quantmod)
getSymbols("PG",src="yahoo",from="2019-08-19", to="2020-08-18")

pg=Delt(PG$PG.Close)[-1]


### VaR diario Primer método Paramétrico

mpg=mean(pg)
sdpg=sd(pg)
VaR99=mpg+qnorm(0.01)*sdpg
VaR95=mpg+qnorm(0.05)*sdpg
VaR90=mpg+qnorm(0.1)*sdpg


VaR99*10000
VaR95*10000
VaR90*10000


### VaR mensual Primer método Paramétrico

VaR99m=mpg*20+qnorm(0.01)*sdpg*sqrt(20)
VaR95m=mpg*20+qnorm(0.05)*sdpg*sqrt(20)
VaR90m=mpg*20+qnorm(0.1)*sdpg*sqrt(20)


VaR99m*10000
VaR95m*10000
VaR90m*10000



### VaR anual Primer método

VaR99a=mpg*250+qnorm(0.01)*sdpg*sqrt(250)
VaR95a=mpg*250+qnorm(0.05)*sdpg*sqrt(250)
VaR90a=mpg*250+qnorm(0.1)*sdpg*sqrt(250)


VaR99a*10000
VaR95a*10000
VaR90a*10000




#### Segundo método Histórico no Paramétrico


# Proceso diario
VaR=quantile(pg,probs=c(0.01,0.05,0.1))
VaR ### Diario
p=density(pg)
hist(pg,col="blue",breaks=30, freq = FALSE)
abline(v=VaR, col="red")
lines(p,col="red")

# Proceso Mensual

VaR=quantile(pg,probs=c(0.01,0.05,0.1))
VaR*sqrt(20) ### Mensual



### Test de normalidad

library(tseries)

jarque.bera.test(pg)

x <- rnorm(100)  # null
jarque.bera.test(x)

x <- runif(100)  # alternative
jarque.bera.test(x)


### Modelo paramétrico a través de la distrobución t Student
library(QRM)
res=fit.st(pg)
library(MASS)
fitdistr(pg, "t")