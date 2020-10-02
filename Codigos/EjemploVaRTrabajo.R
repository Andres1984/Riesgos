### PLantilla alternativa Trabajo####
# Instalar en este orden los siguientes paquetes

# install.packages("Quandl")
# install.packages("PerformanceAnalytics")
# install.packages("quadprog")
# install.packages("IntroCompFinR", repos="http://R-Forge.R-project.org")

library(xts) 
library(Quandl)
library(quantmod)
library(QRM)
library(tseries)
library(PerformanceAnalytics)
library(quadprog)
library(IntroCompFinR)
library(xlsx)
library(readxl)
library(httr)# Otra forma de encontrar direcciones 
url1<-'https://github.com/Andres1984/Riesgos/blob/master/Codigos/TRM.xlsx?raw=true'### Descargar los datos de la TRM
GET(url1, write_disk(tf <- tempfile(fileext = ".xlsx"))) 
TRM <- read_excel(tf)
date <- as.Date(TRM$Date, "%Y-%m-%d")
class(date)
TRMTS=xts(TRM$Price,date)
colnames(TRMTS)="TRM"
plot(TRM)
plot(TRMTS)
getSymbols("SDCI",src="yahoo", from="2018-10-02", to="2020-10-02")
KC1=Quandl("CHRIS/CME_FC2", api_key="zxdSEzha_e_UwhD8Pgdw",type="xts", start_date="2018-10-02", end_date="2020-10-02")
PL1=Quandl("CHRIS/SHFE_ZN2", api_key="zxdSEzha_e_UwhD8Pgdw",type="xts",  start_date="2018-10-02", end_date="2020-10-02")
QG1=Quandl("CHRIS/ICE_G2", api_key="zxdSEzha_e_UwhD8Pgdw",type="xts",  start_date="2018-10-02", end_date="2020-10-02")
LN1=Quandl("CHRIS/ICE_N2", api_key="zxdSEzha_e_UwhD8Pgdw",type="xts", start_date="2018-10-02", end_date="2020-10-02")


### Nos aseguramos de eliminar los valores perdidos
precios=cbind(KC1$Settle,PL1$Settle,QG1$Settle,LN1$Settle,SDCI$SDCI.Close,TRMTS)
index.notNA<-which(is.na(coredata(QG1$Settle))==FALSE)
precios<-precios[index.notNA,]

head(precios)
tail(precios)

apply(is.na(precios)==TRUE, 2,sum)

precios<-na.locf(precios)
colnames(precios)=c("Cacao","Platino","Queso","Lino","ETF","TRM")

apply(is.na(precios)==TRUE, 2,sum)

View(precios)


precios=precios[7:517,]


head(precios)
tail(precios)

apply(is.na(precios)==TRUE, 2,sum)
### Rendimientos

kc1=Delt(precios$Cacao)[-1]# Rendimiento commoditie A
pl=Delt(precios$Platino)[-1]
qg=Delt(precios$Queso)[-1]
kn=Delt(precios$Lino)[-1]
etf=Delt(precios$ETF)[-1]
trm=Delt(precios$TRM)[-1]
rend=cbind(kc1,pl,qg,kn,etf,trm) # Concateno los rendimientos
colnames(rend)=names(precios)
a=cor(rend)
library(corrplot)
corrplot(a,method="number")
plot(rend$Platino)
plot(rend$TRM)
plot(rend$ETF)
## test de Jarque bera

t1=jarque.bera.test(kc1)
t2=jarque.bera.test(pl)
t3=jarque.bera.test(qg)
t4=jarque.bera.test(kn)
t5=jarque.bera.test(etf)
t6=jarque.bera.test(trm)

t1;t2;t3;t4;t5;t6
### VaR a 20, 40 y 60 Normal

m.vec=colMeans(rend)### media de todos los retornos
m.vec
sd.vec=colStdevs(rend) ### desv estandar de todos los retornos
sd.vec
### Cacao

VaRN99c=m.vec[1]+qnorm(0.01)*sd.vec[1]
VaRN95c=m.vec[1]+qnorm(0.05)*sd.vec[1]
VaRN90c=m.vec[1]+qnorm(0.1)*sd.vec[1]

### Platino

VaRN99p=m.vec[2]+qnorm(0.01)*sd.vec[2]
VaRN95p=m.vec[2]+qnorm(0.05)*sd.vec[2]
VaRN90p=m.vec[2]+qnorm(0.1)*sd.vec[2]

#### VaR t Student 20, 40 y 60


## Queso

res=fit.st(rend$Queso)
qn=res$par.ests[1]
qm=res$par.ests[2]
qs=res$par.ests[3]


VaRT99q=qm+qt(0.01,qn)*qs
VaRT95q=qm+qt(0.05,qn)*qs
VaRT90q=qm+qt(0.1,qn)*qs

### VaR TRM
trm=Delt(precios$TRM)[-1]
mtrm=mean(trm)
sdtrm=sd(trm)
trmjb=jarque.bera.test(trm)
trmjb


VaRN99trm=mtrm+qnorm(0.01)*sdtrm
VaRN95trm=mtrm+qnorm(0.05)*sdtrm
VaRN90trm=mtrm+qnorm(0.1)*sdtrm
res=fit.st(trm)
nutrm=res$par.ests[1]
mutrm=res$par.ests[2]
sdtrm=res$par.ests[3]


VaRT99trm=mutrm+qt(0.01,nutrm)*sdtrm
VaRT95trm=mutrm+qt(0.05,nutrm)*sdtrm
VaRT90trm=mutrm+qt(0.1,nutrm)*sdtrm
VaRT99trm
VaRN99trm


### Portafolio optimo y minimo ####
m.vec60=m.vec[1:4]*60 ### vector de rendimientos
varcov=var(rend[,1:4])*60### Historico#### Aca se debe construir la matriz varcovar con ewma

pmin=globalMin.portfolio( m.vec60,varcov,shorts = TRUE)
pmin$er
pmin$sd
pmin$weights


risk.free=0.21788/100
r=(1+risk.free)^(1/4)-1


pop=tangency.portfolio(m.vec60, varcov, r, shorts = TRUE)
pop$er
pop$sd
pop$weights

inin=200000000/3842
inin
inportop=inin*pop$weights
inportop

inminpor=inin*pmin$weights
inminpor
