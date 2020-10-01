library(quantmod)
getSymbols("WMT", src="yahoo", from="2019-08-20",to="2020-08-19")
wmt=Delt(WMT$WMT.Close)[-1]
plot(WMT$WMT.Close)
plot(wmt)

library(ggplot2)
colnames(wmt)="WMT"
ggplot(wmt, aes(x=WMT)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") 


### VaR Primer método Paramétrico


mwmt=mean(wmt)
sdwmt=sd(wmt)

# VaR al Diario

VaR99=mwmt+qnorm(0.01)*sdwmt ##Diario Compradora
VaR99*20*10000

#VaR99=mwmt+qnorm(0.99)*sdwmt ## Diario Vendedora
#VaR99

VaR95=mwmt+qnorm(0.05)*sdwmt ## Diario al 5%
VaR95*10000*3700
VaR90=mwmt+qnorm(0.1)*sdwmt ## Diario al 10%
VaR90*10000*3700
## VaR Mensual

VaR99M=mwmt*20+qnorm(0.01)*sdwmt*sqrt(20)##Mensual al 1%
VaR99M*10000

VaR95M=mwmt*20+qnorm(0.05)*sdwmt*sqrt(20)##Mensual al 5%
VaR95M*10000
VaR90M=mwmt*20+qnorm(0.1)*sdwmt*sqrt(20)##Mensual al 10%
VaR90M*10000
### VaR Anual 

VaR99A=mwmt*250+qnorm(0.01)*sdwmt*sqrt(250)##Anual al 1%
VaR99A*10000

VaR95A=mwmt*250+qnorm(0.05)*sdwmt*sqrt(250)##Anual al 5%
VaR95A*10000

VaR90A=mwmt*250+qnorm(0.1)*sdwmt*sqrt(250)##Anual al 10%
VaR90A*10000
VaR1A=mwmt*250+qnorm(0.99)*sdwmt*sqrt(250) ## Diario Vendedora
VaR1A




## Segundo método No paramétrico Histórico


VaR=quantile(wmt, probs = c(0.01, 0.05, 0.1))
VaRM=VaR*sqrt(20)
VaRA=VaR*sqrt(250)


library(ggplot2)
colnames(wmt)="WMT"
ggplot(wmt, aes(x=WMT)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") + geom_vline(xintercept = VaR, col="blue")

### Test de Jarque Bera

library(tseries)

jarque.bera.test(wmt)

par(mfrow=c(2,1))
plot(density(rnorm(250)), type = "l",main="Normal Distribution")
plot(density(rt(250,50)),type = "l", main="T Student" )


### VaR Paramétrico t Student
library(QRM)
res=fit.st(wmt)
library(MASS)
res1=fitdistr(wmt, "t")

nu=res$par.ests[1]
sigma=res$par.ests[3]
mut=res$par.ests[2]


VaR99t=mut+qt(p=0.01,df=nu)*sigma ## VaR paramétrico t Student
VaR95t=mut+qt(p=0.05,df=nu)*sigma
VaR90t=mut+qt(p=0.1,df=nu)*sigma



### Organizando los resultados #####

### Usted desea comprar 20 acciones 
### Vamos a organizar el valor de la inversión diaría en una columna
### Vamos a organizar el VaR diarío parámetrico normal en otra columna
### Vamos a organizar el VaR diarío parámetrico t Student en otra columna
### Vamos a organizar el VaR diarío No parámetrico
### Vamos a organizar el VaR Geométrico Browniano

InvInicial=WMT$WMT.Close[252]*20
InvInicial
VaRTK=cbind(VaR99,VaR95,VaR90,VaR99t,VaR95t,VaR90t,VaR[1],VaR[2],VaR[3])*as.numeric(InvInicial)
WMTTK=WMT$WMT.Close*20 # Número de acciones
dWMTTK=diff(WMTTK)
colnames(WMTTK)="Inversion"
plot(WMTTK)
WMTTK$VaR1N=WMTTK$Inversion*VaR99
WMTTK$VaR5N=WMTTK$Inversion*VaR95
WMTTK$VaR10N=WMTTK$Inversion*VaR90
WMTTK$VaR1T=WMTTK$Inversion*VaR99t
WMTTK$VaR5T=WMTTK$Inversion*VaR95t
WMTTK$VaR10T=WMTTK$Inversion*VaR90t
WMTTK$VaR1H=WMTTK$Inversion*VaR[1]
WMTTK$VaR5H=WMTTK$Inversion*VaR[2]
WMTTK$VaR10H=WMTTK$Inversion*VaR[3]



dim(WMTTK)

dWMTTK=dWMTTK[-1]
WMTTTK=WMTTK[,2:10][-1]



  TK=NULL
  TK$N99=ifelse(WMTTTK$VaR1N>=dWMTTK, 1, 0)
  TK$N95=ifelse(WMTTTK$VaR5N>=dWMTTK, 1, 0)
  TK$N90=ifelse(WMTTTK$VaR10N>=dWMTTK, 1, 0)
  TK$T99=ifelse(WMTTTK$VaR1T>=dWMTTK, 1, 0)
  TK$T95=ifelse(WMTTTK$VaR5T>=dWMTTK, 1, 0)
  TK$T90=ifelse(WMTTTK$VaR10T>=dWMTTK, 1, 0)
  TK$H99=ifelse(WMTTTK$VaR1H>=dWMTTK, 1, 0)
  TK$H95=ifelse(WMTTTK$VaR5H>=dWMTTK, 1, 0)
  TK$H90=ifelse(WMTTTK$VaR10H>=dWMTTK, 1, 0)
  TK=as.data.frame(TK)

RTK=colSums(TK)  
colnames(TK)=names(WMTTTK)  
RTK=as.data.frame(RTK)
colnames(RTK)=names(WMTTTK) 



