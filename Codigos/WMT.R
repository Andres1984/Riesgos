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
VaR99*10000

#VaR99=mwmt+qnorm(0.99)*sdwmt ## Diario Vendedora
#VaR99

VaR95=mwmt+qnorm(0.05)*sdwmt ## Diario al 5%
VaR95*10000
VaR90=mwmt+qnorm(0.1)*sdwmt ## Diario al 10%
VaR90*10000


### ES WMT

p = c(0.01,0.05,0.1) 
VaR = qnorm(p)
VaR
ES = -dnorm(qnorm(p))/p
ES

ES99=mwmt+ES[1]*sdwmt
ES95=mwmt+ES[2]*sdwmt
ES90=mwmt+ES[3]*sdwmt

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
ESH=quantile(wmt, probs= c(0.005, 0.025, 0.05))
ESH*52000*0.3
VaR*52000*0.3
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

p = c(0.01,0.05,0.1) 
VaRtES = qt(p,nu)
VaRtES
ESt = -dt(p,nu)/p
ESt

ES99t=mut+ESt[1]*sigma
ES95t=mut+ESt[2]*sigma
ES90t=mut+ESt[3]*sigma


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

dWMTTK=diff(WMTTK$Inversion)



dWMTTK=dWMTTK[-1]
WMTTTK=WMTTK[,2:10][-1]



TK=NULL
TK$N99=ifelse(WMTTTK$VaR1N>dWMTTK,1,0)
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
RTK=as.data.frame(RTK)


#### Movimiento geométrico Browniano


set.seed(123)# Número Pseudo aleatorios
caminatas <- function(s0, mu, sigma, nsims, periods) 
{
  # So precio spot
  # mu tendencia (promedio de los rendimientos)
  # sigma Volatilidad de los rendimientos
  # nsim número de simulaciones
  # periods Es un vector
  
  s0 = as.vector(s0)
  nsteps = length(periods)
  dt = c(periods[1], diff(periods))
  
  if( length(s0) == 1 ) {
    drift = mu - 0.5 * sigma^2
    if( nsteps == 1 ) {
      s0 * exp(drift * dt + sigma * sqrt(dt) * rnorm(nsims))
    } else {
      temp = matrix(exp(drift * dt + sigma * sqrt(dt) * rnorm(nsteps * nsims)), nc=nsims)
      for(i in 2:nsteps) temp[i,] = temp[i,] * temp[(i-1),]
      s0 * temp
    }
  } else {
    require(MASS)
    drift = mu - 0.5 * diag(sigma)
    n = length(mu)
    
    if( nsteps == 1 ) {
      s0 * exp(drift * dt + sqrt(dt) * t(mvrnorm(nsims, rep(0, n), sigma)))
    } else {
      temp = array(exp(as.vector(drift %*% t(dt)) + t(sqrt(dt) * mvrnorm(nsteps * nsims, rep(0, n), sigma))), c(n, nsteps, nsims))
      for(i in 2:nsteps) temp[,i,] = temp[,i,] * temp[,(i-1),]
      s0 * temp
    }
  }
  
  
  
}


### Proyección de precios

nsims=10000
periods=0:250


WMTSM=caminatas(as.numeric(tail(WMT$WMT.Close,1)),mwmt,sdwmt,nsims,periods )
matplot(periods,WMTSM[,1:10000], type='l', xlab='días',ylab='Precios',main='Escenarios Wallmart')

EV=function(ULD){
  n= dim(ULD)[1]
  
  # Matriz de simulaciones  
  require(dplyr)
  EVUL=rowMeans(ULD)
  VOLUL=sqrt(rowMeans(ULD^{2})-EVUL^{2})#V[x]=E[x^2]-(E[x])^2
  ULEV=as.data.frame(EVUL)
  ULEV=mutate(ULEV, VOLULS =EVUL+VOLUL)
  ULEV=mutate(ULEV, VOLULI =EVUL-VOLUL)
  ULEV[1,2:3]=EVUL[1]
  
  TUL=t(ULEV)
  TUL[,1]=EVUL[1]
  VAR0.05=rep(0,n-1)
  for (i in 1:dim(TUL)[2]){
    VAR0.05[i]=quantile(TUL[,i],0.05)
  }
  ULEV$VAR0.05=VAR0.05
  VAR0.01=rep(0,n-1)
  for (i in 1:dim(TUL)[2]){
    VAR0.01[i]=quantile(TUL[,i],0.01)
  }
  ULEV$VAR0.01=VAR0.01
  
  return(ULEV)
}


WMTEV=EV(WMTSM)


matplot(WMTEV[,], type='l', xlab='días', ylab='Precios',main="Escenarios Wallmart" , cex= 10.8 )



#### Test de Kupiec ### backtesting


WMTSMTK=caminatas(as.numeric(head(WMT$WMT.Close,1)),mwmt,sdwmt,nsims,periods )
WMTEVTK=EV(WMTSMTK)
WMTEVTK$HISTORICO=WMT$WMT.Close[1:251]
matplot(WMTEVTK[,], type='l', xlab='días', ylab='Precios',main="Test de Kupiec Wallmart" , cex= 10.8 )

# Hasta este punto se trabaja con redimientos
EWMAwmt=cbind(wmt)
colnames(EWMAwmt)="WMT"
lambda=0.95
EWMAwmt$l=0

l=rep(0,length(EWMAwmt[,2]))### Columnas de ceros

for  (i in length(EWMAwmt[,2]):1){
  
  l[i]=lambda^(i-1)  ##
  
}

l=sort(l,decreasing = FALSE)
EWMAwmt$l=l
EWMAwmt$B=EWMAwmt[,1]^2
EWMAwmt$C=EWMAwmt$l*EWMAwmt$B
volwmt=sqrt(sum(EWMAwmt$C)*(1-lambda))

WMTSMTKE=caminatas(as.numeric(head(WMT$WMT.Close,1)),mwmt,sdwmt,nsims,periods )
WMTEVTKE=EV(WMTSMTKE)
WMTEVTKE$HISTORICO=WMT$WMT.Close[1:251]


par(mfrow=c(2,1))
matplot(WMTEVTK[,], type='l', xlab='días', ylab='Precios',main="Test de Kupiec Wallmart" , cex= 10.8 )
matplot(WMTEVTKE[,], type='l', xlab='días', ylab='Precios',main="Test de Kupiec EWMA Wallmart" , cex= 10.8 )
