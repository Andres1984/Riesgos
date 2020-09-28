## Primeros cálculos del VaR

library(quantmod)
getSymbols("FB",src="yahoo",from="2019-08-12",to="2020-08-11")
fb=Delt(FB$FB.Close)[-1]

plot(FB$FB.Close)
plot(fb, main="rendimientos")
hist(fb, breaks=50, col="blue")
abline(v = VaRP,col="red")

## Primer Método

mfb=mean(fb)
sdfb=sd(fb)

## VaR diario operacion en largo

VaR99=mfb+qnorm(0.01)*sdfb
VaR99*20000
VaR95=mfb+qnorm(0.05)*sdfb
VaR95*20000
VaR90=mfb+qnorm(0.1)*sdfb
VaR90*20000

## VaR Mensual

VaR99m=mfb*20+qnorm(0.01)*sdfb*sqrt(20)
VaR99m*20000
VaR95m=mfb*20+qnorm(0.05)*sdfb*sqrt(20)
VaR95m*20000
VaR90m=mfb*20+qnorm(0.1)*sdfb*sqrt(20)
VaR90m*20000

## VaR Anual

VaR99a=mfb*250+qnorm(0.01)*sdfb*sqrt(250)
VaR99a*20000
VaR95a=mfb*250+qnorm(0.05)*sdfb*sqrt(250)
VaR95a*20000
VaR90a=mfb*250+qnorm(0.1)*sdfb*sqrt(250)
VaR90a*20000


## VaR diario operacion en corto

VaR99=mfb+qnorm(0.99)*sdfb
VaR99*20000
VaR95=mfb+qnorm(0.95)*sdfb
VaR95*20000
VaR90=mfb+qnorm(0.90)*sdfb
VaR90*20000


## VaR a través de los percentiles

VaR=quantile(fb,probs=c(0.01,0.05,0.1))
VaR[1]*20000
VaR[2]*20000
VaR[3]*20000



library(tseries)
jarque.bera.test(fb)

### VaR usando una distribución t student  
library(QRM)
res=fit.st(fb)
sigma=res$par.ests[3]
nu=res$par.ests[1]

# VaR Diario

VaR99t=mfb+sigma*qt(p=0.01,df=nu)
VaR95t=mfb+sigma*qt(p=0.05,df=nu)
VaR90t=mfb+sigma*qt(p=0.1,df=nu)


#VaR mensual

VaR99tm=mfb*20+sigma*sqrt(20)*qt(p=0.01,df=nu)
VaR95tm=mfb*20+sigma*sqrt(20)*qt(p=0.05,df=nu)
VaR90tm=mfb*20+sigma*sqrt(20)*qt(p=0.1,df=nu)


# VaR Anual


VaR99ta=mfb*250+sigma*sqrt(250)*qt(p=0.01,df=nu)
VaR95ta=mfb*250+sigma*sqrt(250)*qt(p=0.05,df=nu)
VaR90ta=mfb*250+sigma*sqrt(250)*qt(p=0.1,df=nu)

### Modelo VaR por Geométrico Browniano



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


nsims=10000
periods=0:20
FBSM=caminatas(as.numeric(tail(FB$FB.Close ,1)),mfb,sdfb,nsims,periods)
matplot(periods,FBSM[,1:10000],type = "l",ylab = "Precio", xlab="Tiempo", main=" Escenario Facebook")

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

EVFB=EV(FBSM)


matplot(EVFB[,],type = "l",ylab = "Precio",xlab = "Tiempo",main="Escenarios Facebook")

hoy=3700*EVFB[1,4]
VaR5SM=3700*EVFB[21,4]
VaR5SM-hoy



### Prueba de Back testing para un período de 255 días ####


### Organizando los resultados #####

### Usted desea comprar 20 acciones Posición de la Inversión
### Vamos a organizar el valor de la inversión diaría en una columna
### Vamos a organizar el VaR diarío parámetrico normal en otra columna
### Vamos a organizar el VaR diarío parámetrico t Student en otra columna
### Vamos a organizar el VaR diarío No parámetrico

## Test de Kupiec TK

InvInicial=FB$FB.Close[252]*20
InvInicial


FBTK=FB$FB.Close*20###  Usted desea comprar 20 acciones 
colnames(FBTK)="Inversion"
FBTK$VaR1N=FBTK$Inversion*VaR99## VaR paramétrico Dist Normal 99 Nivel de confianza
FBTK$VaR5N=FBTK$Inversion*VaR95## VaR paramétrico Dist Normal 95 Nivel de confianza
FBTK$VaR10N=FBTK$Inversion*VaR90## VaR paramétrico Dist Normal 90 Nivel de confianza
FBTK$VaR1t=FBTK$Inversion*VaR99t## VaR paramétrico Dist t Student 99 Nivel de confianza
FBTK$VaR5t=FBTK$Inversion*VaR95t## VaR paramétrico Dist t Student 95 Nivel de confianza
FBTK$VaR10t=FBTK$Inversion*VaR90t## VaR paramétrico Dist t Student 90 Nivel de confianza
FBTK$VaR1H=FBTK$Inversion*VaR[1]## VaR No paramétrico  99 Nivel de confianza
FBTK$VaR5H=FBTK$Inversion*VaR[2]## VaR No paramétrico  95 Nivel de confianza
FBTK$VaR10H=FBTK$Inversion*VaR[3]## VaR No paramétrico  90 Nivel de confianza

plot(FBTK$Inversion)
dFBTK=diff(FBTK$Inversion)



dFBTK=dFBTK[-1]
FBTTK=FBTK[,2:10][-1]### Solo está el VaR


TK=NULL
TK$N99=ifelse(FBTTK$VaR1N>=dFBTK,1,0)
TK$N95=ifelse(FBTTK$VaR5N>=dFBTK,1,0)
TK$N90=ifelse(FBTTK$VaR10N>=dFBTK,1,0)
TK$T99=ifelse(FBTTK$VaR1t>=dFBTK,1,0)
TK$T95=ifelse(FBTTK$VaR5t>=dFBTK,1,0)
TK$T90=ifelse(FBTTK$VaR10t>=dFBTK,1,0)
TK$H99=ifelse(FBTTK$VaR1H>=dFBTK,1,0)
TK$H95=ifelse(FBTTK$VaR5H>=dFBTK,1,0)
TK$H90=ifelse(FBTTK$VaR10H>=dFBTK,1,0)

TK=as.data.frame(TK)
RTK=colSums(TK)
colnames(TK)=names(FBTTK)  
RTK=as.data.frame(RTK)












