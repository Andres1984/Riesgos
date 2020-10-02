### Medidas de VaR 



library(quantmod)
getSymbols("TIF",src="yahoo",from="2019-08-14",to="2020-08-13")
tif=Delt(TIF$TIF.Close)[-1]


plot(TIF$TIF.Close, main="TIF")
plot(tif, main="Rendimientos TIF")

## Primer método

## Definimos el intervalos 99%, 95%, 90%

mtif=mean(tif)
sdtif=sd(tif)


### VaR Para un día un paramétrico
VaR99=mtif+qnorm(0.01)*sdtif ## VaR al con un nivel de confianza del 99%
VaR99*10000*3700
VaR95=mtif+qnorm(0.05)*sdtif
VaR95*10000*3700
VaR90=mtif+qnorm(0.1)*sdtif
VaR90*10000*3700

### VaR para un mes 

VaR99M=mtif*20+qnorm(0.01)*sdtif*sqrt(20) ## VaR al con un nivel de confianza del 99%
VaR99M*10000*3700
VaR95M=mtif*20+qnorm(0.05)*sdtif*sqrt(20)
VaR95M*10000*3700
VaR90M=mtif*20+qnorm(0.1)*sdtif*sqrt(20)
VaR90M*10000*3700


### VaR para  de forma Anual

VaR99A=mtif*250+qnorm(0.01)*sdtif*sqrt(250) ## VaR al con un nivel de confianza del 99%
VaR99A*10000*3700
VaR95A=mtif*250+qnorm(0.05)*sdtif*sqrt(250)
VaR95A*10000*3700
VaR90A=mtif*250+qnorm(0.1)*sdtif*sqrt(250)
VaR90A*10000*3700


### Segundo Método usando los perccentiles No paramétrico
VaR=quantile(tif,probs=c(0.01,0.05,0.1))


library(ggplot2)
colnames(tif)="TIF"
ggplot(tif, aes(x=TIF)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="cyan") 

#### Prueba de normalidad 

### Test de Jarque Bera

par(mfrow=c(2,1))
plot(density(rnorm(250)), type = "l",main="Normal Distribution")
plot(density(rt(250,2)),type = "l", main="T Student" )


library(tseries)

jarque.bera.test(tif)


## VaR Usando la distribución t de Student

library(QRM)
res=fit.st(tif)
nu=res$par.ests[1]
mu=res$par.ests[2]
sigma=res$par.ses[3]
VaR99t=mu+qt(0.01,nu)*sigma
VaR95t=mu+qt(0.05,nu)*sigma
VaR90t=mu+qt(0.1,nu)*sigma


# VaR mensual 
VaR99tm=mu*20+qt(0.01,nu)*sigma*sqrt(20)
VaR95tm=mu*20+qt(0.05,nu)*sigma*sqrt(20)
VaR90tm=mu*20+qt(0.1,nu)*sigma*sqrt(20)


#VaR Anual


VaR99ta=mu*250+qt(0.01,nu)*sigma*sqrt(250)
VaR95ta=mu*250+qt(0.05,nu)*sigma*sqrt(250)
VaR90ta=mu*250+qt(0.1,nu)*sigma*sqrt(250)


## VaR por Simulacióm Montecarlo

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
TIFM=caminatas(as.numeric(tail(TIF$TIF.Close,1)),mtif,sdtif,nsims,periods)
matplot(periods,TIFM[,1:nsims],type='l',ylab="Precios",xlab="Tiempo",main="Escenarios Tiffany´s")

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


TIFEV=EV(TIFM)


matplot(TIFEV[,], type='l', xlab='Tiempo', ylab='Precios',main="Escenarios Tiffany´s" , cex= 10.8 )

Hoy=3700*TIFEV[1,4]*20
VaR5Sm=3700*TIFEV[21,4]*20

Perdida5=VaR5Sm-Hoy


### Prueba de Back testing para un período de 255 días ####


### Organizando los resultados #####

### Usted desea comprar 20 acciones 
### Vamos a organizar el valor de la inversión diaría en una columna
### Vamos a organizar el VaR diarío parámetrico normal en otra columna
### Vamos a organizar el VaR diarío parámetrico t Student en otra columna
### Vamos a organizar el VaR diarío No parámetrico



InvInicial=TIF$TIF.Close[252]*20
InvInicial

TIFTK=TIF$TIF.Close*20###  Usted desea comprar 20 acciones 
colnames(TIFTK)="Inversion"
TIFTK$VaR1N=TIFTK$Inversion*VaR99 ## VaR paramétrico Dist Normal 99 Nivel de confianza
TIFTK$VaR5N=TIFTK$Inversion*VaR95 ## VaR paramétrico Dist Normal 95 Nivel de confianza
TIFTK$VaR10N=TIFTK$Inversion*VaR90 ## VaR paramétrico Dist Normal 90 Nivel de confianza
TIFTK$VaR1t=TIFTK$Inversion*VaR99t ## VaR paramétrico Dist T STudent 99 Nivel de confianza
TIFTK$VaR5t=TIFTK$Inversion*VaR95t ## VaR paramétrico Dist T STudent 95 Nivel de confianza
TIFTK$VaR10t=TIFTK$Inversion*VaR90t ## VaR paramétrico Dist T STudent 90 Nivel de confianza
TIFTK$VaR1H=TIFTK$Inversion*VaR[1] ## VaR No paramétrico  99 Nivel de confianza
TIFTK$VaR5H=TIFTK$Inversion*VaR[2] ## VaR No paramétrico  95 Nivel de confianza
TIFTK$VaR10H=TIFTK$Inversion*VaR[3] ## VaR No paramétrico  90 Nivel de confianza

dTIFTK=diff(TIFTK$Inversion)


dTIFTK=dTIFTK[-1]# Diferencias en la posición
TIFTTK=TIFTK[,2:10][-1]### Solo está el VaR


TK=NULL
TK$N99=ifelse(TIFTTK$VaR1N>=dTIFTK,1,0)
TK$N95=ifelse(TIFTTK$VaR5N>=dTIFTK,1,0)
TK$N90=ifelse(TIFTTK$VaR10N>=dTIFTK,1,0)
TK$T99=ifelse(TIFTTK$VaR1t>=dTIFTK,1,0)
TK$T95=ifelse(TIFTTK$VaR5t>=dTIFTK,1,0)
TK$T90=ifelse(TIFTTK$VaR10t>=dTIFTK,1,0)
TK$H99=ifelse(TIFTTK$VaR1H>=dTIFTK,1,0)
TK$H95=ifelse(TIFTTK$VaR5H>=dTIFTK,1,0)
TK$H90=ifelse(TIFTTK$VaR10H>=dTIFTK,1,0)
TK=as.data.frame(TK)

RTK=colSums(TK)  
colnames(TK)=names(TIFTTK)  
RTK=as.data.frame(RTK)



### VaR con volatilidad ewma

# Hasta este punto se trabaja con redimientos
EWMAtif=cbind(tif)
colnames(EWMAtif)="TIF"
lambda=0.95
EWMAtif$l=0

l=rep(0,length(EWMAtif[,2]))### Columnas de ceros

for  (i in length(EWMAtif[,2]):1){
  
  l[i]=lambda^(i-1)  ##
  
}

l=sort(l,decreasing = FALSE)
EWMAtif$l=l
EWMAtif$B=EWMAtif[,1]^2
EWMAtif$C=EWMAtif$l*EWMAtif$B
voltif=sqrt(sum(EWMAtif$C)*(1-lambda))



VaR1NE=mtif+qnorm(0.01)*voltif
VaR5NE=mtif+qnorm(0.05)*voltif
VaR10NE=mtif+qnorm(0.1)*voltif

VaR99tE=mu+qt(0.01,nu)*voltif
VaR95tE=mu+qt(0.05,nu)*voltif
VaR90tE=mu+qt(0.1,nu)*voltif



TIFTK$VaR1NE=TIFTK$Inversion*VaR1NE ## VaR paramétrico Dist Normal 99 Nivel de confianza vol ewma
TIFTK$VaR5NE=TIFTK$Inversion*VaR5NE ## VaR paramétrico Dist Normal 95 Nivel de confianza vol ewma
TIFTK$VaR10NE=TIFTK$Inversion*VaR10NE ## VaR paramétrico Dist Normal 90 Nivel de confianza vol ewma
TIFTK$VaR1tE=TIFTK$Inversion*VaR99tE ## VaR paramétrico Dist T STudent 99 Nivel de confianza vol ewma
TIFTK$VaR5tE=TIFTK$Inversion*VaR95tE ## VaR paramétrico Dist T STudent 95 Nivel de confianza vol ewma
TIFTK$VaR10tE=TIFTK$Inversion*VaR90tE ## VaR paramétrico Dist T STudent 90 Nivel de confianza vol ewma
TIFTTK=TIFTK[,2:16][-1]### Solo está el VaR


TK=NULL
TK$N99=ifelse(TIFTTK$VaR1N>=dTIFTK,1,0)
TK$N95=ifelse(TIFTTK$VaR5N>=dTIFTK,1,0)
TK$N90=ifelse(TIFTTK$VaR10N>=dTIFTK,1,0)
TK$T99=ifelse(TIFTTK$VaR1t>=dTIFTK,1,0)
TK$T95=ifelse(TIFTTK$VaR5t>=dTIFTK,1,0)
TK$T90=ifelse(TIFTTK$VaR10t>=dTIFTK,1,0)
TK$H99=ifelse(TIFTTK$VaR1H>=dTIFTK,1,0)
TK$H95=ifelse(TIFTTK$VaR5H>=dTIFTK,1,0)
TK$H90=ifelse(TIFTTK$VaR10H>=dTIFTK,1,0)
TK$NE99=ifelse(TIFTTK$VaR1NE >=dTIFTK,1,0)
TK$NE95=ifelse(TIFTTK$VaR5NE >=dTIFTK,1,0)
TK$NE90=ifelse(TIFTTK$VaR10NE>=dTIFTK,1,0)
TK$TE99=ifelse(TIFTTK$VaR1tE >=dTIFTK,1,0)
TK$TE95=ifelse(TIFTTK$VaR5tE >=dTIFTK,1,0)
TK$TE90=ifelse(TIFTTK$VaR10tE>=dTIFTK,1,0)

TK=as.data.frame(TK)

RTK=colSums(TK)  
colnames(TK)=names(TIFTTK)  
RTK=as.data.frame(RTK)




