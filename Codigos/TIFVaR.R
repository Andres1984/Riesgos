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

VaR99M=mtif*250+qnorm(0.01)*sdtif*sqrt(250) ## VaR al con un nivel de confianza del 99%
VaR99M*10000*3700
VaR95M=mtif*250+qnorm(0.05)*sdtif*sqrt(250)
VaR95M*10000*3700
VaR90M=mtif*250+qnorm(0.1)*sdtif*sqrt(250)
VaR90M*10000*3700


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
