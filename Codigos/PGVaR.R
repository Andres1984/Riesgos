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


### ES FB

p = c(0.1,0.05,0.01) 
VaR = qnorm(p)
ES = -dnorm(qnorm(p))/p
ES
ES90=mpg+ES[1]*sdpg### ES al 90% de Nivel de confianza
ES95=mpg+ES[2]*sdpg### ES al 95% de Nivel de confianza
ES99=mpg+ES[3]*sdpg### ES al 99% de Nivel de confianza
ES99*10000
ES95*10000
ES90*10000

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
ES=quantile(pg,probs=c(0.005,0.025,0.05))
VaR ### Diario
p=density(pg)
hist(pg,col="blue",breaks=30, freq = FALSE)
abline(v=VaR, col="red",cex=1.4)
abline(v=ES, col="green")
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
nu=res$par.ests[1]### Grados de libertad
mut=res$par.ests[2]
sigma=res$par.ests[3]

# VaR t Student diario
VaRt99=mut+qt(0.01,nu)*sigma
VaRt95=mut+qt(0.05,nu)*sigma
VaRt90=mut+qt(0.1,nu)*sigma

VaRt99*10000
VaRt95*10000
VaRt90*10000
p = c(0.1,0.05,0.01) 
VaRtES = qt(p,nu)
VaRtES
ES = -dt(p,nu)/p
ES

ESt99=mut+ES[3]*sigma
ESt95=mut+ES[2]*sigma
ESt90=mut+ES[1]*sigma

ESt99*10000
ESt95*10000
ESt90*10000

# VaR t Student Mensual

VaRt99m=mut*20+qt(0.01,nu)*sigma*sqrt(20)
VaRt95m=mut*20+qt(0.05,nu)*sigma*sqrt(20)
VaRt90m=mut*20+qt(0.1,nu)*sigma*sqrt(20)


# VaR t Student Anual

VaRt99a=mut*250+qt(0.01,nu)*sigma*sqrt(250)
VaRt95a=mut*250+qt(0.05,nu)*sigma*sqrt(250)
VaRt90a=mut*250+qt(0.1,nu)*sigma*sqrt(250)


###  VaR Simulación Montecarolo ( Movimiento Geométrico Browniano)


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


PGMB=caminatas(as.numeric(tail(PG$PG.Close,1)),mpg,sdpg,nsims,periods)
matplot(periods,PGMB[,1:10000],type = "l",ylab = "Precio", xlab = "Tiempo", main=" Escenarios PG")



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



PGEV=EV(PGMB)


matplot(PGEV[,],type = "l",ylab = "Precio", xlab = "Tiempo", main=" Escenarios PG")


hoy=3700*PGEV[1,4]*20
VaR95Gbm=3700*PGEV[21,4]*20
Perdida=VaR95Gbm-hoy



### Prueba de Back testing para un período de 255 días ####


### Organizando los resultados #####

### Usted desea comprar 20 acciones Posición de la Inversión
### Vamos a organizar el valor de la inversión diaría en una columna
### Vamos a organizar el VaR diarío parámetrico normal en otra columna
### Vamos a organizar el VaR diarío parámetrico t Student en otra columna
### Vamos a organizar el VaR diarío No parámetrico

## Test de Kupiec TK



InvInicial=PG$PG.Close[252]*20
InvInicial


PGTK=PG$PG.Close*20###  Usted desea comprar 20 acciones 
colnames(PGTK)="Inversion"
plot(PGTK)
PGTK$VaR1N=PGTK$Inversion*VaR99## VaR paramétrico Dist Normal 99 Nivel de confianza
PGTK$VaR5N=PGTK$Inversion*VaR95## VaR paramétrico Dist Normal 95 Nivel de confianza
PGTK$VaR10N=PGTK$Inversion*VaR90## VaR paramétrico Dist Normal 90 Nivel de confianza
PGTK$VaR1T=PGTK$Inversion*VaRt99 ## VaR paramétrico Dist t Student 99 Nivel de confianza
PGTK$VaR5T=PGTK$Inversion*VaRt95## VaR paramétrico Dist t Student 95 Nivel de confianza
PGTK$VaR10T=PGTK$Inversion*VaRt90## VaR paramétrico Dist t Student 90 Nivel de confianza
PGTK$VaR1H=PGTK$Inversion*VaR[1] ## VaR No paramétrico 99 Nivel de confianza
PGTK$VaR5H=PGTK$Inversion*VaR[2]  ## VaR No paramétrico  95 Nivel de confianza
PGTK$VaR10H=PGTK$Inversion*VaR[3] ## VaR No paramétrico 90 Nivel de confianza

dPGTK=diff(PGTK$Inversion)[-1]
PGTTK=PGTK[,2:10][-1]### Solo está el VaR

TK=NULL
TK$N99=ifelse(PGTTK$VaR1N>dPGTK,1,0)
TK$N95=ifelse(PGTTK$VaR5N>dPGTK,1,0)
TK$N90=ifelse(PGTTK$VaR10N>dPGTK,1,0)
TK$T99=ifelse(PGTTK$VaR1T>dPGTK,1,0)
TK$T95=ifelse(PGTTK$VaR5T>dPGTK,1,0)
TK$T90=ifelse(PGTTK$VaR10T>dPGTK,1,0)
TK$H99=ifelse(PGTTK$VaR1H>dPGTK,1,0)
TK$H95=ifelse(PGTTK$VaR5H>dPGTK,1,0)
TK$H90=ifelse(PGTTK$VaR10H>dPGTK,1,0)

TK=as.data.frame(TK)
RTK=colSums(TK)
colnames(TK)=names(PGTTK)  
RTK=as.data.frame(RTK)




### Test de Kupiec Movimiento Geomértico


nsims=10000
periods=0:251


PGMBTK=caminatas(as.numeric(head(PG$PG.Close,1)),mpg,sdpg,nsims,periods)
PGEVTK=EV(PGMBTK)
PGEVTK$HISTORICO=PG$PG.Close
matplot(PGEVTK,type = "l",ylab = "Precio", xlab = "Tiempo", main=" Test de Kupiec PG")
