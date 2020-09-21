vf=function(va,r,t){

  ### Output en este caso es vf el valor futuro
  ### Inputs va el valor actual
  ### r que es la tasa de interés
  ### t que es el tiempo
  
  vf=va*(1+r)^t
  
  return(vf)
  
}

vf(1000,0.1,1)


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


library(quantmod)
getSymbols("UL",src = "yahoo", from="2019-08-01",to="2020-08-01")
ul=Delt(UL$UL.Close)[-1]
ulm=mean(ul)
ulsd=sd(ul)

# Pronóstico a un mes datos diarios

ULD=caminatas(as.numeric(tail(UL$UL.Close,1)),ulm,ulsd,10000,0:20)
tiempo=0:20
matplot(tiempo,ULD[,1:10000], type='l', xlab='días',ylab='Precios',main='Escenarios Unilever')


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

ULEV=EV(ULD)

matplot(ULEV[,], type='l', xlab='días', ylab='Precios',main="Escenarios Unilever" , cex= 10.8 )

# Pronóstico a un mes datos mensuales

t=seq(0,1,by=0.01)
ULM=caminatas(as.numeric(tail(UL$UL.Close,1)),ulm*20,ulsd*sqrt(20),1000,t)
tiempo=t
matplot(t,ULM[,1:1000], type='l', xlab='mes',ylab='Precios',main='Escenarios Unilever')



set.seed(123)# Número Pseudo aleatorios
evcaminatas <- function(s0, mu, sigma, nsims, periods) 
{
  # So precio spot
  # mu tendencia (promedio de los rendimientos)
  # sigma Volatilidad de los rendimientos
  # nsim número de simulaciones
  # periods Es un vector
  
  s0 = as.vector(s0)
  nsteps = length(periods)
  dt = c(periods[1], diff(periods))
  

    drift = mu - 0.5 * sigma^2
    if( nsteps == 1 ) {
      s0 * exp(drift * dt + sigma * sqrt(dt) * rnorm(nsims))
    } else {
      temp = matrix(exp(drift * dt + sigma * sqrt(dt) * rnorm(nsteps * nsims)), nc=nsims)
      for(i in 2:nsteps) temp[i,] = temp[i,] * temp[(i-1),]
      ST=s0 * temp
    }
  
    ev=rowMeans(ST)
    
    return(ev)
  
}


ULD=evcaminatas(100,0.03,0.04,10000,0:1)
