# install.packages("PerformanceAnalytics")
# install.packages("quadprog")
# install.packages("IntroCompFinR", repos="http://R-Forge.R-project.org")
library(PerformanceAnalytics)
library(quadprog)
library(IntroCompFinR)
library(quantmod)
library(xts)

simbolos=c("TIF","PG","WMT","FB")

getSymbols(simbolos,src="yahoo", from="2019-10-05",to="2020-10-05")

par(mfrow=c(2,2))
plot(TIF$TIF.Close)
plot(PG$PG.Close)
plot(WMT$WMT.Close)
plot(FB$FB.Close)

tif=Delt(TIF$TIF.Close)[-1]
pg=Delt(PG$PG.Close)[-1]
wmt=Delt(WMT$WMT.Close)[-1]
fb=Delt(FB$FB.Close)[-1]
rend=cbind(tif,pg,wmt,fb)
colnames(rend)=simbolos
mu.vec=colMeans(rend)## Vector de rendimientos
varcovar=var(rend)## Matriz Var Covar vol historíca
sd.vec=colSds(rend)## Vector de Desviación estandar
sdtif=sd(tif)
sdpg=sd(pg)
sdwmt=sd(wmt)
sdfb=sd(fb)
sd.vec=cbind(sdtif,sdpg,sdwmt,sdfb)
corrp=cor(rend) ## Matriz de correlaciones
library(corrplot)
corrplot(corrp,method = "number")
### VaR de cada activo paramétrico mensual 95%

vartif=mu.vec[1]*20+sd.vec[1]*qnorm(0.05)*sqrt(20)
varpg=mu.vec[2]*20+sd.vec[2]*qnorm(0.05)*sqrt(20)
varwmt=mu.vec[3]*20+sd.vec[3]*qnorm(0.05)*sqrt(20)
varfb=mu.vec[4]*20+sd.vec[4]*qnorm(0.05)*sqrt(20)
var.vec=cbind(vartif,varpg,varwmt,varfb)

mu.vec20=mu.vec*20
sigma20=varcovar*20
pmin=globalMin.portfolio(mu.vec, sigma20, shorts = TRUE)
pmin$er
pmin$sd
wmin=pmin$weights

risk.free=0.21788/100
r=(1+risk.free)^(1/12)-1

pop=tangency.portfolio(mu.vec, sigma20, r, shorts = TRUE)
pop$er
pop$sd
wop=pop$weights



VaR1=c(var.vec[1]^2,corrp[1,2]*var.vec[1]*var.vec[2],corrp[1,3]*var.vec[1]*var.vec[3],corrp[1,4]*var.vec[1]*var.vec[4])
VaR2=c(corrp[1,2]*var.vec[1]*var.vec[2],var.vec[2]^2,corrp[2,3]*var.vec[2]*var.vec[3],corrp[2,4]*var.vec[2]*var.vec[4])
VaR3=c(corrp[1,3]*var.vec[1]*var.vec[3],corrp[2,3]*var.vec[2]*var.vec[3],var.vec[3]^2,corrp[3,4]*var.vec[3]*var.vec[4])
VaR4=c(corrp[1,4]*var.vec[1]*var.vec[4],corrp[2,4]*var.vec[2]*var.vec[4],corrp[3,4]*var.vec[3]*var.vec[4],var.vec[4]^2)

VaRm=rbind(VaR1,VaR2,VaR3,VaR4)
colnames(VaRm)=simbolos
rownames(VaRm)=simbolos
VaR2min=t(wmin)%*%VaRm%*%wmin
VaRmin=sqrt(VaR2min)
VaR2op=t(wop)%*%VaRm%*%wop
VaRop=sqrt(VaR2op)
VaRmin*52000
VaRop*52000



#### VaR del Portafolio versión 2

sdmin=pmin$sd
mumin=pmin$er
VaRp2v= mumin+qnorm(0.05)*sdmin ## Parmétrico normal
VaRp2v*52000*-1

sdop=pop$sd
muop=pop$er
VaRp2op=muop+qnorm(0.05)*sdop
VaRp2op*52000*-1

#### No se le olvide hacer el xts

wmin
Stocks=cbind(TIF$TIF.Close,PG$PG.Close,WMT$WMT.Close,FB$FB.Close)

# Construcción del portafolio
port=as.data.frame(rep(1,length(acciones[,1])))# Cree un data frame
port$b=rep(1,length(STocks[,1]))# Agregue columnas
port$c=rep(1,length(STocks[,1]))
port$d=rep(1,length(STocks[,1]))
colnames(port)=simbolos
pesos=wmin# Asumiendo que los pesos del mínimo global
for (i in 1:length(STocks[,1])){
  port[i,]=as.data.frame( STocks[i,]*pesos)
}

portmin=rowSums(port)
o=data.frame(date=index(FB), coredata(FB))
o=as.Date(o$date)
portmin=xts(portmin,as.Date(o))
rendport=Delt(portmin)[-1]
plot(portmin)
plot(rendport)
plot(cumsum(rendport))
