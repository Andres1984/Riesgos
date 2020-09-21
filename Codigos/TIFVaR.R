### Medidas de VaR 



library(quantmod)
getSymbols("TIF",src="yahoo",from="2019-08-14",to="2020-08-13")
tif=Delt(TIF$TIF.Close)[-1]

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


library(tseries)

jarque.bera.test(tif)




