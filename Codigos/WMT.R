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
VaR99*10000*3700

VaR99=mwmt+qnorm(0.99)*sdwmt ## Diario Vendedora
VaR99

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
