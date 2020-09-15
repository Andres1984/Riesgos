library(quantmod)
getSymbols("WMT", src="yahoo", from="2019-08-20",to="2020-08-19")
wmt=Delt(WMT$WMT.Close)[-1]

colnames(wmt)="WMT"
ggplot(wmt, aes(x=WMT)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") 


### VaR Primer método


mwmt=mean(wmt)
sdwmt=sd(wmt)

# VaR al 1%

VaR1=mwmt+qnorm(0.01)*sdwmt ##Diario Compradora
VaR1

VaR1=mwmt+qnorm(0.99)*sdwmt ## Diario Vendedora
VaR1

VaR1M=mwmt*20+qnorm(0.01)*sdwmt*sqrt(20)##Mensual
VaR1M


VaR1A=mwmt*250+qnorm(0.01)*sdwmt*sqrt(250)##Anual
VaR1A

VaR1A=mwmt*250+qnorm(0.99)*sdwmt*sqrt(250) ## Diario Vendedora
VaR1A


