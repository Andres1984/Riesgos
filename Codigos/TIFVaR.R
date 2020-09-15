### Medidas de VaR 



library(quantmod)
getSymbols("TIF",src="yahoo",from="2019-08-14",to="2020-08-13")
tif=Delt(TIF$TIF.Close)[-1]

## Primer método

## Definimos el intervalos 99%, 95%, 90%

mtif=mean(tif)
sdtif=sd(tif)


### Para un día
VaR99=mtif+qnorm(0.01)*sdtif ## VaR al con un nivel de confianza del 99%
VaR99*10000
VaR95=mtif+qnorm(0.05)*sdtif
VaR95*10000
VaR90=mtif+qnorm(0.1)*sdtif
VaR90*10000




