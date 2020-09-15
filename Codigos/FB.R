## Primeros cálculos del VaR

library(quantmod)
getSymbols("FB",src="yahoo",from="2019-08-12",to="2020-08-11")
fb=Delt(FB$FB.Close)[-1]

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

VaR99=mfb*20+qnorm(0.01)*sdfb*sqrt(20)
VaR99*20000
VaR95=mfb*20+qnorm(0.05)*sdfb*sqrt(20)
VaR95*20000
VaR90=mfb*20+qnorm(0.1)*sdfb*sqrt(20)
VaR90*20000

## VaR Anual

VaR99=mfb*250+qnorm(0.01)*sdfb*sqrt(250)
VaR99*20000
VaR95=mfb*250+qnorm(0.05)*sdfb*sqrt(250)
VaR95*20000
VaR90=mfb*250+qnorm(0.1)*sdfb*sqrt(250)
VaR90*20000


## VaR diario operacion en corto

VaR99=mfb+qnorm(0.99)*sdfb
VaR99*20000
VaR95=mfb+qnorm(0.95)*sdfb
VaR95*20000
VaR90=mfb+qnorm(0.90)*sdfb
VaR90*20000


## VaR a través de los percentiles

VaRP=quantile(fb,probs=c(0.01,0.05,0.1))
VaRP[1]*20000
VaRP[2]*20000
VaRP[3]*20000



library(tseries)
jarque.bera.test(fb)

### VaR usando una distribución t student  
library(QRM)
res=fit.st(fb)
sigma=res$par.ests[3]
nu=res$par.ests[1]


VaR99t=mfb+sigma*qt(p=0.01,df=nu)
VaR95t=mfb+sigma*qt(p=0.05,df=nu)
VaR90t=



VaR5 = -sigma * qt(df=nu,p=p) * value
