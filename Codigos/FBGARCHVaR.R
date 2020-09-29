# load libraries
library(PerformanceAnalytics)
library(quantmod)
library(rugarch)
library(car)
library(MTS)

getSymbols("FB",src="yahoo",from="2019-08-12",to="2020-08-11")
fb=Delt(FB$FB.Close)[-1]
options(digits=4)

colnames(fb)="FB"
dataToPlot = cbind(fb, fb^2, abs(fb))
colnames(dataToPlot) = c("Returns", "Returns^2", "abs(Returns)")
plot.zoo(dataToPlot, main="FB Daily Returns", col="blue")


par(mfrow=c(3,1))
acf(fb, main="FB Returns")
acf(fb^2, main="FB Returns^2")
acf(abs(fb), main="FB abs(Returns)")

table.Stats(fb)
spec = ugarchspec()
fit = ugarchfit(data = fb, spec = spec,solver = "hybrid", fit.control = list(stationarity = 1))


Box.test(coredata(fb^2), type="Ljung-Box", lag = 12)
fbn=data.matrix(as.data.frame(fb))
library(aTSA)
archTest(fbn)


# estimated coefficients
coef(fit)
# unconditional mean in mean equation
uncmean(fit)
# unconditional variance: omega/(alpha1 + beta1)
uncvariance(fit)
# persistence: alpha1 + beta1
persistence(fit)
# half-life:
halflife(fit)

# residuals: e(t)
plot.ts(residuals(fit), ylab="e(t)", col="blue")
abline(h=0)

# sigma(t) = conditional volatility
plot.ts(sigma(fit), ylab="sigma(t)", col="blue")

# illustrate plot method
plot(fit)


## Simulacion


garch11.sim = ugarchsim(fit,n.sim=nrow(fb),rseed=123,tartMethod="unconditional")
# plot actual returns and simulated returns
par(mfrow=c(2,1))
plot(fb, main="Actual tif returns")
plot(as.xts(garch11.sim@simulation$seriesSim, order.by=index(fb)),main="Simulated GARCH(1,1) Returns")
par(mfrow=c(1,1))

garch11.fcst = ugarchforecast(fit, n.ahead=100,n.roll=0)
par(mfrow=c(2,1))
plot(garch11.fcst, which=1)
plot(garch11.fcst, which=3)


## Grafico Varianza estacionaria

fcst.var.hDay = cumsum(garch11.fcst@model$modeldata$sigma^2)
fcst.vol.hDay = sqrt(fcst.var.hDay)

plot(fcst.var.hDay,fcst.vol.hDay, type="l",col="blue")


# VaR con GARCH
fcst.df = as.data.frame(garch11.fcst@model$modeldata$data)
VaR.95.garch11 = fcst.df$series[1] + fcst.df$sigma[1]*qnorm(0.05)


# VAR a 20 días
sigma.20day = sqrt(sum(fcst.df$sigma[1:20]^2))
VaR.95.garch11.20day = 20*fcst.df$series[1] + sigma.20day*qnorm(0.05)




garch11.boot = ugarchboot(fit, method="Partial", n.ahead=100, n.bootpred=2000)

plot(garch11.boot)







spec = ugarchspec(distribution.model = "std")### Reajustar a la distribución
spec= ugarchspec(variance.model = list(garchOrder=c(1,1)), mean.model = list(armaOrder=c(0,0)))
mod = ugarchroll(spec, data = fb, n.ahead = 1, 
                 n.start = 100,  refit.every = 10, refit.window = "recursive", 
                 solver = "hybrid", fit.control = list(fit@fit$coef),
                 calculate.VaR = TRUE, VaR.alpha = c(0.01, 0.025, 0.05),
                 keep.coef = TRUE)
report(mod, type="VaR", VaR.alpha = 0.01, conf.level = 0.95) 
report(mod, type="fpm")

plot(mod)
# VaR plot
plot(mod, which=1)
# Coef plot`
plot(mod, which=5)
# show backtesting report

report(mod, type="VaR")
report(mod, type="fpm")

