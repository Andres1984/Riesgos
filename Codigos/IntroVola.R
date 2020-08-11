# Ejemplo de Volatilidad

library(quantmod)

getSymbols("TSLA", src="yahoo", from="2019-01-01",to="2019-12-31")

TSLAA=TSLA$TSLA.Close
getSymbols("TSLA", src="yahoo", from="2019-03-01",to="2020-04-30")

TSLAB=TSLA$TSLA.Close

getSymbols("TSLA", src="yahoo", from="2019-07-01",to="2020-07-01")

TSLAC=TSLA$TSLA.Close


par(mfrow=c(3,1))
plot(TSLAA, col="blue")
plot(TSLAB, col="red")
plot(TSLAC, col="orange")



tslaa=Delt(TSLAA)[-1]
tslab=Delt(TSLAB)[-1]
tslac=Delt(TSLAC)[-1]


par(mfrow=c(3,1))
plot(tslaa, col="blue")
plot(tslab, col="red")
plot(tslac, col="orange")



getSymbols("KO", src="yahoo", from="2019-01-01",to="2019-12-31")
getSymbols("KO", src="yahoo", from="2019-07-01",to="2020-07-01")
ko=Delt(KO$KO.Close)[-1]


par(mfrow=c(2,1))
plot(tslaa, col="blue")
plot(ko, col="red")



