## Volatilidad EWMA
library(quantmod)
getSymbols("BA",src="yahoo",from="2019-08-24",to="2020-08-24")
ba=Delt(BA$BA.Close)[-1]
# Hasta este punto se trabaja con redimientos
EWMAba=cbind(ba)
colnames(EWMAba)="BA"
lambda=0.9
EWMAba$l=0

l=rep(0,length(EWMAba[,2]))### Columnas de ceros

for  (i in length(EWMAba[,2]):1){
  
  l[i]=lambda^(i-1)  ##
  
}

l=sort(l,decreasing = FALSE)
EWMAba$l=l
EWMAba$B=EWMAba[,1]^2
EWMAba$C=EWMAba$l*EWMAba$B
volba=sqrt(sum(EWMAba$C)*(1-lambda))
