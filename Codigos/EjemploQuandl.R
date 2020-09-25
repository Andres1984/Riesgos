## Proceso básico para descargar los precios de Quandl
## Recuerden cambiar el api_key="zxdSEzha_e_UwhD8Pgdw"
## El vencimiento del contrato depende del grupo al que pertenece
## Grupo 1 un mes ejemplo CL1
## Grupo 2 dos meses CL2
## Grupo 3 tres meses CL3
## Grupo 4 cuatro meses CL4

library(Quandl)
CL3=Quandl("CHRIS/CME_GC3", api_key="zxdSEzha_e_UwhD8Pgdw",type = "xts")
plot(CL3$Last)
SI3=Quandl("CHRIS/CME_SI3", api_key="zxdSEzha_e_UwhD8Pgdw",type="xts")
plot(SI3$Last)

KC1=Quandl("CHRIS/ICE_KC1", api_key="zxdSEzha_e_UwhD8Pgdw",type="xts")
PL1=Quandl("CHRIS/CME_PL1", api_key="zxdSEzha_e_UwhD8Pgdw",type="xts")
QG1=Quandl("CHRIS/CME_QG1", api_key="zxdSEzha_e_UwhD8Pgdw",type="xts")
LN1=Quandl("CHRIS/CME_LN1", api_key="zxdSEzha_e_UwhD8Pgdw",type="xts")

library(quantmod)

KC1=tail(KC1$Settle,510)
PL1=tail(PL1$Settle,510)
QG1=tail(QG1$Settle,510)
LN1=tail(LN1$Settle,510)

index.notNA<-which(is.na(coredata(PL1$Settle))==FALSE)

kc1=Delt(KC1)[-1]
pl=Delt(PL1)[-1]
qg=Delt(QG1)[-1]
kn=Delt(LN1)[-1]

sum(is.na(rend))
rend=cbind(kc1,pl,qg,kn)
rend=rend[9:510,]
a=cor(rend)
a
library(corrplot)
apply(is.na(rend),2,sum)


library(xlsx)
write.xlsx(rend, "/Users/andresmartinez/Google Drive/ULasalle/2020-II/Riesgos/Rendimientos.xlsx") 



corrplot(a)

plot(KC1$Settle)
KC1=tail(KC1$Settle,510)
plot(KC1)
CL3=tail(CL3$Last,510)
SI3=tail(SI3$Last,510)

plot(CL3)


Cocoa=Quandl("CHRIS/LIFFE_C2", api_key="zxdSEzha_e_UwhD8Pgdw",type="xts")
C2=Quandl("CHRIS/LIFFE_C2", api_key="zxdSEzha_e_UwhD8Pgdw",type="xts")

Cocoa=tail(Cocoa$Settle,510)



Brent= Quandl("CHRIS/CME_BZ4", api_key="zxdSEzha_e_UwhD8Pgdw",type="xts")

Brent=tail(Brent$Last,510)

