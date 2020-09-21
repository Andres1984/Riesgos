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
plot(KC1$Settle)
KC1=tail(KC1$Settle,510)
plot(KC1)
CL3=tail(CL3$Last,510)
SI3=tail(SI3$Last,510)

plot(CL3)



C2=Quandl("CHRIS/LIFFE_C2", api_key="zxdSEzha_e_UwhD8Pgdw",type="xts")

