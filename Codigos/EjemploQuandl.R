## Proceso básico para descargar los precios de Quandl
## Recuerden cambiar el api_key="zxdSEzha_e_UwhD8Pgdw"
library(Quandl)
CL3=Quandl("CHRIS/CME_GC3", api_key="zxdSEzha_e_UwhD8Pgdw",type = "xts")
plot(CL3$Last)
SI3=Quandl("CHRIS/CME_SI3", api_key="zxdSEzha_e_UwhD8Pgdw",type="xts")
plot(SI3$Last)


CL3=tail(CL3$Last,510)
SI3=tail(SI3$Last,510)

plot(CL3)
