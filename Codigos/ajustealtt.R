KC1=Quandl("CHRIS/CME_MGC1", api_key="zxdSEzha_e_UwhD8Pgdw",type="xts", start_date="2018-10-02", end_date="2020-10-02")
KC1<-na.locf(KC1,na.rm = TRUE)
kcq=Delt(KC1$Settle)[-1]
library(MASS)

res1=fitdistr(kcq, "t")


res1$estimate
