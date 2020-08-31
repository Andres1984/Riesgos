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

volmsft=0.03
volappl=0.023

pab=0.6
pac=0.4
pbc=0.75


varcovar=matrix(c(volba*volba,pab*volba*volmsft,pac*volba*volappl,pab*volba*volmsft,volmsft*volmsft,
                  pbc*volmsft*volappl,pac*volba*volappl,pbc*volmsft*volappl,volappl*volappl),nrow = 3,ncol = 3)

varcovar=varcovar*20


mu.vec=cbind(0.02,0.03,0.04)


# Portafolio mínimo global volewma

## Crear matriz A
n=3
one.vec=rep(1,n)
topmat=cbind(2*varcovar,one.vec)
botmat=c(one.vec,0)
Amat=rbind(topmat,botmat)
bvec=c(rep(0,n),1)
z=solve(Amat)%*%bvec
wmin=z[1:n]
names(wmin)=c("AMZN","AAPL","GOOG")
mmin=mu.vec%*%wmin
sigma2min=t(wmin)%*%varcovar%*%wmin
sigmamin=sqrt(sigma2min)
wmin
mmin
sigmamin


barplot(wmin,col="blue")
