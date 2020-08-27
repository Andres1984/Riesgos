## Como crear la matriz varcovar para el portafolio, revisar el libro de medición y control 
## de riesgos financieros, el solo copiar y pegar este codigo no garantiza el exito
## del ejercicio
#Vol ewma
ewa=0.1 ## Vol ewma debe estar diaría
ewb=0.3
ewc=0.12

pab=0.65 # Correlacion
pac=0.53
pbc=0.20

#Se crea el vector

a=c(ewa*ewa,pab*ewa*ewb,pac*ewa*ewc,pab*ewa*ewb,ewb*ewb,pbc*ewb*ewc,pac*ewa*ewc,pbc*ewb*ewc,ewc*ewc)

#Matriz Varcovar

cov=matrix(a,nrow=3,ncol=3)
cov=cov*20
