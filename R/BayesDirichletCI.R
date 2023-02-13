# Bayesian CI
IC.dirichlet<-function(datos,level=0.95){
N<-sum(datos)
k<-length(datos)
alfa=1-level

gl1<-datos+1
gl2<-N-datos+k-1

LI<-qbeta(alfa/(2*k),gl1,gl2)
LS<-qbeta(1-alfa/(2*k),gl1,gl2)
Volumen<-prod(LS-LI)
list(LI=LI,LS=LS,Volumen=Volumen) 
}
