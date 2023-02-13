# Interval based on the TCL with continuity correction
TCL.corr<-function(datos, level=0.95){
N<-sum(datos)
k<-length(datos)
pi.g<-datos/N
Z<-abs(qnorm((1-level)/(2*k)))+1/(2*N)
temp<-Z*sqrt(pi.g*(1-pi.g)/N)
LI<-pi.g-temp
LI<-ifelse(LI<0,0,LI)
LS<-pi.g+temp
LS<-ifelse(LS>1,1,LS)
Volumen<-prod(LS-LI)
list(LI=LI,LS=LS,Volumen=Volumen)
}
