# Interval based on Goodman
IC.Goodman<-function(datos, level=0.95){
 N<-sum(datos)
 k<-length(datos)
 pi.g<-datos/N
 alfa<-1-level
 Chi<-qchisq(alfa/k,1,lower.tail=F)
temp<-sqrt(Chi*pi.g*(1-pi.g)/N)
LI<-pi.g-temp
LI<-ifelse(LI<0,0,LI)
 LS<-pi.g+temp
 LS<-ifelse(LS>1,1,LS)
 Volumen<-prod(LS-LI)
 list(LI=LI,LS=LS,Volumen=Volumen)
}
