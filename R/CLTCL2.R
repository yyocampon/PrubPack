TCL.II<-function(datos, level=0.95){
 N<-sum(datos)
 k<-length(datos)
 pi.g<-datos/N
 Z<-abs(qnorm((1-level)/(k-1)))
 theta<-Z/sqrt(N)
 theta2<-theta^2
 denominador<-1/(2*(1+theta2))
 B<-theta*sqrt(theta2+4*pi.g*(1-pi.g))
 A<-2*pi.g+theta2

LI<-(A-B)*denominador
LI<-ifelse(LI<0,0,LI)
LS<-(A+B)*denominador
LS<-ifelse(LS>1,1,LS)
Volumen<-prod(LS-LI)
list(LI=LI,LS=LS,Volumen=Volumen)
}
