# Wilson interval
IC.Wilson<-function(datos, level=0.95){
 N<-sum(datos)
 k<-length(datos)
 pi.g<-datos/N
 Z<-abs(qnorm((1-level)/(2*k)))
 
 A<-N/(N+Z^2)
 B<-Z^2/(N+Z^2)
 pi.adj<-A*pi.g+B*1/2
 
 error<-sqrt((1/(N+Z^2))*(A*(pi.g*(1-pi.g))+B*1/4))
 
 LI<-pi.adj-error
 LI<-ifelse(LI<0,0,LI)
 LS<-pi.adj+error
 LS<-ifelse(LS>1,1,LS)
 Volumen<-prod(LS-LI)
 list(LI=LI,LS=LS,Volumen=Volumen)
}