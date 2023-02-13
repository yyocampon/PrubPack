# Confidence Interval based on Baily

IC.Baily1<-function(datos,level=0.95){
 N<-sum(datos)
 k<-length(datos)
 pi.p<-(datos+3/8)/(N+3/4)
 alfa<-1-level
 Chi<-qchisq(alfa/k,1,lower.tail=F)
 temp<-Chi/sqrt(4*N+2)
 LI<-sin(asin(sqrt(pi.p))-temp)^2
 LS<-sin(asin(sqrt(pi.p))+temp)^2
 LI<-ifelse(LI<0,0,LI)
 LS<-ifelse(LS>1,1,LS)
 Volumen<-prod(LS-LI)
 list(LI=LI,LS=LS,Volumen=Volumen)
}
