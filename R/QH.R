# Quesenberry and Hurst

QH<-function(datos, nivel=0.95){
 N<-sum(datos)
 k<-length(datos)
 Chi<-qchisq(nivel,k-1)
 temp<-sqrt(Chi*(Chi+4*datos*(N-datos)/N))
 temp1<-Chi+2*datos
 temp2<-2*(N+Chi)
 LI<-(temp1-temp)/temp2 
 LS<-(temp1+temp)/temp2
 Volumen<-prod(LS-LI)
 list(LI=LI,LS=LS,Volumen=Volumen)
}
