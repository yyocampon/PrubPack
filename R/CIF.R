# Interval based on F
IC.F<-function(datos, level=0.95){
 N<-sum(datos)
 k<-length(datos)
 alfa<-1-level
 gl1<-2*(datos+1)
 gl2<-2*(N-datos)
 
 LS<-1/(1+(N-datos)/((datos+1)*qf(alfa/(2*k),gl1,gl2,lower.tail=F)))
 
 gl1<-2*datos
 gl2<-2*(N-datos+1)
 
 LI<-1/(1+(N-datos+1)/(datos*qf(1-alfa/(2*k),gl1,gl2,lower.tail=F)))
 Volumen<-prod(LS-LI)
 list(LI=LI,LS=LS,Volumen=Volumen)
}
