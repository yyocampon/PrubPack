# Bootstrap Interval 
IC.boot<-function(datos, level=0.95,Nboot=1000){
calcula.pi<-function(datos){
datos<-ifelse(datos==0,0.5,datos)
N<-sum(datos)
return(datos/N)
   }
   
alfa<-1-level
N<-sum(datos)
k<-length(datos)
pi.g<-datos/N
 
temp<-apply(apply(rmultinom(Nboot,N,prob=pi.g),2,calcula.pi),
           1,quantile,probs=c(alfa/k,1-alfa/k))
Volumen<-prod(temp[2,]-temp[1,])
 
list(LI=temp[1,],LS=temp[2,],Volumen=Volumen)
}
