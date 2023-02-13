# Likelihood ratio IC
IC.RV<-function(datos,level=0.95){
 N<-sum(datos)
 k<-length(datos)
 pi.g<-datos/N
 alfa=1-level
 
 funci<-function(pis,pi.g=pi.g,N=N,datos=datos,alfa=alfa){
 k<-length(datos)
 temp<-abs(-2*sum(datos*log(pis/pi.g))
        -qchisq(1-alfa/k,k-1,lower.tail=T))
 return(temp)
 }
 
temp1<-optim(pi.g/2,funci,method='L-BFGS-B',lower=0,upper=pi.g,
pi.g=pi.g,N=N,datos=datos,alfa=alfa)  
temp2<-optim((1+pi.g)/2,funci,method='L-BFGS-B',lower=pi.g,upper=1,
pi.g=pi.g,N=N,datos=datos,alfa=alfa)
list(temp1=temp1,temp2=temp2)
}
