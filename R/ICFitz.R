# Fitzpatrick and Scott
IC.Fitz<-function(datos, level=0.95){
N<-sum(datos)
k<-length(datos)
pi.g<-datos/N
Z<-abs(qnorm((1-level)/2))
temp<-Z/(2*sqrt(N))
LI<-pi.g-temp
LI<-ifelse(LI<0,0,LI)
LS<-pi.g+temp
LS<-ifelse(LS>1,1,LS)
Volumen<-prod(LS-LI)
list(LI=LI,LS=LS,Volumen=Volumen)
}
