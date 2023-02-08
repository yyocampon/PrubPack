# Maximum likelihood point estimator
ML.estimator<-function(datos,ContCorrection=0.5){
 datos<-ifelse(datos==0,ContCorrection,datos)
 N<-sum(datos)
 k<-length(datos)
 pi.g<-datos/N
 pi.g<-matrix(pi.g,ncol=1)
 VarCov<-(diag(pi.g)-pi.g%*%t(pi.g))/N
 list(estimator=pi.g,VarCov=VarCov)
}
