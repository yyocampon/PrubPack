ro<-function(c1,x){
 n<-sum(x)
 k<-length(x)
 p.temp<-1
 for(i in 1:k){
 lam.i<-x[i]
 ai<-x[i]-c1
 bi<-x[i]+c1
 p.temp<-p.temp*(ppois(bi,lam.i)-ppois(ai-1,lam.i))
 }
 p<-1/dpois(n,n)*p.pois.trunc(c1,x)*p.temp
 return(p)
}