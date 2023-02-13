p.pois.trunc<-function(c1,x){
k<-length(x)
s1<-0
s2<-0
for(i in 1:k){
  lam<-x[i]
  a<-lam-c1
  b<-lam+c1
  s1<-s1+media.trun(lam,a,b)
  s2<-s2+var.trun(lam,a,b)

}
 z1<-(sum(x)+0.5-s1)/sqrt(s2)
 z2<-(sum(x)-0.5-s1)/sqrt(s2)
 p<-pnorm(z1)-pnorm(z2)
 return(p)
}