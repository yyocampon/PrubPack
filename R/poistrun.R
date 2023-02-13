# Computation of the Estimate probability of truncated Poisson
pois.trunc<-function(x,lam,a,b){
if(x<a || x>b) res<-0
else{
if(a==0) res<-dpois(x,lam)/ppois(b,lam)
else res<-dpois(x,lam)/(ppois(b,lam)-ppois(a-1,lam))
}
res
}