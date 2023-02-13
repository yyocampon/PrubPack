IC.sison<-function(x,alpha=0.05,decimal=12.4){

n=sum(x)
print(c('n=',n))
k=length(x)
p=x/n
probn=1/(ppois(n,n)-ppois(n-1,n));
sqrt2pi=sqrt(2)*gamma(0.5);

pold=0

temp<-apply(matrix(1:n,ncol=1),1,ro,x)
temp2<-which.min(abs(temp-(1-alpha)))
print(c('c1=',temp2))
c1<-temp2
pold<-temp[c1-1]
p<-temp[c1]

delta=(1-alpha-pold)/(p-pold)
print(c('delta=',delta))
out=matrix(0,nrow=k,ncol=5)
num=matrix(0,nrow=k,ncol=1)
vol1=1
vol2=1

for(i in 1:k){
num[i,1]=i
obsp=x[i]/n
cn=c1/n
print(c('c/n=',cn))
onen=1/n
out[i,1]=obsp
out[i,2]=obsp-cn
out[i,3]=obsp+cn+2*delta/n
if( out[i,2]<0)  out[i,2]=0
if( out[i,3]>1)  out[i,3]=1
out[i,4]=obsp-cn-onen
out[i,5]=obsp+cn+onen
if( out[i,4]<0)  out[i,4]=0
if( out[i,5]>1)  out[i,5]=1
vol1=vol1*(out[i,3]-out[i,2])
vol2=vol2*(out[i,5]-out[i,4])
}

c11=c('Proportion', 'Lower(SG)','Upper(SG)',
     'Lower(C+1)','Upper(C+1)')
cove=100*(1-alpha)
sg=(x+delta)/n
c2='SG-midpoint'

cat('------------------------------------------------ \n')
cat( cove,' % Simultaneous confidence interval\n')
cat('-------------------------------------------------\n')
cat('C = ', c1,'\n')
print(p)
cat( 'Volume(SG) = ', vol1,'\n')
cat( 'Volume(C+1)= ', vol2,'\n')
print(c('prob. est.','LI I','LS I','LI II','LS II'))
print(out)

} # Fin SG