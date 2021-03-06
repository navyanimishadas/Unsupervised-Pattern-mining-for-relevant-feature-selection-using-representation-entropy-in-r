repen<- function(data){
n<-dim(data)[2]
covMat<-cov(data)
e<-eigen(covMat)$values
Se<-sum(e)
normLanda<-e/Se
hr<-0
for (i in 1:n)
{
 hr<-(hr-normLanda[i]*log(normLanda[i],10))
}
hrn<- hr/log(n,10)
return(c(hr,hrn))
}

