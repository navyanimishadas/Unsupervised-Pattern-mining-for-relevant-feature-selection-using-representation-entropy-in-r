Rankfea<-function(data)
{
source("repen.R")
m<-dim(data)[1]
n<-dim(data)[2]
ha<-repen(data)
hi<-c()
for (i in 1:n)
{
 dd<-data[,-i]
 mnth<-repen(dd)
 hi<-c(hi,mnth[2])
}
ce<-hi-ha[2]
p<-sort(ce,decreasing = TRUE,index.return = TRUE)
ranking<-p$ix
return(p$ix)
}



