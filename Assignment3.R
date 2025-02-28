library(dplyr)

my.bootstrapci <-function(vec0,nboot=10000,alpha=0.1)
{
  n0<-length(vec0)
  mean0<-mean(vec0)
  sd0<-sqrt(var(vec0))
  bootvec<-NULL
  for( i in 1:nboot){
    vecb<-sample(vec0,replace=T)
    meanb<-mean(vecb)
    sdb<-sqrt(var(vecb))
    bootvec<-c(bootvec,(meanb-mean0)/(sdb/sqrt(n0)))
  }
  lq<-quantile(bootvec,alpha/2)
  uq<-quantile(bootvec,1-alpha/2)
  LB<-mean0-(sd0/sqrt(n0))*uq
  UB<-mean0-(sd0/sqrt(n0))*lq
  NLB<-mean0-(sd0/sqrt(n0))*qt(1-alpha/2,n0-1)
  NUB<-mean0+(sd0/sqrt(n0))*qt(1-alpha/2,n0-1)
  list(bootstrap.confidence.interval=c(LB,UB),normal.confidence.interval=c(NLB,NUB))}



Simfunc<-function(mu.val=3,n=30,nsim=1000)
{
  #%%%%%%%
  cvec.boot<-NULL
  cvec.norm<-NULL
  #%%%%%%%%%
  mulnorm<-(exp(mu.val+1/2))
  #$$$$$$$$$$$$$$
  for(i in 1:nsim){
    if((i/10)==floor(i/10)){
      print(i)
      #################
    }
    #%%%%%%%%%%%%%%%%
    vec.sample<-rlnorm(n,mu.val)
    #################
    boot.list<-my.bootstrapci.ml(vec.sample)
    boot.conf<-boot.list$bootstrap.confidence.interval
    norm.conf<-boot.list$normal.confidence.interval
    ##########################
    ###################################
    cvec.boot<-c(cvec.boot,(boot.conf[1]<mulnorm)*(boot.conf[2]>mulnorm))
    #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    cvec.norm<-c(cvec.norm,(norm.conf[1]<mulnorm)*(norm.conf[2]>mulnorm))
  }
  #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  list(boot.coverage=(sum(cvec.boot)/nsim),norm.coverage=(sum(cvec.norm)/nsim))
}



# Convert results list to a data frame
results_df <- do.call(rbind, lapply(names(results), function(name) {
  vals <- unlist(strsplit(name, "_"))
  data.frame(
    n = as.numeric(vals[2]),
    alpha = as.numeric(vals[4]),
    Bootstrap_Coverage = results[[name]]$boot.coverage,
    Normal_Coverage = results[[name]]$norm.coverage
  )
}))

print(results_df)





