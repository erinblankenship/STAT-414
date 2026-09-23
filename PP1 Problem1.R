acls <- data.frame(
  discipline=c("Literature","Classics","Philosophy","History","Linguistics","PoliSci","Sociology"),
  poptotal=c(9100,1950,5500,10850,2100,5500,9000),
  returns=c(636,451,481,611,493,575,588),
  percfem=c(38,27,18,19,36,13,26),
  percagree=c(37,23,23,29,19,43,41))

acls$females<-with(acls, round(returns * (percfem/100)))

acls$males<-with(acls, returns - females)

aclsfem<-acls[rep(1:nrow(acls),times=acls$females),]
aclsfem$femid<-1
aclsmale<-acls[rep(1:nrow(acls),times=acls$males),]
aclsmale$femid<-0
aclsdf<-rbind(aclsfem,aclsmale)
dim(aclsdf)
head(aclsdf)
