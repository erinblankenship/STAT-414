alligator<-read.csv("alligator.csv",header=TRUE)
head(alligator)

n<-nrow(alligator)

##### Summary Stats ###########

xbar<-mean(alligator$Length)
xbar
ybar<-mean(alligator$Weight)
ybar
varx<-var(alligator$Length)
varx
vary<-var(alligator$Weight)
vary
corrxy<-cor(alligator$Length,alligator$Weight)
corrxy

bhat1<-corrxy*sqrt(vary)/sqrt(varx)
bhat0<-ybar-bhat1*xbar
bhat1
bhat0

xbarU<-90

ybarhatreg<-ybar+bhat1*(xbarU-xbar)
ybarhatreg

alligator$resid<-alligator$Weight - (bhat0+bhat1*alligator$Length)
s2e<-sum(alligator$resid**2)/(n-1)
s2e

#### lm function ################

alreg<-lm(Weight~Length,data=alligator)
alreg
anova(alreg)


####### survey package ###########

library(survey)
aldesign<-svydesign(id=~1, weights=~1, data=alligator)

alreg2<-svyglm(Weight~Length,design=aldesign)
summary(alreg2)

xbarU<-data.frame(Length=90)

predict(alreg2,xbarU)
confint(predict(alreg2,xbarU),df=n-2)
