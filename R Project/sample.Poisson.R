library(car)

# Ornstein's data (a lot of this analysis is taken from Fox and Weisberg:
# An R companion to Applied Regression)

library(ggplot2)
ggplot(Ornstein,aes(interlocks,fill=nation))+geom_histogram(binwidth=4,position="dodge")

# First model
model1 <- glm(interlocks ~ . , family = poisson, data = Ornstein)
crPlots(model1,"assets")

# Second model
model2<-glm(interlocks~nation+sector+log2(assets),family=poisson,data=Ornstein)
crPlots(model2,"log2(assets)")

# Is there significant evidence for effects?
Anova(model2)

# Effects on response scale:
exp(coef(model2))

# Plotting effects
library(effects)
plot(allEffects(model2,default.levels=50))

# Mean-variance relationship
with(Ornstein,tapply(interlocks,nation,function(x){
  sprintf("mu = %1.2f, var=%1.2f",mean(x),var(x))}))

# Robust standard-error estimation
library(sandwich)
Cov.Matrix<-vcovHC(model2,type="HC0")
std.err<-sqrt(diag(Cov.Matrix))
Robust.Est<-cbind(Estimate=coef(model2),'SE'=std.err,
         'Pr(>|z|)'= 2*pnorm(abs(coef(model2)/std.err), lower.tail=FALSE))

# Test for overdispersion
library(AER)
dispersiontest(model2)

#Tests adjusted for overdispersion
Anova(model2,"F")

# Fitting a quasi-Poisson model
model3<- glm(interlocks ~ log2(assets) + nation + sector, family = quasipoisson,
    data = Ornstein)
summary(model3)


# Fitting a negative binomial model
model4<- glm.nb(interlocks ~ log2(assets) + nation + sector,
       data = Ornstein)
summary(model4)

# Claims Data
# The data is available from this website:
# http://www.businessandeconomics.mq.edu.au/our_departments/Applied_Finance_and_Actuarial_Studies/research/books/GLMsforInsuranceData/data_sets
# Load into Excel and then save as CSV file with name insurance.csv

insurance <- read_csv("insurance.csv")
y<-insurance$claims
N<-insurance$population
x<-insurance$accidents

claim1<-glm(y~log(x)+offset(log(N)),family=poisson)
summary(claim1)

plot(log(x),residuals(claim1,type="pearson"),xlab="log(accidents)",ylab="Pearson residuals",pch=16)

library(AER)
dispersiontest(claim1,trafo=2)

library(MASS)
claim2<-glm.nb(y~log(x)+offset(log(N)))

plot(log(x),residuals(claim2,type="pearson"),xlab="log(accidents)",ylab="Pearson residuals",pch=16)

as.numeric(2*(logLik(claim2)-logLik(claim1)))
