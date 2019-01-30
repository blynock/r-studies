# sample code from MCD to generate "true" synthetic Poisson model

# clean up - remove all vars related to this script from environment
# rm(nobs, disp, pchi2, py, py1, B, mysim, x1, x2, x3, dataf, datafall, cnt, poic, pr, rwm1984, rwm5yr, poic2)

library(dplyr)
library(MASS); library(COUNT); set.seed(4590); 

# create data; nobs random samples from uniform 0,1
nobs <- 50000; x1 <- runif(nobs); x2 <- runif(nobs); x3 <- runif(nobs)

py <- rpois(nobs, exp(1 + 0.75*x1 - 1.25*x2 + .5*x3))
cnt <- table(py)
dataf <- data.frame(prop.table(table(py) ) )
dataf$cumulative <- cumsum(dataf$Freq)
datafall <- data.frame(cnt, dataf$Freq*100, dataf$cumulative * 100)
datafall; summary(py)
summary(py1 <- glm(py ~ x1 + x2 + x3, family=poisson))
confint.default(py1); py1$aic/(py1$df.null+1)
pr <- resid(py1, type = "pearson")
pchi2 <- sum(residuals(py1, type="pearson")^2)
disp <- pchi2/py1$df.residual; pchi2; disp


sim <- function(coefficients, num.observations=50000, formula=NULL, excess.zeros=0)
{
  # generate random uniform predictors
  num.predictors = length(coefficients)
  # create predictor matrix (intercept term)
  pred.mat <- matrix(rep(1,num.observations),nrow=num.observations)
  # add other predictors to matrix
  for(i in 2:num.predictors){
    vals <- runif(num.observations)
    pred.mat <- cbind(pred.mat, vals)
    if(i==2){
      # create a data frame to fit model to later
      df <- data.frame(x1=vals)
    }
    else{
      # add next predictor to data frame
      df[, paste("x",i-1,sep="")] <- vals
    }
  }
  # generate observations from poisson with mean derived from exp(predictors * coefficients)
  df$y <- rpois(num.observations, exp(pred.mat %*% coefficients))
  
  # if we have an excess zero probability defined, convert some values to zeros
  if(excess.zeros>0 & excess.zeros < 1){
    df$y = ifelse(runif(num.observations) < excess.zeros, 0, df$y);
  }

  if(is.null(formula)){
    model.pois <- glm(y ~ ., family="poisson", data = df)
  }
  else{
    model.pois <- glm(formula, family="poisson", data=df)
  }
  summary(model.pois)
  pr <- sum(residuals(model.pois, type="pearson")^2)
  dr <- sum(residuals(model.pois, type="deviance")^2)
  prdisp <- pr/model.pois$df.residual
  devdisp <- dr/model.pois$df.residual
  return(list(data=df, model=model.pois, pearson.dispersion=prdisp,deviance.dispersion=devdisp))
}



B <- replicate(100, mysim())

# Coefficients: intercept and x1, x2, x3
apply(matrix(unlist(B[1,]),4,100),1,mean)

# Dispersion
mean(unlist(B[2,]))


#############
# model from real data
#############
library(COUNT)
data(rwm5yr); rwm1984 <- subset(rwm5yr, year==1984)
rwm1984$cage = rwm1984$age - mean(rwm1984$age)

summary(poic <- glm(docvis ~ outwork + cage, family=poisson, data=rwm1984))
pr <- sum(residuals(poic, type="pearson")^2)
pr/poic$df.residual
modelfit(poic)

summary(poic2 <- glm(docvis ~ outwork + cage + edlevel, family=poisson, data=rwm1984))
pr <- sum(residuals(poic2, type="pearson")^2)
pr/poic2$df.residual
modelfit(poic2)

cnt <- table(rwm1984$docvis)
dataf <- data.frame(prop.table(cnt ) )
dataf$cumulative <- cumsum(dataf$Freq)
datafall <- data.frame(cnt, dataf$Freq*100, dataf$cumulative * 100)
datafall

rwm1984$edlevel <- as.factor(rwm1984$edlevel)
str(rwm1984)
levels(rwm1984$edlevel)[1] <- "Not HS grad"
levels(rwm1984$edlevel)[2] <- "HS"
levels(rwm1984$edlevel)[3] <- "Coll/Univ"
levels(rwm1984$edlevel)[4] <- "Grad School"

levels(rwm1984$edlevel)





m<- c(0.5,1,3,5) #Poisson means
y<- 0:11 #Observed counts
layout(1)
for (i in 1:length(m)) {
  p<- dpois(y, m[i]) #poisson pdf
  if (i==1) {
    plot(y, p, col=i, type="l", lty=i)
  } else {
    lines(y, p, col=i, lty=i)
  }
}
