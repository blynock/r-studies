library(bootstrap)

k_fold_rsq <- function(lmfit, ngroup=10) {
  # assumes library(bootstrap)
  # adapted from http://www.statmethods.net/stats/regression.html
  mydata <- lmfit$model
  outcome <- names(lmfit$model)[1]
  predictors <- names(lmfit$model)[-1]
  
  theta.fit <- function(x,y){lsfit(x,y)}
  theta.predict <- function(fit,x){cbind(1,x)%*%fit$coef} 
  X <- as.matrix(mydata[predictors])
  y <- as.matrix(mydata[outcome]) 
  
  results <- crossval(X,y,theta.fit,theta.predict,ngroup=ngroup)
  raw_rsq <- cor(y, lmfit$fitted.values)**2 # raw R2 
  cv_rsq <- cor(y,results$cv.fit)**2 # cross-validated R2
  
  c(raw_rsq=raw_rsq, cv_rsq=cv_rsq)
}

k_fold_rsq(model.lasso.log)

k_fold_rsq(model.forwards.suggested.bic,ngroup=42)
k_fold_rsq(model.forwards.suggested.aic,ngroup=42)
k_fold_rsq(model.backwards.suggested.aic,ngroup=42)
k_fold_rsq(model.backwards.suggested.bic,ngroup=42)
k_fold_rsq(model.full, ngroup=42)
k_fold_rsq(model.ridge, ngroup=42)


tidy(model.backwards.suggested.aic)