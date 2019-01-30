# Fitting and testing Poisson models on facebook data

n = nrow(facebook.proc)

model.poisson.full <- glm(PostLikes ~ ., family="poisson", data=facebook.proc)
model.poisson.full.stats.pchi2 <- sum(residuals(model.poisson.full, type="pearson")^2)
model.poisson.full.stats.dev <- sum(residuals(model.poisson.full, type="dev")^2)
model.poisson.full.stats.dispersion <- model.poisson.full.stats.pchi2/model.poisson.full$df.residual;
model.poisson.full.stats.dispersion2 <- model.poisson.full.stats.dev/model.poisson.full$df.residual;

summary(model.poisson.full)
extractAIC(model.poisson.full, k=log(n))
model.poisson.full.stats.dispersion

model.poisson.2 <- glm(PostLikes ~ . - Month, family="poisson", data=facebook.proc)
model.poisson.2.stats.pchi2 <- sum(residuals(model.poisson.full, type="pearson")^2)
model.poisson.2.stats.dispersion <- model.poisson.full.stats.pchi2/model.poisson.full$df.residual;

summary(model.poisson.2)
confint(model.poisson.2)
extractAIC(model.poisson.2, k=log(n))
model.poisson.2.stats.dispersion

# Robust standard-error estimation
library(sandwich)
coefs <- coef(model.poisson.full)
Cov.Matrix<-vcovHC(model.poisson.full,type="HC0")
std.err<-sqrt(diag(Cov.Matrix))
Robust.Est<-cbind(Estimate=coefs,'SE'=std.err,
                  'Pr(>|z|)'= 2*pnorm(abs(coefs/std.err), lower.tail=FALSE))

# predicted count + CI (from MCR book)``
lpred <- predict(model.poisson.full, newdata=facebook.proc, se.fit=TRUE)
up <- (lpred$fit + 1.96*lpred$se.fit); lo <- (lpred$fit - 1.96*lpred$se.fit)
eta <- lpred$fit;
mu <- model.poisson.full$family$linkinv(eta);
upci <- model.poisson.full$family$linkinv(up); 
loci <- model.poisson.full$family$linkinv(lo);

plot(loci, col="red", type="l")
lines(mu)
lines(upci, col="red")

plot(cbind(loci, mu, upci))


# cross-validated predictions
library(boot)
(cv.glm(facebook.proc, model.poisson.full))$delta[[2]]
(cv.glm(facebook.proc, model.poisson.2))$delta[[2]]


require(MASS)
confint(model.poisson.full)
confint(model.quasipoisson.full)

require(glmnet)

# using LASSO to fit Poisson?

# cross-validated fit
cvfit = cv.glmnet(
  x=data.matrix(facebook.proc[,1:6]),
  y=facebook.proc$PostLikes,
  family="poisson",
  alpha=1,
  nfolds=85
)
coef(cvfit, cvfit$lambda.min)

plot(cvfit)


fit = glm(count ~ treatment,family="poisson",data=data) 
fit.overdisp = glm(count ~ treatment,family="quasipoisson",data=data) 
summary(fit.overdisp)$dispersion # dispersion coefficient
pchisq(summary(fit.overdisp)$dispersion * fit$df.residual, fit$df.residual, lower = F) # significance for overdispersion

