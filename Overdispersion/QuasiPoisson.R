n = nrow(facebook.proc)

model.quasipoisson.full <- glm(PostLikes ~ ., family="quasipoisson", data=facebook.proc)
model.quasipoisson.full.stats.pchi2 <- sum(residuals(model.quasipoisson.full, type="pearson")^2)
model.quasipoisson.full.stats.dispersion <- model.quasipoisson.full.stats.pchi2/model.quasipoisson.full$df.residual;

summary(model.quasipoisson.full)
extractAIC(model.quasipoisson.full, k=log(n))
model.quasipoisson.full.stats.dispersion


model.quasipoisson.2 <- glm(PostLikes ~ . - Month, family="quasipoisson", data=facebook.proc)
model.quasipoisson.2.stats.pchi2 <- sum(residuals(model.quasipoisson.2, type="pearson")^2)
model.quasipoisson.2.stats.dispersion <- model.quasipoisson.2.stats.pchi2/model.quasipoisson.2$df.residual;
summary(model.quasipoisson.2)
confint(model.quasipoisson.2)
extractAIC(model.quasipoisson.2, k=log(n))
model.quasipoisson.2.stats.dispersion


model.quasipoisson.3 <- glm(PostLikes ~ PageLikes + Category + Type + Weekend + Paid, family="quasipoisson", data=facebook.proc)
model.quasipoisson.3.stats.pchi2 <- sum(residuals(model.quasipoisson.3, type="pearson")^2)
model.quasipoisson.3.stats.dispersion <- model.quasipoisson.3.stats.pchi2/model.quasipoisson.2$df.residual;
summary(model.quasipoisson.3)
confint(model.quasipoisson.3)
extractAIC(model.quasipoisson.3, k=log(n))
model.quasipoisson.3.stats.dispersion



confint(model.poisson.2)
confint(model.quasipoisson.2)

plot(model.poisson.2)

plot(model.quasipoisson.2)


library(boot)
(cv.glm(facebook.proc, model.quasipoisson.full))$delta[[2]]
(cv.glm(facebook.proc, model.quasipoisson.2))$delta[[2]]
(cv.glm(facebook.proc, model.quasipoisson.3))$delta[[2]]

plot(model.quasipoisson.3)
