# Poisson & NegBin Hurdle

model.hurdle.poisson.sim <- hurdle(y ~ x1, dist="poisson", data=data.sim.2, zero.dist="binomial", link="logit")
summary(model.hurdle.poisson.sim); AIC(model.hurdle.poisson.sim)
rootogram.hurdle.poisson.sim <- rootogram(model.hurdle.poisson.sim, plot=FALSE)
autoplot(rootogram.hurdle.poisson.sim)

AIC(model.poisson.fb)

model.hurdle.poisson.fb <- hurdle(PostLikes ~ Type + Category + Paid + Weekend, dist="poisson", data=facebook.proc, zero.dist="binomial", link="logit")
summary(model.hurdle.poisson.fb); AIC(model.hurdle.poisson.fb)
rootogram.hurdle.poisson.fb <- rootogram(model.hurdle.poisson.fb, plot=FALSE, max=210)
autoplot(rootogram.hurdle.poisson.fb)


model.hurdle.negbin.fb <- hurdle(PostLikes ~ Type + Category + Weekend, dist="negbin", data=facebook.proc, zero.dist="binomial", link="logit")
summary(model.hurdle.negbin.fb); 
AIC(model.hurdle.negbin.fb)

rootogram.hurdle.negbin.fb <- rootogram(model.hurdle.negbin.fb, plot=FALSE, max=210)
autoplot(rootogram.hurdle.negbin.fb)


