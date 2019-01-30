#ZINF

model.zinb.full <- zeroinfl(PostLikes ~ Type + Category + Paid + Weekend, data = facebook.proc, dist = "negbin")

summary(model.zinb.full)

model.zinb.rootogram <- rootogram(model.zinb.full, plot=FALSE)

autoplot(model.zinb.rootogram)

