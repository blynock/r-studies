# Fitting a negative binomial model

model.nb.sim.1 <- glm.nb(y ~ ., data = data.sim.1)
summary(model.nb.sim.1)
rootogram.nb.sim.1 <- rootogram(model.nb.sim.1, plot=FALSE, max=max(data.sim.1$y))
autoplot(rootogram.nb.sim.1)
ggsave(file="images/rootogram_nb_sim.png", height=2, width=7)


model.nb.full <- glm.nb(PostLikes ~ ., data = facebook.proc)
summary(model.nb.full)
AIC(model.nb.full)
rootogram.nb.full <- rootogram(model.nb.full, plot=FALSE, max=208)
autoplot(rootogram.nb.full)

model.nb.2 <- glm.nb(PostLikes ~ . - PageLikes - Month - Paid, data = facebook.proc)
summary(model.nb.2)
AIC(model.nb.2)
rootogram.nb.2 <- rootogram(model.nb.2, plot=FALSE, max=208)
autoplot(rootogram.nb.2)
ggsave(file="images/rootogram_nb_2.png", height=2, width=7)
