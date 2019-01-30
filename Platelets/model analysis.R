# have to run import.r lasso.r to get the data set up for this

# import the data and build all the models
source("import.r")
source("stepwise.r")
source("lasso.r")

require(ggplot2)

stats.model.lasso = data.frame(true=y)
stats.model.lasso$obs <- seq(1, 85,1)
stats.model.lasso$predicted <- predict(model.lasso, s = lambda.lasso, newx = x)
stats.model.lasso$residuals <- stats.model.lasso$true - stats.model.lasso$predicted

stats.model.lasso.log = data.frame(true=y.log)
stats.model.lasso.log$obs <- seq(1, 85,1)
stats.model.lasso.log$predicted <- predict(model.lasso.log, s = lambda.lasso.log, newx = x)
stats.model.lasso.log$residuals <- stats.model.lasso.log$true - stats.model.lasso.log$predicted

stats.model.ridge = data.frame(true=y)
stats.model.ridge$obs <- seq(1, 85,1)
stats.model.ridge$predicted <- predict(model.ridge, s = lambda.ridge, newx = x)
stats.model.ridge$residuals <- stats.model.ridge$true - stats.model.ridge$predicted

# LASSO model plots

# plot non-log response model residuals
ggplot(data=stats.model.lasso, aes(x=obs, y=residuals)) + 
  geom_point() + 
  geom_smooth(method="loess",color="red") + 
  geom_abline(slope=0, intercept=0, color="#888888", size=1)

# plot log model response residuals
ggplot(data=stats.model.lasso.log, aes(x=obs, y=residuals)) + 
  geom_point() + 
  geom_smooth(method="loess",color="red") + 
  geom_abline(slope=0, intercept=0, color="#888888", size=1)

# RIDGE model plot
ggplot(data=stats.model.ridge, aes(x=obs, y=residuals)) + 
  geom_point() + 
  geom_smooth(method="loess",color="red") + 
  geom_abline(slope=0, intercept=0, color="#888888", size=1)

# Stepwise model plots
# AIC forwards
m <- model.forwards.suggested.aic
ggplot(
  data=data.frame(true=y, obs=seq(1,85), fitted=fitted.values(m), residuals=residuals(m)), 
  aes(x=fitted, y=residuals)) +
  geom_point() + 
  geom_smooth(method="loess",color="red") + 
  geom_abline(slope=0, intercept=0, color="#888888", size=1)+
  ggtitle("Stepwise Forwards AIC")

m <- model.backwards.suggested.aic
ggplot(data=data.frame(true=y, obs=seq(1,85), fitted=fitted.values(m), residuals=residuals(m)), aes(x=fitted, y=residuals), main="backwards AIC") + 
  geom_point() + 
  geom_smooth(method="loess",color="red") + 
  geom_abline(slope=0, intercept=0, color="#888888", size=1)+
  ggtitle("Stepwise Backwards AIC")

m <- model.forwards.suggested.bic
ggplot(data=data.frame(true=y, obs=seq(1,85), fitted=fitted.values(m), residuals=residuals(m)), aes(x=fitted, y=residuals)) + 
  geom_point() + 
  geom_smooth(method="loess",color="red") + 
  geom_abline(slope=0, intercept=0, color="#888888", size=1)+
  ggtitle("Stepwise Forwards BIC")

m <- model.backwards.suggested.bic
ggplot(data=data.frame(true=y, obs=seq(1,85), fitted=fitted.values(m), residuals=residuals(m)), aes(x=fitted, y=residuals)) + 
  geom_point() + 
  geom_smooth(method="loess",color="red") + 
  geom_abline(slope=0, intercept=0, color="#888888", size=1)+
  ggtitle("Stepwise Backwards BIC")
