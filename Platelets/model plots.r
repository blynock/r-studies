# have to run import.r and then lasso.r to get the data set up for this

require(ggplot2)

stats.model.lasso = data.frame(true=y)
stats.model.lasso$obs <- seq(1, 85,1)
stats.model.lasso$predicted <- predict(model.lasso, s = cvfit$lambda.min, newx = x)
stats.model.lasso$residuals <- stats.model.lasso$true - stats.model.lasso$predicted

stats.model.lasso.log = data.frame(true=y.log)
stats.model.lasso.log$obs <- seq(1, 85,1)
stats.model.lasso.log$predicted <- predict(model.lasso.log, s = cvfit$lambda.min, newx = x)
stats.model.lasso.log$residuals <- stats.model.lasso.log$true - stats.model.lasso.log$predicted


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


# AIC model plot
ggplot(data=model.forwards.suggested.aic, aes(x=obs, y=residuals)) + 
  geom_point() + 
  geom_smooth(method="loess",color="red") + 
  geom_abline(slope=0, intercept=0, color="#888888", size=1)

plot(model.forwards.suggested.aic,1)
