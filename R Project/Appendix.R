# appendix code


# function which returns data simulated from poisson or neg binomial
sim <- function(coefficients, num.observations=50000, 
                excess.zeros=0, type="poisson")
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

  if(type=="poisson"){
    # generate observations from poisson
    df$y <- rpois(num.observations, exp(pred.mat %*% coefficients))
  }
  else if(type=="negbin"){
    # theta fairly arbitrarily set at 2
    theta = 2;
    # generate observations from negbin
    df$y <- rnegbin(num.observations, 
                    exp(pred.mat %*% coefficients), theta)
  }

  # if we have an excess zero probability defined, convert some 
  # values to zeros
  # this is not totally equivalent to the two stage described, 
  # but close enough for our purposes
  if(excess.zeros>0 & excess.zeros < 1){
    df$y = ifelse(runif(num.observations) < excess.zeros, 0, df$y);
  }
  return(df);
}

# function which returns both dispersion stats 
dispersionStats <- function(model){
  prdisp <- sum(residuals(model, type="pearson")^2)/model$df.residual
  devdisp <- sum(residuals(model, type="deviance")^2)/model$df.residual
  return(list(pearson.dispersion=prdisp,deviance.dispersion=devdisp))
}

#===== DATA SIMULATION ================================
set.seed(12)
data.sim.1 <- sim(c(2,1,-1,0.5,0), 5000, excess.zeros=0, type="poisson")
data.sim.2 <- sim(c(2,1,-1,0.5,0), 5000, excess.zeros=0.1, type="poisson")
data.sim.3 <- sim(c(2,1,-1,0.5,0), 5000, excess.zeros=0, type="negbin")
data.sim.4 <- sim(c(2,1,-1,0.5,0), 5000, excess.zeros=0.1, type="negbin")

#===== POISSON ========================================
library(countreg)

# poisson full fit to poisson data
model.poisson <- glm(y ~ ., family="poisson", data=data.sim.1)
summary(model.poisson)
dispersionStats(model.poisson)
rootogram.true.poisson <- rootogram(model.poisson, plot=FALSE, 
                                    max=max(data.sim.1$y))
autoplot(rootogram.true.poisson);
ggsave(file="images/rootogram_true_poisson.png", height=2, width=6.5)

# poisson fit with missing predictors to poisson data
model.poisson.missing.1 <- glm(y ~ x1, family="poisson", data=data.sim.1)
summary(model.poisson.missing.1)
AIC(model.poisson.missing.1)
dispersionStats(model.poisson.missing.1)
rootogram.poisson.missing.1 <- rootogram(model.poisson.missing.1, 
                                         plot=FALSE, max=max(data.sim.1$y))
autoplot(rootogram.poisson.missing.1); 
ggsave(file="images/rootogram_poisson_missing_1.png", height=2, width=6.5)

# poisson fit with only noise term to poisson data
model.poisson.noise <- glm(y ~ x4, family="poisson", data=data.sim.1)
summary(model.poisson.noise)
dispersionStats(model.poisson.noise)
rootogram.poisson.noise <- rootogram(model.poisson.noise, 
                                     max=max(data.sim.1$y), plot=FALSE)
autoplot(rootogram.poisson.noise); 
ggsave(file="images/rootogram_poisson_noise.png", height=2, width=6.5)

# poisson full fit to poisson data with excess zeros
model.poisson.excess.zeros <- glm(y ~ ., family="poisson", data=data.sim.2)
summary(model.poisson.excess.zeros)
dispersionStats(model.poisson.excess.zeros)
rootogram.poisson.excess.zeros <- rootogram(model.poisson.excess.zeros, 
                                            plot=FALSE, max=max(data.sim.2$y))
autoplot(rootogram.poisson.excess.zeros)
ggsave(file="images/rootograms_excess_zeros.png", height=2, width=6.5)

# poisson full fit to negbin data
model.poisson.negbin <- glm(y ~ ., family="poisson", data=data.sim.3)
summary(model.poisson.negbin)
dispersionStats(model.poisson.negbin)
rootogram.poisson.negbin <- rootogram(model.poisson.negbin, 
                                      plot=FALSE, max=max(data.sim.3$y))
autoplot(rootogram.poisson.negbin);
ggsave(file="images/rootogram_poisson_negbin.png", height=2, width=6.5)

# poisson fit to facebook data
model.poisson.fb <- glm(PostLikes ~ ., family="poisson", data=facebook.proc)
summary(model.poisson.fb)
deviance(model.poisson.fb)
AIC(model.poisson.fb)
dispersionStats(model.poisson.fb)
rootogram.poisson.fb <- rootogram(model.poisson.fb, plot=FALSE, max=208)
autoplot(rootogram.poisson.fb);
ggsave(file="images/rootogram_poisson_facebook.png", height=2, width=6.5)

#===== QUASIPOISSON ==========================================

# quasipoisson fit to poisson data
model.quasipoisson <- glm(y ~ ., 
                          family="quasipoisson", data=data.sim.1)
summary(model.quasipoisson)

# quasipoisson fit to negbin data
model.quasipoisson.negbin <- glm(y ~ ., 
                                 family="quasipoisson", data=data.sim.3)
summary(model.quasipoisson.negbin)
# note quasipoisson doesn't think noise parameter x4 is significant, as opposed to poisson

# quasipoisson fit to facebook data
model.quasipoisson.fb <- glm(PostLikes ~ ., 
                             family="quasipoisson", data=facebook.proc)
summary(model.quasipoisson.fb)
deviance(model.quasipoisson.fb)
# doesn't think PageLikes, Month, Paid are significant

#===== NEG BIN  ======================
library(MASS)
model.nb.sim.1 <- glm.nb(y ~ ., data = data.sim.1)
summary(model.nb.sim.1)
rootogram.nb.sim.1 <- rootogram(model.nb.sim.1, 
                                plot=FALSE, max=max(data.sim.1$y))
autoplot(rootogram.nb.sim.1)
ggsave(file="images/rootogram_nb_sim.png", height=2, width=6.5)

model.nb.sim.2 <- glm.nb(y ~ x1, data = data.sim.1)
summary(model.nb.sim.2)
AIC(model.nb.sim.2)
rootogram.nb.sim.2 <- rootogram(model.nb.sim.2, 
                                plot=FALSE, max=max(data.sim.1$y))
autoplot(rootogram.nb.sim.2)

model.nb.full <- glm.nb(PostLikes ~ ., data = facebook.proc)
summary(model.nb.full)
AIC(model.nb.full)
rootogram.nb.full <- rootogram(model.nb.full, 
                               plot=FALSE, max=208)
autoplot(rootogram.nb.full)

model.nb.2 <- glm.nb(PostLikes ~ Category + Type + Weekend, 
                     data = facebook.proc)
summary(model.nb.2)
AIC(model.nb.2)
rootogram.nb.2 <- rootogram(model.nb.2, plot=FALSE, max=208)
autoplot(rootogram.nb.2)
ggsave(file="images/rootogram_nb_2.png", height=2, width=6.5)

#===== HURDLE NEG BIN ===========================
model.hurdle.negbin.fb <- hurdle(PostLikes ~ Type + Category + Weekend, 
                                 dist="negbin", 
                                 data=facebook.proc, 
                                 zero.dist="binomial",
                                 link="logit")
summary(model.hurdle.negbin.fb); 
AIC(model.hurdle.negbin.fb)

model.hurdle.negbin.fb.2 <- hurdle(PostLikes ~ Type + Category + Weekend | Weekend, 
                                 dist="negbin", 
                                 data=facebook.proc, 
                                 zero.dist="binomial",
                                 link="logit")
summary(model.hurdle.negbin.fb.2); 
AIC(model.hurdle.negbin.fb.2)


rootogram.hurdle.negbin.fb <- rootogram(model.hurdle.negbin.fb, 
                                        plot=FALSE, max=210)
autoplot(rootogram.hurdle.negbin.fb)
ggsave(file="images/rootogram_hurdle_nb_fb.png", height=2, width=6.5)
