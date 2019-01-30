library(glmnet)
library(broom)
require(parcor)

x <- data.matrix(platelets_NoCAD_Numeric[,-44])
y <- platelets_NoCAD_Numeric$Platelet_1000.mL
y.log <- log(platelets_NoCAD_Numeric$Platelet_1000.mL)

# cross-validated fit using GLMNET and LOO XV
set.seed(1)
cvfit = cv.glmnet(x=x,y=y,family="gaussian",alpha=1,nfolds=85)
model.lasso <- cvfit$glmnet.fit
lambda.lasso <- cvfit$lambda.min
coef(cvfit, cvfit$lambda.min)

cvfit.log = cv.glmnet(x=x,y=y.log,family="gaussian",alpha=1,nfolds=10)
model.lasso.log <- cvfit.log$glmnet.fit
lambda.lasso.log <- cvfit$lambda.min


cvfit = cv.glmnet(x=x,y=y,family="gaussian",alpha=0,nfolds=85)
model.ridge <- cvfit$glmnet.fit
lambda.ridge <- cvfit$lambda.min

coef(cvfit, 5.26)

coef(cvfit, cvfit$lambda.min)

# (Intercept)                         439.401508083
# Age                                  -1.268798208
# Height                               -0.215169538
# Sex                                  12.654680974
# Current_Smoker                      -12.346828591
# Airway_Disease                       -8.677982402
# Pulse_rate_PPM                       -0.181281384
# Nonanginal_Chest_Pain                 0.947833032
# Creatine_mg.dL                       -8.628501496
# Hemoglobin_g.dL                      -5.207754592
# Potassium_mEq.lit                   -10.562789284
# White_Blood_Cells.mL                  0.002909197

# Platelet count goes down with current smoking... this is not the direction of effect found by 
# other studies. Is it confounded by sex? Surely not, as it's in the model
# So what is going on?
# Are people who smoke in this data set somehow exceptional?
# Impossible to say without knowing what the selection criteria were

model.lars <- parcor::mylars(X=x, y=y, k = 85)

plot(model.lars$lambda,model.lars$cv,type='l',xlab='lambda',ylab='SSR.n')
abline(v=model.lars$lambda.opt,lwd=2,lty=2,col='darkgray')

# above gives lambda of 5.26
