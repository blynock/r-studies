# Stepwise AIC 

# playing around with add1 / drop1
# fit full model then iteratively remove a term at a time
model.full <- lm(Platelet_1000.mL ~ ., data=platelets_NoCAD)

# r1 <- drop1(model.full, ~ .)
# row.names(r1[r1$AIC==min(r1$AIC),]) # give the row name of row where AIC score is the min
# # suggests removing Valvular Heart Disease...
# 
# model.2 <- lm(Platelet_1000.mL ~ . - Valvular_Heart_Disease, data=platelets_NoCAD)
# r2 <- drop1(model.2, ~.)
# row.names(r2[r2$AIC==min(r2$AIC),])
# 
# model.3 <- lm(Platelet_1000.mL ~ . - Valvular_Heart_Disease, data=platelets_NoCAD)
# r3 <- drop1(model.3, ~.-Sex) # suggested Sex to drop but we can't, so exclude from consideration
# row.names(r3[r3$AIC==min(r3$AIC),])
# 
# model.4 <- lm(Platelet_1000.mL ~ . - Valvular_Heart_Disease  - Nonanginal_Chest_Pain, data=platelets_NoCAD)
# r4 <- drop1(model.4, ~.-Sex)
# row.names(r4[r4$AIC==min(r4$AIC),])

# And so on

# alternatively start with bare bones model and add one at a time
model.1 <- lm(Platelet_1000.mL ~ Sex, data=platelets_NoCAD)
r1 <- add1(model.1, formula(model.full))
row.names(r1[r1$AIC==min(r1$AIC),])
#suggests adding Age

model.2 <- lm(Platelet_1000.mL ~ Sex + Age, data=platelets_NoCAD)
r2 <- add1(model.2, formula(model.full))
row.names(r2[r2$AIC==min(r2$AIC),])
#suggests adding Atypical [whatever that is...]

# etc etc

# this stuff above works but it's time-consuming and a general pain
# might be useful for tweaking when mostly done?

##################
# automated version in package MASS:

library(MASS)

###### Step regression using AIC
#### Backwards
# first create full model to step backwards from
# formula of Platelet_1000.mL ~ . means include all other variables in the dataset as predictors

# we think Sex is required in final model, so exclude SexM and SexF from the removal scope
# So removal scope is:  ~. - SexM - SexF
model.backwards.suggested.aic <- stepAIC(model.full, scope=c(upper=~.,lower=~Sex), direction="backward")
model.backwards.suggested.bic <- stepAIC(model.full, scope=c(upper=~.,lower=~Sex), direction="backward", k=log(nrow(platelets_NoCAD)))
# final model AIC score is 624.7

#### Forwards
# starting with model containing Sex
model.sex <- lm(Platelet_1000.mL ~ Sex, data=platelets_NoCAD)

# scope argument is the formula which is the upper bound for the step space
# I'm using the formula extracted from the full model I fitted above
model.forwards.suggested.aic <- stepAIC(model.sex, scope=formula(model.full), direction="forward")
model.forwards.suggested.bic <- stepAIC(model.sex, scope=formula(model.full), direction="forward", k=log(nrow(platelets_NoCAD)))
# final model AIC is 633.6



# what effect does adding the best available interaction term give?
extractAIC(model.forwards.suggested.aic)
interaction.to.add = add1(model.forwards.suggested.aic, ~ .^2)
data.frame(Interaction=row.names(interaction.to.add[interaction.to.add$AIC==min(interaction.to.add$AIC),]),New.AIC=min(interaction.to.add$AIC))
# new AIC is 628.8; seems it improves things. so we perhaps need to play around with this

model.backwards.suggested.aic$anova
model.backwards.suggested.bic$anova
model.forwards.suggested.aic$anova
model.forwards.suggested.bic$anova


model.candidate.4 <- lm(log(Platelet_1000.mL) ~ Sex + Age + Atypical + Current_Smoker + 
                          White_Blood_Cells.mL + Hemoglobin_g.dL + Potassium_mEq.lit + 
                          Family_History_Of_CAD + Pulse_rate_PPM + Dyspnea + Blood_Urea_Nitrogen_mg.dL + 
                          Creatine_mg.dL + Sodium_mEq.lit + Atypical:White_Blood_Cells.mL +
                        Age:Dyspnea + Dyslipidermia + Dyspnea:Dyslipidermia + Sex:Atypical,
                        data = platelets_NoCAD)
summary(model.candidate.4)
