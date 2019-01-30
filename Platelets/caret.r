# using the caret package to automate model fitting and testing

library(caret)

set.seed(1)
train(formula(model.forwards.suggested.aic), data=platelets_NoCAD, trControl=trainControl(method="LOOCV"), method="lm")
# forwards AIC - Rsq 0.32, RMSE 42.5
train(formula(model.forwards.suggested.bic), data=platelets_NoCAD, trControl=trainControl(method="LOOCV"),method="lm")
#forwards BIC - Rsq 0.24, RMSE 44.3
train(formula(model.backwards.suggested.aic), data=platelets_NoCAD, trControl=trainControl(method="LOOCV"), method="lm")
# backwards AIC - Rsq 0.29, RMSE 45.8
train(formula(model.backwards.suggested.bic), data=platelets_NoCAD, trControl=trainControl(method="LOOCV"),method="lm")
#backwards BIC - Rsq 0.24 RMSE 44.5


# best model we have here is fowards AIC
summary(model.forwards.suggested.aic)

extractAIC(model.constructed)
extractAIC(model.constructed.log)

train(formula(model.constructed), data=platelets_NoCAD, trControl=trainControl(method="LOOCV"), method="lm")

train(formula(model.constructed.log), data=platelets_NoCAD, trControl=trainControl(method="LOOCV"), method="lm")

#seems to do a better job of predicting without the log

interaction.to.add = add1(model.constructed, ~ .^2)
data.frame(Interaction=row.names(interaction.to.add[interaction.to.add$AIC==min(interaction.to.add$AIC),]),New.AIC=min(interaction.to.add$AIC))

# step 1 - added Atypical:White_BloodCells.mL
# step 2 - added Age:Dyspnea

#constructed model
model.constructed.log <- lm(log(Platelet_1000.mL) ~ Sex + Age + Atypical + Current_Smoker + White_Blood_Cells.mL + 
                              Hemoglobin_g.dL + Potassium_mEq.lit + Family_History_Of_CAD + 
                              Pulse_rate_PPM + Dyspnea + Blood_Urea_Nitrogen_mg.dL + Creatine_mg.dL + 
                              Sodium_mEq.lit + Atypical:White_Blood_Cells.mL + Age:Dyspnea, data=platelets_NoCAD)

model.constructed <- lm(Platelet_1000.mL ~ Sex + Age + Atypical + Current_Smoker + White_Blood_Cells.mL + 
                          Hemoglobin_g.dL + Potassium_mEq.lit + Family_History_Of_CAD + 
                          Pulse_rate_PPM + Dyspnea + Blood_Urea_Nitrogen_mg.dL + Creatine_mg.dL + 
                          Sodium_mEq.lit + Atypical:White_Blood_Cells.mL + Age:Dyspnea, data=platelets_NoCAD)

model.constructed.nopulse <- lm(Platelet_1000.mL ~ Sex + Age + Atypical + Current_Smoker + White_Blood_Cells.mL + 
                                  Hemoglobin_g.dL + Potassium_mEq.lit + Family_History_Of_CAD + 
                                  Dyspnea + Blood_Urea_Nitrogen_mg.dL + Creatine_mg.dL + 
                                  Sodium_mEq.lit + Atypical:White_Blood_Cells.mL + Age:Dyspnea, data=platelets_NoCAD)
extractAIC(model.constructed.nopulse)
extractAIC(model.constructed)

#=================================

model.best.subsets <- lm(
  log(Platelet_1000.mL) ~ 
    Age + Sex + Atypical + Current_Smoker + Hemoglobin_g.dL + log(White_Blood_Cells.mL) + Nonanginal_Chest_Pain +
    Atypical:log(White_Blood_Cells.mL) + Atypical:Current_Smoker + Hemoglobin_g.dL:Nonanginal_Chest_Pain,
  data=platelets_NoCAD)


model.best.subsets <- lm(
  log(Platelet_1000.mL) ~ Age + Sex + Atypical + Current_Smoker + Hemoglobin_g.dL + log(White_Blood_Cells.mL),
  data=platelets_NoCAD)
# + Potassium_mEq.lit:Hemoglobin_g.dL
mmps(model.best.subsets)

plot(model.best.subsets,1)

model.candidate.1 <- model.best.subsets

bl.suggest <- function(arg.model ){
  term.to.add = add1(arg.model, scope= ~. + Age + Weight + Height + Sex + BMI + Diabetes_Mellitus + 
                       Hypertension + Current_Smoker + Ex.Smoker + Family_History_Of_CAD + 
                       Cerebrovascular_Accident + Airway_Disease + Thyroid_Disease + 
                       Dyslipidermia + Blood_Pressure_mm_Hg + Pulse_rate_PPM + Edema + 
                       Lung_Rales + Systolic_Murmur + Diastolic_Murmur + Typical_Chest_Pain + 
                       Dyspnea + Function_Class + Atypical + Nonanginal_Chest_Pain + 
                       ECG_St_Depression + ECG_T_Inversion + ECG_Left_Ventricular_Hypertrophy + 
                       Fasting_Blood_Sugar_mg.dL + Creatine_mg.dL + Triglyceride_mg.dL + 
                       Low.Density_Lipoprotein_mg.dL + High.Density_Lipoprotein_mg.dL + 
                       Blood_Urea_Nitrogen_mg.dL + Erythrocyte_Sedimentation_Rate_mm.h + 
                       Hemoglobin_g.dL + Potassium_mEq.lit + Sodium_mEq.lit + White_Blood_Cells.mL + 
                       Lymphocyte_. + Neutrophil_. + Ejection_Fraction_. + Valvular_Heart_Disease)
  interaction.to.add = add1(arg.model, scope=~.^2)
  term.to.drop = drop1(arg.model, scope=~.)
  
  return(
    data.frame(
      Num.Pred=extractAIC(arg.model)[1], 
      Old.AIC=extractAIC(arg.model)[2],
      Add.Single.Term=row.names(term.to.add[term.to.add$AIC==min(term.to.add$AIC),]), 
      Term.New.AIC=min(term.to.add$AIC), 
      Add.Interaction=row.names(interaction.to.add[interaction.to.add$AIC==min(interaction.to.add$AIC),]), 
      Interaction.New.AIC=min(interaction.to.add$AIC),
      Drop.Term=row.names(term.to.drop[term.to.drop$AIC==min(term.to.drop$AIC),]),
      Drop.Term.AIC=min(term.to.drop$AIC)
    )
  )
}

bl.suggest(model.best.subsets)

summary(model.best.subsets)
#===============================

extractAIC(model.best.subsets)

train(formula(model.best.subsets), data=platelets_NoCAD, trControl=trainControl(method="LOOCV"),method="lm")
#best subsets + interaction terms - Rsq 0.33 RMSE 41
plot(model.best.subsets,1)

model.caret.lasso <- train(Platelet_1000.mL~., data=platelets_NoCAD_Numeric[,-nearZeroVar(platelets_NoCAD_Numeric)], trControl=trainControl(method="repeatedcv",number=10, repeats=10), method="lasso")


#===============================

train(formula(model.constructed), data=platelets_NoCAD, trControl=trainControl(method="LOOCV"),method="lm")
#constructed from stepwise forwards AIC

model.nosmoking <- lm(Platelet_1000.mL ~ Sex + Age + Atypical + White_Blood_Cells.mL + 
                        Hemoglobin_g.dL + Potassium_mEq.lit + Family_History_Of_CAD + 
                        Pulse_rate_PPM + Dyspnea + Blood_Urea_Nitrogen_mg.dL + Creatine_mg.dL + 
                        Sodium_mEq.lit + Atypical:White_Blood_Cells.mL + Age:Dyspnea, data=platelets_NoCAD)

train(formula(model.nosmoking), data=platelets_NoCAD, trControl=trainControl(method="LOOCV"),method="lasso")
#constructed from stepwise forwards AIC


model.wes <- lm(log(Platelet_1000.mL) ~ Sex + Age + log(White_Blood_Cells.mL) + log(Hemoglobin_g.dL) + log(Potassium_mEq.lit) + Family_History_Of_CAD + Pulse_rate_PPM, data=platelets_NoCAD)


mmps(model.wes)

par(mfrow=c(1,2))
plot(model.wes,1)
plot(model.best.subsets,1)


set.seed(1)
train(formula(model.candidate.1), data=platelets_NoCAD, trControl=trainControl(method="repeatedcv", number=10, repeats = 20),method="lm")

set.seed(1)
train(formula(model.wes), data=platelets_NoCAD, trControl=trainControl(method="repeatedcv", number=10, repeats = 20),method="lm")

bl.suggest(model.wes)

summary(model.wes)


model.candidate.1 <- lm(
  Platelet_1000.mL ~ 
    Age + Sex + Atypical + Current_Smoker + Hemoglobin_g.dL + log(White_Blood_Cells.mL) + Potassium_mEq.lit + Nonanginal_Chest_Pain +
    Atypical:log(White_Blood_Cells.mL) + Atypical:Current_Smoker + Potassium_mEq.lit:Hemoglobin_g.dL + Hemoglobin_g.dL:Nonanginal_Chest_Pain,
  data=platelets_NoCAD)

bl.suggest(model.candidate.1)


model.best.subsets <- lm(
  log(Platelet_1000.mL) ~ 
    Age + Sex + Atypical + Current_Smoker + 
    Hemoglobin_g.dL + log(White_Blood_Cells.mL),
  data=platelets_NoCAD
)

bl.suggest(model.best.subsets)


model.best.subsets.2 <- lm(
  log(Platelet_1000.mL) ~ 
    Age + Sex + Atypical + Current_Smoker + 
    Hemoglobin_g.dL + log(White_Blood_Cells.mL) +
    Atypical:log(White_Blood_Cells.mL),
  data=platelets_NoCAD
)
bl.suggest(model.best.subsets.2)


AIC & Term to Add & New AIC\\
-293.8 & Atypical:log(White_Blood_Cells.mL) & -297.8\\
-297.8 & Potassium_mEq.lit & -298.9\\
-298.9 & Atypical:Current_Smoker & -300.8\\

model.best.subsets.3 <- lm(
  log(Platelet_1000.mL) ~ 
    Age + Sex + Atypical + Current_Smoker + 
    Hemoglobin_g.dL + log(White_Blood_Cells.mL) +
    Atypical:log(White_Blood_Cells.mL) + Potassium_mEq.lit,
  data=platelets_NoCAD)
bl.suggest(model.best.subsets.3)


model.best.subsets.4 <- lm(
  log(Platelet_1000.mL) ~ 
    Age + Sex + Atypical + Current_Smoker + 
    log(Hemoglobin_g.dL) + log(White_Blood_Cells.mL) + log(Potassium_mEq.lit) +
    Atypical:log(White_Blood_Cells.mL) +  Atypical:Current_Smoker,
  data=platelets_NoCAD)
bl.suggest(model.best.subsets.4)

model.final
