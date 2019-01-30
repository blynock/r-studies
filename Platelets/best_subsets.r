require(dplyr)
require(leaps) #best subset package
require(stringr) #for nicer layout of results

renderBSRSummary <- function(name,best.subsets){
  best.subsets.summary <- summary(best.subsets)
  cat(str_pad(paste("\n===",name,sep=""),40, side="right",pad="="))
  cat("\n")
  bestModelAdjR2 <- which.max(best.subsets.summary$adjr2)
  cat(paste("Adjusted R squared",bestModelAdjR2,sep=":"))
  cat("\n")
  print(best.subsets.summary$outmat[bestModelAdjR2,])
  bestModelCp <- which.min(best.subsets.summary$cp)
  cat(paste("CP",bestModelCp,sep=":"))
  cat("\n")
  print(best.subsets.summary$outmat[bestModelCp,])
  bestModelBic <- which.min(best.subsets.summary$bic)
  cat(paste("BIC",bestModelBic,sep=":"))
  cat("\n")
  print(best.subsets.summary$outmat[bestModelBic,])
}


renderBSRSummary("Overall", best.subsets)

model.best.subsets <- lm(Platelet_1000.mL ~ Age + Sex + Atypical + Current_Smoker + Hemoglobin_g.dL + White_Blood_Cells.mL, data=platelets_NoCAD)

library(leaps)
best.subsets <- regsubsets(Platelet_1000.mL~., platelets_NoCAD, nvmax=12)
best.subsets.bic <- which.min(summary(best.subsets)$bic)
print(best.subsets.summary$outmat[best.subsets.bic,])

interaction.to.add = add1(model.forwards.suggested.aic, ~ .^2)
minAIC <- min(interaction.to.add$AIC)
picked <- interaction.to.add[interaction.to.add$AIC==minAIC,]
data.frame(
  Interaction=row.names(picked),
  New.AIC=min(interaction.to.add$AIC)
)