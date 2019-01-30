#Reporting

model.final <- lm (
  log(Platelet_1000.mL) ~ 
    Age + Sex + Current_Smoker + Atypical + 
    log(White_Blood_Cells.mL) + log(Hemoglobin_g.dL) + +log(Potassium_mEq.lit) + 
    log(White_Blood_Cells.mL):Atypical + Atypical:Current_Smoker, data=platelets_NoCAD)

platelets_CAD$Obesity = NULL
platelets_CAD$CAD = "Yes"
platelets_NoCAD$CAD = "No"


platelets_CAD$Predicted = exp(predict(model.final, newdata=platelets_CAD))
platelets_NoCAD$Predicted = exp(predict(model.final, newdata=platelets_NoCAD))

platelets_CAD$Residual = platelets_CAD$Platelet_1000.mL - platelets_CAD$Predicted
platelets_NoCAD$Residual = platelets_NoCAD$Platelet_1000.mL - platelets_NoCAD$Predicted

platelets_ALL <- rbind(platelets_CAD, platelets_NoCAD)
platelets_ALL$CAD <- as.factor(platelets_ALL$CAD)

require(ggplot2)
require(cowplot)

cad_mean <- mean(platelets_CAD$Platelet_1000.mL)
nocad_mean <- mean(platelets_NoCAD$Platelet_1000.mL)

cad_mean
nocad_mean

plot.cad.histogram <- ggplot(platelets_ALL, aes(x=Platelet_1000.mL, color=CAD, fill=CAD), y=..density..) +
  geom_histogram(position = "dodge", alpha=0.5)+
  theme_cowplot() +
  labs(x="Platelet count (1000 / mL)", y="Number of patients")

ggsave(file="report/cad_histogram.png", plot=plot.cad.histogram, width=10, height=5)

plot.cad.density <- ggplot(platelets_ALL, aes(x=Platelet_1000.mL, color=CAD, fill=CAD), y=..density..) +
  geom_density(alpha=0.5)+
  theme_cowplot() +
  labs(x="Platelet count (1000 / mL)", y="Proportion of patients")+ 
  theme(
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )

ggsave(file="report/cad_density.png", plot=plot.cad.density, width=10, height=5)

plot.cad.density.2 <- ggplot(platelets_ALL_raw, aes(x=Platelet_1000.mL, color=CAD, fill=CAD), y=..density..) +
  geom_density(alpha=0.5)+
  theme_cowplot() +
  labs(x="Platelet count (1000 / mL)", y="Proportion of patients")+ 
  theme(
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )

ggsave(file="report/cad_density_2.png", plot=plot.cad.density.2, width=10, height=5)


pred_mean <- mean(platelets_CAD$Predicted)

plot.cad.predicted_vs_observed <- ggplot(platelets_CAD, aes(x=Platelet_1000.mL)) +
  geom_density(size=1, alpha=0.1, aes(color="Observed")) +
  geom_density(aes(x=Predicted, color="Predicted"), alpha=0.1, linetype=2, size=1) + 
  geom_vline(xintercept=pred_mean, color="#990000", linetype=2, size=1, show.legend = TRUE) +
  geom_vline(xintercept=cad_mean, color="red", linetype=2, size=1, show.legend=TRUE) +
  theme_cowplot() +
  labs(x="Platelet count (1000 / mL)", y="Proportion of patients")+
  scale_color_manual(name=NULL, values=c(Predicted="#990000", Observed="red"))+ 
  theme(
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )
plot.cad.predicted_vs_observed

ggsave(file="report/cad_predicted_vs_observed.png", plot=plot.cad.predicted_vs_observed, width=10, height=5)

require(dplyr)

platelets_CAD$Residual

platelets_CAD %>% arrange(desc(abs(Residual))) %>% dplyr::select("Id", "Platelet_1000.mL", "Predicted", "Residual", "Age","Sex", "Atypical", "Current_Smoker", "White_Blood_Cells.mL", "Hemoglobin_g.dL", "Potassium_mEq.lit")

testdf$Atypical <- as.factor(0)
predict(model.final, newdata=testdf)

testdf$Atypical <- as.factor(1)
predict(model.final, newdata=testdf)

platelets_ALL_raw <- rbind(platelets_CAD_raw, platelets_NoCAD_raw)

plot.cad.boxplot <- ggplot(platelets_ALL_raw, aes(x=CAD, y=Platelet_1000.mL, color=CAD, fill=CAD)) +
  geom_boxplot(alpha=0.3)+
  theme_cowplot() +
  labs(x="", y="Platelet Count (1000/mL")+
  theme(
    legend.position = "none"
  )

ggsave(file="report/cad_boxplots.png", plot=plot.cad.boxplot, width=10, height=5)

modelcols <- c("Platelet_1000.mL", "Predicted", "Residual", "Age","Sex", "Atypical", "Current_Smoker", "White_Blood_Cells.mL", "Hemoglobin_g.dL", "Potassium_mEq.lit")

#patients - 6, 50, 154, 7, 82
patient.id = 82

pred_data <- data.frame(platelets_CAD[patient.id,] %>% dplyr::select(modelcols))
pred_data

# change these values to alter prediction data
# hemo 5num: 8.9 12.0 13.1 14.2 17.6
# pota 5num: 3.1 4.0 4.3 4.5 6.6
# wbc 5num: 3700  5900  7150  9000 18000

# pred_data[1,]$Current_Smoker=0
# pred_data[1,]$White_Blood_Cells.mL=6700
pred_data[1,]$Hemoglobin_g.dL = 9
pred_data[1,]$Potassium_mEq.lit = 4.4

exp(predict(model.final, platelets_CAD[patient.id,]))
exp(predict(model.final, pred_data))

#residual with original prediction
platelets_CAD[patient.id,]$Platelet_1000.mL- exp(predict(model.final, platelets_CAD[patient.id,]))

#residual with adjustments made above
platelets_CAD[patient.id,]$Platelet_1000.mL- exp(predict(model.final, pred_data))

