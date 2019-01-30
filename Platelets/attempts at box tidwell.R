library(car)

pl.f <- platelets_NoCAD[platelets_NoCAD$Sex=="F",]
pl.m <- platelets_NoCAD[platelets_NoCAD$Sex=="M",]
  
model.full <- lm(Platelet_1000.mL ~ ., data=platelets_NoCAD)
boxTidwell(Platelet_1000.mL ~ Height, data=pl.m)

#box tidwell findings
# height -> l = -.02? log?

#Weight + Height + BMI + Fasting_Blood_Sugar_mg.dL

high.count.vars <- high.count.vars[-c(14)]


platelets_NoCAD$Fasting_Blood_Sugar_mg.dL

summary(lm(Platelet_1000.mL ~ Fasting_Blood_Sugar_mg.dL, data=platelets_NoCAD))

ggplot(data=platelets_NoCAD %>% filter(Sex=="F"), aes(y=Platelet_1000.mL, x=Height)) + 
  geom_point() +
  geom_smooth(se=FALSE,method="lm")

summary(pl.m$Height)
