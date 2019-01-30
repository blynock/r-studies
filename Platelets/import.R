# Configuration:
# set this to false if you want the platelets_NoCAD data set with outliers left in
# otherwise removes the outliers we have agreed should be removed
remove.identified.outliers = TRUE

library(tibble)
library(dplyr)

# forced check.names=TRUE to make sure we have a standard environment
platelets_CAD_raw <- as.tibble(read.csv("data/Platelets_CAD.csv", check.names = TRUE))
platelets_NoCAD_raw <- as.tibble(read.csv("data/Platelets_NoCAD.csv", check.names = TRUE))

platelets_CAD_raw$CAD = "Yes"
platelets_NoCAD_raw$CAD = "No"

platelets_CAD <- platelets_CAD_raw
platelets_NoCAD <- platelets_NoCAD_raw

#quick overview
table(is.na(platelets_NoCAD))
#no missing data, good

head(platelets_NoCAD)
tail(platelets_NoCAD)
#can't really tell what is going on with so many variables

summary(platelets_NoCAD)
# hard to make sense of this but looks like there are a lot of binary vars in here which makes
# sense in medical data. 

# would be good to convert to factors

# quick look to identify which columns have barely any unique values (almost certainly factors)
# and those with a medium number (maybe factors)
low.count.vars <- vector()
mid.count.vars <- vector()
high.count.vars <- vector()

variables <- colnames(platelets_NoCAD)

for(var in variables){
  if(length(unique(platelets_NoCAD[[var]])) < 5){
    low.count.vars <- c(low.count.vars, var)
  } 
  if(length(unique(platelets_NoCAD[[var]])) %in% seq(5,30)){
    mid.count.vars <- c(mid.count.vars, var)
  }
  if(length(unique(platelets_NoCAD[[var]])) >30 ){
    high.count.vars <- c(high.count.vars, var)
  }
}
rm(var)

low.count.vars
mid.count.vars
high.count.vars

# from looking at those variables, seems all the low.count_vars are factors and the mid.count.vars aren't

# removing obesity column since it's a function of BMI (Obesity = 1(BMI>=25))
platelets_NoCAD = platelets_NoCAD %>% mutate(Obesity=NULL)

# Also BMI = weight / height^2, should we remove it? or maybe remove height and weight?

# outlier removal
# id: 1 - female patient with platelet count of 742k / mL and Erythrocyte Sedimentation rate of 76
# id: 72 - male patient with white blood cell count of 17800
if(remove.identified.outliers){
  platelets_NoCAD <- platelets_NoCAD[-c(1,72),]
}

platelets_NoCAD_Factors.split <- platelets_NoCAD

platelets_NoCAD_Factors.split$Function_Class1 = ifelse(platelets_NoCAD$Function_Class == 1, 1, 0)
platelets_NoCAD_Factors.split$Function_Class2 = ifelse(platelets_NoCAD$Function_Class == 2, 1, 0)
platelets_NoCAD_Factors.split$Function_Class3 = ifelse(platelets_NoCAD$Function_Class == 3, 1, 0)

platelets_NoCAD_Factors.split$Valvular_Heart_Disease1 = ifelse(platelets_NoCAD$Valvular_Heart_Disease == 1, 1, 0)
platelets_NoCAD_Factors.split$Valvular_Heart_Disease2 = ifelse(platelets_NoCAD$Valvular_Heart_Disease == 2, 1, 0)
platelets_NoCAD_Factors.split$Valvular_Heart_Disease3 = ifelse(platelets_NoCAD$Valvular_Heart_Disease == 3, 1, 0)

# keep a numeric copy
platelets_CAD_Numeric <- platelets_CAD
platelets_NoCAD_Numeric <- platelets_NoCAD

# Now convert factors in the 
# based on analysis of height distributions we think Sex=0 => M, Sex=1 => F
# so update the data set to reflect this
platelets_NoCAD$Sex = ifelse(platelets_NoCAD$Sex == 0, "M", "F")
platelets_CAD$Sex = ifelse(platelets_CAD$Sex == 0, "M", "F")

# convert the low count ones to factors in the non-numeric versions
for(var in low.count.vars[-7]){
  platelets_NoCAD[[var]] <- as.factor(platelets_NoCAD[[var]])
  platelets_CAD[[var]] <- as.factor(platelets_CAD[[var]])
}

# check that looks ok
str(platelets_NoCAD)

#clean up to avoid namespace conflicts
rm(var)
rm(variables)
