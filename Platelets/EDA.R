# investigate the distributions and see if they look sensible
library(ggplot2)
library(dplyr)
library(ggcorrplot)

source("import.r")

# corr <- round(cor(platelets_NoCAD_Numeric),2)
# too many covariates!
#corr <- round(cor(platelets_NoCAD_Numeric %>% select(mid.count.vars)),2)

data.filtered.male <- platelets_NoCAD_Numeric %>%  dplyr::filter(Sex==0) %>% dplyr::select(c(high.count.vars, mid.count.vars))
data.filtered.female <- platelets_NoCAD_Numeric %>%  dplyr::filter(Sex==1) %>% dplyr::select(c(high.count.vars, mid.count.vars))

corr <- round(cor(data.filtered.female),2)
ggcorrplot(corr, 
           hc.order=TRUE, 
           type="lower", 
           lab=TRUE, 
           lab_size = 3, 
           method="circle", 
           colors=c("tomato2", "white", "springgreen3"), 
           title="Correlogram of platelets data, Female", 
           ggtheme=theme_bw
           )

ggplot(platelets_NoCAD, aes(x=Age,color=Sex)) + geom_histogram(binwidth = 10)
# the age distribution is a slightly odd shape, very few people < 40 or > 65.
# in UK population, about 18% are over 65. (https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/articles/overviewoftheukpopulation/july2017)
# so would expect about 22% of this data to be.
c(
  count(platelets_NoCAD %>% filter(Age >= 65)),
  count(platelets_NoCAD),
  count(platelets_NoCAD %>% filter(Age >= 65))/count(platelets_NoCAD)
)
# but actually only ~14% are. Why is that? Will the results be generalisable?

# how does the count look by sex?
ggplot(data=platelets_NoCAD, aes(x=Age, y=Platelet_1000.mL,color=Sex)) + geom_point()
# very high outlier, sex=1, age 66. could this be a mistake?

platelets_NoCAD %>% filter(Age==66) %>% select("Platelet_1000.mL")
# it's 742k / ml. quick google suggests normal range is 100-400k / ml which looks right from our data.
# this _could_ be legitimate but might be a mistake and will probably distort the model.
# worth removing?

# height distributions?
ggplot(platelets_NoCAD, aes(x=Height,color=Sex, y=..density..)) + geom_histogram(binwidth=1, position="dodge" ) + geom_density()
# firstly, this strongly suggests that 0 = male, 1 = female
# looks acceptably normal by gender, OK

# weight distributions?
ggplot(platelets_NoCAD, aes(x=Weight,color=Sex, y=..density..)) + geom_histogram(binwidth=1, ) + geom_density()
# there's a lot of men who are all 75kg, is this suspicious?

table(platelets_NoCAD$Weight, platelets_NoCAD$Sex)
# only 7, I suppose it's within the realms of possibility

table(platelets_NoCAD$Sex)
# 46 0s, 41 1s. strongly suspect that 0=M, 1=F based on height and weight distributions,
# but don't know for sure. will take as working hypothesis

# BMI distributions?
ggplot(platelets_NoCAD, aes(x=BMI,color=Sex, y=..density..)) + geom_histogram(binwidth=1, ) + geom_density()
# male is approximately normal, female is definitely not

table(platelets_NoCAD$Sex, platelets_NoCAD$Diabetes_Mellitus)
c(3/46, 7/41)
# seems 6.5% of men have Diabetes, 17% of women
# normally 8.3% in adult population. (wikipedia) suspicious?

table(platelets_NoCAD$Sex, platelets_NoCAD$Hypertension)
# roughly 30% men, 40% women here
# general population ~ 30% http://www.bloodpressureuk.org/microsites/kyn/Home/Media/Factsandfigures

table(platelets_NoCAD$Sex, platelets_NoCAD$Current_Smoker)
# 30% of men smoke but _none_ of the women in the data set smoke
# 19.3% vs 15.3% on average according to ONS https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/healthandlifeexpectancies/bulletins/adultsmokinghabitsingreatbritain/2015

table(platelets_NoCAD$Sex, platelets_NoCAD$Ex.Smoker)
# only 2 ex smokers in the data set, both men. interesting

table(platelets_NoCAD$Sex, platelets_NoCAD$Family_History_Of_CAD)
# 2x as many women (19%) (proportionately) as men (8.7%) have family history. 
# not sure how that can be true? do women report this more accurately?

table(platelets_NoCAD$Sex, platelets_NoCAD$Obesity)
# 63% of the sampled men are obese and 80% the women are obese
# 23.1% of UK population are obese https://en.wikipedia.org/wiki/Obesity_in_the_United_Kingdom
# data set is clearly non-representative here

table(platelets_NoCAD$Sex, platelets_NoCAD$Cerebrovascular_Accident)
# one man has had cerebrovascular accident - stroke
# 64 year old man, obese

platelets_NoCAD %>% filter(Obesity==0  & BMI >= 25)
# obese seems to be defined as BMI >= 25
# don't need obese as a predictor if we use BMI
# definitely don't need both


table(platelets_NoCAD$Sex, platelets_NoCAD$Airway_Disease)
# 1 man with airway disease, no women

table(platelets_NoCAD$Sex, platelets_NoCAD$Thyroid_Disease)
# no men, 3 women with thyroid disease

table(platelets_NoCAD$Sex, platelets_NoCAD$Dyslipidermia)
# 21.7 of men have this, 56% of women
# this also appears to be a typo? google suggests dyslipidemia
# which means abnormal amount of lipids in the blood
# Prolonged elevation of insulin levels can also lead to dyslipidemia.
# correlation with diabetes?

ggplot(platelets_NoCAD, aes(x=Blood_Pressure_mm_Hg,color=Sex, y=..density..)) + geom_histogram(binwidth=1, position="dodge") + geom_density()
# very similar by sex
# seems to have been rounded to the nearest 5

ggplot(platelets_NoCAD, aes(x=Pulse_rate_PPM,color=Sex, y=..density..)) + geom_histogram(binwidth=1, position="dodge") + geom_density()
# again looks like a lot of rounding occurs here
# this seems like it would be fairly useless as a predictor as a consequence

table(platelets_NoCAD$Sex, platelets_NoCAD$Edema)
# just 2 men with this, 0 women
# swelling due to fluid accumulation
# link to blood protein levels, can this link to platelets in some way?

table(platelets_NoCAD$Sex, platelets_NoCAD$Lung_Rales)
# crackling in breath linked to resp. infection
# 2 sampled men have this, 0 women

table(platelets_NoCAD$Sex, platelets_NoCAD$Systolic_Murmur)
# 6 of each sex. 
# heart murmur during systole (contraction)

table(platelets_NoCAD$Sex, platelets_NoCAD$Diastolic_Murmur)
# rarer than systolic. 3 of each

table(platelets_NoCAD$Sex, platelets_NoCAD$Typical_Chest_Pain)
# appears to refer to angina
# 7 men, 3 women

table(platelets_NoCAD$Sex, platelets_NoCAD$Dyspnea)
# very common in data set, 50% of men, 58% of women
# shortness of breath. definite link to obesity (but not perfectly correlated)

table(platelets_NoCAD$Sex, platelets_NoCAD$Function_Class)
# can't find this with a very quick google
# suspect something to do with ability to care for self / lead a normal life
# majority of patients are level 0; about 1/5 men level 2, while about 1/4 women level 3 or 4
# note: men tend to die

table(platelets_NoCAD$Sex, platelets_NoCAD$Atypical)
# not sure what this means!
# anyway, it's the majority of the patients in both genders

table(platelets_NoCAD$Sex, platelets_NoCAD$Nonanginal_Chest_Pain)
# refers to such things as aortic tears, &c
# 13% of men, 17% of women

table(platelets_NoCAD$Sex, platelets_NoCAD$ECG_St_Depression)
# this is an electrocardiogram result often associated with restricted blood supply to heart
# blood related, seems like a potential predictor to me!
# 6.5% of men, 22% of women

table(platelets_NoCAD$Sex, platelets_NoCAD$ECG_T_Inversion)
# another ECG result which might be associated with restricted blood flow to heart
# also seen in angina and other general heart problems
# 8.7% men, 17% women

table(platelets_NoCAD$Sex, platelets_NoCAD$ECG_Left_Ventricular_Hypertrophy)
# related to T_Inversion
# thickening of wall of left ventricle
# 6.5% men, just 1 woman

ggplot(platelets_NoCAD, aes(x=Fasting_Blood_Sugar_mg.dL,color=Sex, y=..density..)) + geom_histogram(binwidth=1, position="dodge") + geom_density()
# 70-100 are normal values for fasting
# there's one in the data set with 300! (woman)
# a few above 200. the data looks fairly normal if you ignore the outer points
# feels this might be related to diabetes, dyslipidemia

ggplot(platelets_NoCAD, aes(x=Creatine_mg.dL,color=Sex, y=..density..)) + geom_histogram(position="dodge") + geom_density()
# this is rounded, data seems quite normal, bit lower for women than men
# used for recycling of adenosine triphosphate

ggplot(platelets_NoCAD, aes(x=Triglyceride_mg.dL,color=Sex, y=..density..)) + geom_histogram(position="dodge") + geom_density()
# another one which is roughly normal but with a long tail on the right with some big outliers
# In the human body, high levels of triglycerides in the bloodstream have been linked to 
# atherosclerosis and, by extension, the risk of heart disease[8] and stroke.[7] However, the 
# relative negative impact of raised levels of triglycerides compared to that of LDL:HDL ratios 
# is as yet unknown. The risk can be partly accounted for by a strong inverse relationship between 
# triglyceride level and HDL-cholesterol level. https://en.wikipedia.org/wiki/Triglyceride#Role_in_disease

ggplot(platelets_NoCAD, aes(x=Low.Density_Lipoprotein_mg.dL,color=Sex, y=..density..)) + geom_histogram(position="dodge") + geom_density()
# not much to say about this, seems roughly normal

ggplot(platelets_NoCAD, aes(x=High.Density_Lipoprotein_mg.dL,color=Sex, y=..density..)) + geom_histogram(position="dodge") + geom_density()
# not much to say about this either.
# distribution is a slightly odd shape. comment on Triglyceride above seems relevant

ggplot(platelets_NoCAD, aes(x=Blood_Urea_Nitrogen_mg.dL,color=Sex, y=..density..)) + geom_histogram(position="dodge") + geom_density()
# a few high outliers again, nothing too exiting otherwise
# does make me wonder if these outliers are all the same few people
# might be worth excluding them they are going to be very high leverage if so

ggplot(platelets_NoCAD, aes(x=Erythrocyte_Sedimentation_Rate_mm.h,color=Sex, y=..density..)) + geom_histogram(position="dodge") + geom_density()
# another very positive skewed one.
# interesting / bizarre men are definitely below women on this
# a few very high outlying women again... same ones?

ggplot(platelets_NoCAD, aes(x=Hemoglobin_g.dL,color=Sex, y=..density..)) + geom_histogram(position="dodge") + geom_density()
# both sexes slightly bimodal dists. worth more of a look?

ggplot(platelets_NoCAD, aes(x=Potassium_mEq.lit,color=Sex, y=..density..)) + geom_histogram(position="dodge") + geom_density()
# data seems normal here
# usually measured in millimoles / litre normal range 3.6-5.2
# data all seems to be within that normal range

ggplot(platelets_NoCAD, aes(x=Sodium_mEq.lit,color=Sex, y=..density..)) + geom_histogram(position="dodge") + geom_density()
# data looks normal
# 135-145 is normal mEq = "milliequivalents"
# an equivalent is the number of moles of an ion in a solution, multiplied by the valence of that ion

ggplot(platelets_NoCAD, aes(x=White_Blood_Cells.mL,color=Sex, y=..density..)) + geom_histogram(position="dodge") + geom_density()
# one very high count
# high Leukocytes usually points to infection?
# person with high count should maybe be excluded, possible infected injury which might affect free platelet count
# A high white blood cell count usually indicates: 
# An increased production of white blood cells to fight an infection. 
# A reaction to a drug that increases white blood cell production. 
# A disease of bone marrow, causing abnormally high production of white blood cells.

ggplot(platelets_NoCAD, aes(x=Lymphocyte_.,color=Sex, y=..density..)) + geom_histogram(position="dodge") + geom_density()
# looks normal
# lymphocyte - type of white blood cell
# similar comment to above?

ggplot(platelets_NoCAD, aes(x=Neutrophil_.,color=Sex, y=..density..)) + geom_histogram(position="dodge") + geom_density()
# looks normal
# another type of white blood cell
# same comments again

ggplot(platelets_NoCAD, aes(x=Ejection_Fraction_.,color=Sex, y=..density..)) + geom_histogram(position="dodge") + geom_density()
# this has negative skew. some low outliers here which probably need investigating
# The left ventricle is the heart's main pumping chamber, so ejection fraction is usually measured 
# only in the left ventricle (LV). An LV ejection fraction of 55 percent or higher is considered 
# normal. An LV ejection fraction of 50 percent or lower is considered reduced.
# low values indicate heart disease

table(platelets_NoCAD$Sex, platelets_NoCAD$Valvular_Heart_Disease)
# one of the 4 heart valves
# not going to be able to translate these factor levels into valve names
# could be significant?


# HDL vs Triglycerides - negative correlation?
ggplot(platelets_NoCAD, aes(x=Triglyceride_mg.dL, y=High.Density_Lipoprotein_mg.dL, color=Sex)) + geom_point() + geom_smooth(se=FALSE)
#doesn't really look like it

# BMI = weight / height^2 (height in metres)
# does this mean we can remove BMI (or the others)?


require(ggplot2)
require(cowplot)

# is there a relationship between height and platelets?
ggplot(platelets_NoCAD, aes(x=Height, y=Platelet_1000.mL)) + 
  geom_point() +
  geom_smooth(method="lm")

ggsave(file="report/images/plot_simpsons_1.png")
ggplot(platelets_NoCAD, aes(x=Height, y=Platelet_1000.mL, color=Sex)) + 
  geom_point() +
  geom_smooth(method="lm")
ggsave(file="report/images/plot_simpsons_2.png")
# AWOOOGA simpson's paradox 
# we definitely need to include sex in the final model; without it significance is all over the place
