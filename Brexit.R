# 1. Choose .cvs file

Brexit <- read.csv(file.choose(), header=T)
head(Brexit)  	
str(Brexit)     
attach(Brexit)
Brexit <- na.omit(Brexit)

# 2. Descriptive Statistics 

summary(Brexit[-1])

# 3. Normality Test

library(nortest)

ad.test(Pct_1)
ad.test(Pct_2)
ad.test(Net_immigration)
ad.test(Population)
ad.test(Populationdensity)
ad.test(GDHIPH)
ad.test(GGDHI)
ad.test(Taxes)
ad.test(Benefits)
ad.test(Median_age)
ad.test(X0to10)
ad.test(X10to20)
ad.test(X20to30)
ad.test(X30to40)
ad.test(X40to50)
ad.test(X50to60)
ad.test(X60to70)
ad.test(X70to80)
ad.test(X80to90)
ad.test(Pct_Leave)
ad.test(Pct_Remain)

# 4. Correlations "leave"

cor.test(Pct_1, Net_immigration, method = "pearson")
cor.test(Pct_1, Population, method = "pearson")     
cor.test(Pct_1, Populationdensity, method = "pearson")   
cor.test(Pct_1, GDHIPH, method = "pearson")
cor.test(Pct_1, GGDHI, method = "spearman")
cor.test(Pct_1, Taxes, method = "pearson")
cor.test(Pct_1, Benefits, method = "pearson")
cor.test(Pct_1, Median_age, method = "pearson")
cor.test(Pct_1, X0to10, method = "pearson")
cor.test(Pct_1, X10to20, method = "pearson")
cor.test(Pct_1, X20to30, method = "pearson")
cor.test(Pct_1, X30to40, method = "pearson")
cor.test(Pct_1, X40to50, method = "pearson")
cor.test(Pct_1, X50to60, method = "pearson")
cor.test(Pct_1, X60to70, method = "spearman")
cor.test(Pct_1, X70to80, method = "spearman")
cor.test(Pct_1, X80to90, method = "pearson")

# 5. Correlations Remain

cor.test(Pct_2, Net_immigration, method = "pearson")
cor.test(Pct_2, Population, method = "pearson")     
cor.test(Pct_2, Populationdensity, method = "pearson")   
cor.test(Pct_2, GDHIPH, method = "spearman")
cor.test(Pct_2, GGDHI, method = "pearson")
cor.test(Pct_2, Taxes, method = "pearson")
cor.test(Pct_2, Benefits, method = "pearson")
cor.test(Pct_2, Median_age, method = "pearson")
cor.test(Pct_2, X0to10, method = "pearson")
cor.test(Pct_2, X10to20, method = "pearson")
cor.test(Pct_2, X20to30, method = "pearson")
cor.test(Pct_2, X30to40, method = "pearson")
cor.test(Pct_2, X40to50, method = "pearson")
cor.test(Pct_2, X50to60, method = "pearson")
cor.test(Pct_2, X60to70, method = "spearman")
cor.test(Pct_2, X70to80, method = "spearman")
cor.test(Pct_2, X80to90, method = "pearson")

# 6. Change "outcome" variable into factor

Brexit$Outcome <- factor(Brexit$Outcome) 

# 7. Logistic regression 

library(carData)
library(car)

BrexitLogisticRegression1 <- glm(Outcome ~ Net_immigration + Population + Populationdensity + GDHIPH + GGDHI + Taxes + Benefits + Median_age + X0to10 + X10to20 + X20to30 + X30to40 + X40to50 + X50to60 + X60to70 + X70to80 + X80to90, data = Brexit, family="binomial")
summary(BrexitLogisticRegression1)
sqrt(vif(BrexitLogisticRegression1)) > 2 

# 9. Odds Ratio 

exp(coef(BrexitLogisticRegression1))

# 10 Relative Importance for GLM model 

library(caret)
varImp(BrexitLogisticRegression1)

# 11 ANOVA to analyze the table of deviance 

anova(BrexitLogisticRegression1, test = "Chisq")

# 12 Calculating R-squared

library(pscl)
pR2(BrexitLogisticRegression1)

# Remove all variables

rm(list=ls())

