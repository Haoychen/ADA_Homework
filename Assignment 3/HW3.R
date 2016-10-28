library('MASS')
data("Pima.te")
Pima.te <- Pima.te[c('glu', 'npreg', 'bp', 'skin', 'bmi', 'age')]
# a) Fit a multiple linear regression model
LinearModel <- lm(glu ~ npreg + bp + skin + bmi + age, data = Pima.te)
summary(LinearModel)

# b) State and assess the validity of the underlying assumptions
# Linearity
library(GGally)
ggpairs(Pima.te)

# interaction
LinearModelwithAllInteractions = lm(glu ~ npreg + bp + skin + bmi + age + npreg*bp + npreg*skin + npreg*bmi +
                                        npreg*age + bp*skin + bp*bmi+ bp*age + skin*bmi + skin*age + bmi*age, data = Pima.te)
summary(LinearModelwithAllInteractions)
LinearModelwithTwoInteractions = lm(glu ~ npreg + bp + skin + bmi + age + bp*age + skin*age, data = Pima.te)
summary(LinearModelwithTwoInteractions)
LinearModelwithOneInteractions = lm(glu ~ npreg + bp + skin + bmi + age + skin*age, data = Pima.te)
summary(LinearModelwithOneInteractions)




# Non-normality
hist(LinearModel$residuals, main = 'Histogram of residuals')
qqnorm(LinearModel$residuals)

# Homoscedasticity
plot(LinearModel$residuals, Pima.te$npreg, xlab = 'residuals', ylab = 'npreg')
plot(LinearModel$residuals, Pima.te$bp, xlab = 'residuals', ylab = 'bp')
plot(LinearModel$residuals, Pima.te$skin, xlab = 'residuals', ylab = 'skin')
plot(LinearModel$residuals, Pima.te$bmi, xlab = 'residuals', ylab = 'bmi')
plot(LinearModel$residuals, Pima.te$age, xlab = 'residuals', ylab = 'age')

# Uncorrelated error
library(car)
durbinWatsonTest(LinearModel)


# Check for outliers and influential points
infIndexPlot(LinearModel)



# c Least Median of Squares Regression
LeastMedianModel <- lmsreg(glu ~ npreg + bp + skin + bmi + age, data = Pima.te)
summary(LeastMedianModel)
coef(LeastMedianModel)
coef(LinearModel)
