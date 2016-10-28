library(MASS)
data("birthwt")
multiLinearModel <- lm(bwt ~ age + lwt + race + smoke + ptl + ht + ui + ftv, data = birthwt)

# Multicolinearity
library(car)
vif(multiLinearModel)
mean(vif(multiLinearModel))


# ridge regression
RidgeRegressionModel <- lm.ridge(bwt ~ age + lwt + race + smoke + ptl + ht + ui + ftv, data = birthwt)

# lasso
library(glmnet)
x <- birthwt[c('age', 'lwt', 'race', 'smoke', 'ptl', 'ht', 'ui', 'ftv')]
y <- birthwt$bwt
LassoModel <- glmnet(as.matrix(x), y)
cv.LassoModel <- cv.glmnet(as.matrix(x), y)
plot(cv.LassoModel)
coef <- coef(cv.LassoModel, s = 'lambda.min')

# stepwise
stepAIC(multiLinearModel, direction = 'both')
stepAIC(multiLinearModel, direction = 'forward')
stepAIC(multiLinearModel, direction = 'backward')
