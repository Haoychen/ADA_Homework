# This is assignment 1
data("ChickWeight")
# a) EDA for variable weight
weight <- ChickWeight$weight

# Measures of Location: Mean, Median
mean(weight)
median(weight)

# Measures of dispersion: Sample Variance
var(weight)

# Histograms
hist(weight, probability = TRUE, breaks = 15, xlab = "Weight", ylab = "Density", main = "The histogram of weight")
lines(density(weight))

# box-plot
boxplot(weight, main = "Box-plot for Weight", xlab = 'Weight')

# Q-Q plot
qqnorm(weight)

# b) Estimate the bias associated with  the sample standard deviation and median of weight

# jackknife
jackknife <- function(func, data){
    estimator.sum <- 0
    for(i in 1:length(data)){
        estimator.sum <- estimator.sum + func(data[-i])
    }
    estimator.bias <- (length(data) - 1) * (estimator.sum / length(data) - func(data))
    return(estimator.bias)
}

jackknife(sd, weight)
jackknife(median, weight)

# Bootstrap
library(bootstrap)
bias.bootstrap <- function(data, func){
    estimator.sample <- func(data)
    estimator.bootstrap <- bootstrap(data, nboot = 100, theta = func)$thetastar
    return(mean(estimator.bootstrap - estimator.sample))
}

bias.bootstrap(weight, sd)
bias.bootstrap(weight, median)


