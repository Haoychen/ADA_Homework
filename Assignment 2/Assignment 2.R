# This is assignment 1
data("chickwts")

# 1 test significant diff
soybean <- chickwts[chickwts$feed == 'soybean',][['weight']]
casein <- chickwts[chickwts$feed == 'casein',][['weight']]

# parametric procedure
t.test(soybean, casein, var.equal = TRUE)

# Non-parametric procedure
wilcox.test(soybean, casein)

# re-sampling procedure
bootstrap.test <- function(x, y, B){
    x.mean <- mean(x)
    y.mean <- mean(y)
    x.var <- var(x)
    y.var <- var(y)
    n <- length(x)
    m <- length(y)
    z.obs <- abs((x.mean - y.mean) / sqrt(x.var / n + y.var / m))
    y <- y + x.mean - y.mean
    z <- c()
    for(i in 1:B){
        x.star <- sample(x, n, replace = TRUE)
        y.star <- sample(y, m, replace = TRUE)
        x.star.mean <- mean(x.star)
        y.star.mean <- mean(y.star)
        x.star.var <- var(x.star)
        y.star.var <- var(y.star)
        z[i] <- abs((x.star.mean - y.star.mean) / sqrt(x.star.var / n + y.star.var / m))
    }
    return(sum(z > z.obs) / B)
}

bootstrap.test(soybean, casein, 10000)


# 2 confidenct interval
sunflower <- chickwts[chickwts$feed == 'sunflower',][['weight']]

# a. 95% confidence interval for median

MedianDiff.ConfidenceInterval <- function(x, y, B){
    median.diff <- c()
    for(i in 1: B){
        x.sample <- sample(x, length(x), replace = T)
        y.sample <- sample(y, length(y), replace = T)
        median.diff[i] <- median(x.sample) - median(y.sample)
    }
    print(quantile(median.diff, c(.025, .975)))
}

MedianDiff.ConfidenceInterval(casein, sunflower, 100000)



# b. 95% CI for ratio of the variance
ratio.ConfidenceInterval <- function(x, y, B){
    ratio <- c()
    for(i in 1: B){
        x.sample <- sample(x, length(x), replace = T)
        y.sample <- sample(y, length(y), replace = T)
        ratio[i] <- var(x.sample) / var(y.sample)
    }
    print(quantile(ratio, c(.025, .975)))
}

ratio.ConfidenceInterval(casein, sunflower, 10000)


# c. 95% CI for ratio of the variance under normal assumption
NormalRatio.ConfidenceInterval <- function(x, y, B){
    ratio <- c()
    for(i in 1: B){
        x.sample <- sample(x, length(x), replace = T)
        y.sample <- sample(y, length(y), replace = T)
        ratio[i] <- var(x.sample) / var(y.sample)
    }
    print(t.test(ratio)$conf.int)
}

NormalRatio.ConfidenceInterval(casein, sunflower, 10000)


# 3 significant diff

meatmeal <- chickwts[chickwts$feed == 'meatmeal',][['weight']]
meatmeal.LowWeight <- sum(meatmeal < 256)
soybean.LowWeight <- sum(soybean < 256)
x <- c(meatmeal.LowWeight, soybean.LowWeight)
n <- c(length(meatmeal), length(soybean))
prop.test(x, n)
