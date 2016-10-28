diet.data <- read.table('diet.dat', header = TRUE)
diet.data <- diet.data[-c(18, 43),]


# without adjusting for week 3 weight
anov1 <- aov(dmi ~ trt, data = diet.data)
summary(anov1)

# adjusting for week 3 weight
diet.data$covar <- as.numeric(diet.data$covar)
anov2 <- aov(dmi ~ covar + trt, data = diet.data)
summary(anov2)

library(lsmeans)
diet.lm <- lm(dmi ~ covar + trt, data = diet.data)
summary(diet.lm)
lsmeans(diet.lm, 'covar')


# Test for parallelism
anov3 <- aov(dmi ~ covar*trt, data = diet.data)
summary(anov3)

# validation
qqnorm(diet.lm$residuals)
bartlett.test(diet.data$dmi, diet.data$trt)

# 2
diet.data$dmi_avg = diet.data$dmi/diet.data$weeks
diet.data$covar_avg = diet.data$covar/diet.data$weeks
anova4 = aov(dmi_avg~trt, data = diet.data)
summary(anova4)
anova5 = aov(dmi_avg~covar_avg + trt, data = diet.data)
summary(anova5)

