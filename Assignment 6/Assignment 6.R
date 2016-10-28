library(dplyr)
library(MASS)

data("ChickWeight")
ChickWeight$Diet <- factor(ChickWeight$Diet)
ChickWeight$Time <- factor(ChickWeight$Time)
ChickWeight$Chick <- factor(ChickWeight$Chick)
ChickWeight.filter <- filter(ChickWeight,Time==4 | Time==8 |Time==12 | Time==16 | Time==20)

# 1
library(nlme)
ChickWeight.repeat <- groupedData(weight~as.numeric(Diet)*as.numeric(Time)|Chick,data=ChickWeight.filter)
fit.cs <- gls(weight ~ Diet * Time, data=ChickWeight.repeat, corr=corCompSymm(,form=~1|Chick))
anova(fit.cs)


fit.un <- gls(weight~Diet*Time,data=ChickWeight.repeat,corr=corSymm(form = ~1|Chick),weights = varIdent(form = ~1|Time))
anova(fit.un)

fit.ar1 <- gls(weight~Diet*Time,data=ChickWeight.repeat,corr=corAR1(,form=~1|Chick))
anova(fit.ar1)

anova(fit.cs,fit.un)
anova(fit.cs,fit.ar1)

# 2
BirthWeight <- ChickWeight$weight
ChickWeight.birth <- cbind(ChickWeight,BirthWeight)
for (i in 1 : nrow(ChickWeight.birth)){
    chick.index <- ChickWeight.birth$Chick[i]
    ChickWeight.birth$BirthWeight[i] <- 
        ChickWeight.birth$weight[which(ChickWeight.birth$Chick == chick.index 
                                     & ChickWeight.birth$Time == 0)]
}
ChickWeight.birth_filter <- filter(ChickWeight.birth,Time==4 | Time==8 |Time==12 | Time==16 | Time==20)
ChickWeight.birth_repeated <- groupedData(weight~as.numeric(Diet)*as.numeric(Time)|Chick,data=ChickWeight.birth_filter)

fit.cs1 <- gls(weight~Diet*Time+BirthWeight,data=ChickWeight.birth_repeated,corr=corCompSymm(,form=~1|Chick))
anova(fit.cs1)

fit.un1 <- gls(weight~Diet*Time+BirthWeight,data=ChickWeight.birth_repeated,corr=corSymm(form = ~1|Chick),weights = varIdent(form = ~1|Time))
anova(fit.un1)

fit.ar11 <- gls(weight~Diet*Time+BirthWeight,data=ChickWeight.birth_repeated,corr=corAR1(,form=~1|Chick))
anova(fit.ar11)

# 3
qqnorm(fit.cs$residuals)
hist(fit.cs$residuals)
qqnorm(fit.un$residuals)
hist(fit.un$residuals)
qqnorm(fit.ar1$residuals)
hist(fit.ar1$residuals)

bartlett.test(x=ChickWeight.filter$weight,g=ChickWeight.filter$Diet)

summary(aov(ChickWeight.filter$weight ~ ChickWeight.filter$Diet*ChickWeight.filter$Time))
