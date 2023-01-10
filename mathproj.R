install.packages("tidyverse")
library(tidyverse)

dataset <- read.csv(file='C:/Users/Rebecca/Desktop/covidcrash_csv.csv',check.names=F,stringsAsFactors=T)
attach(dataset)



head(dataset)


# fatalities

prop.test(x = c(1237,1129), n = c(65829, 43325 ), alternative = "less",
          correct = TRUE)

## p-value < 4.275e-16




# speed violations

prop.test(x = c(2161, 1263), n = c(65829, 43325 ), alternative = "less",
          correct = TRUE)

## p-value = 0.9997

prop.test(x = c(2161, 1263), n = c(65829, 43325 ), alternative = "two.sided",
          correct = TRUE)

## p-value = 0.0006968



# dui violations

prop.test(x = c(1675,1208), n = c(65829, 43325 ), alternative = "less",
          correct = TRUE)

## p-value = 0.007388


# restraint violations

prop.test(x = c(249,206), n = c(65829, 43325 ), alternative = "less",
          correct = TRUE)

## p-value = 0.008397



# fatalities

prop.test(x = c(1237,1129), n = c(65829, 43325 ), alternative = "less",
          correct = TRUE)

## p-value < 4.275e-16


redglmmod = glm(FATALITY ~   SPEED_Vio + PANDEMIC, family = binomial)
summary(redglmmod)


# AIC: 22934


glmmod = glm(Fatality ~ Restraint_Vio + DUI_Vio + Speed_Vio + Pandemic + Urban + Rollover + SurfaceCond, family = binomial)

summary(glmmod)

# AIC: 22937


pchisq(22927,109149)


b  = -3.92793
x1 = -1.15363
x2 =  0.34336

sum= b + x1 + x2
prob = (exp(sum))/(1 + (exp(sum)))
prob

sum2 = b +x2
prob2 = (exp(sum2))/(1 + (exp(sum2)))
prob2

prob3 = (exp(b))/(1 + (exp(b)))
prob3

(prob2-prob3)/prob3




