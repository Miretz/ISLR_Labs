# ISLR page 109

#####################
# LAB
#####################

library(MASS)
library(ISLR)


# SIMPLE LINEAR REGRESSION


names(Boston)

lm.fit <- lm(medv ~ lstat, data = Boston)
lm.fit

summary(lm.fit)
names(lm.fit)
coef(lm.fit)
confint(lm.fit)

predict(lm.fit, data.frame(lstat = c(5, 10, 15)), interval = "confidence")

predict(lm.fit, data.frame(lstat = c(5, 10, 15)), interval = "prediction")

# plotting
plot(Boston$lstat , Boston$medv)
abline(lm.fit)

abline(lm.fit, lwd = 3) # lwd - line width
abline(lm.fit, lwd = 3, col = "red")

plot(Boston$lstat, Boston$medv, pch = 20) # pch - point symbol
plot(Boston$lstat, Boston$medv, pch = "+")
plot(1:20, 1:20, pch = 1:20)

# plot the lm
par(mfrow = c(2, 2))
plot(lm.fit)
par(mfrow = c(1, 1))

# plot residuals
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))

# leverage statistics
plot(hatvalues(lm.fit))
which.max(hatvalues (lm.fit))


# MULTIPLE LINEAR REGRESSION


lm.fit = lm(medv ~ lstat + age, data = Boston)
summary(lm.fit)

lm.fit = lm(medv ~ ., data = Boston)
summary(lm.fit)

summary(lm.fit)$r.sq # R-squared
summary(lm.fit)$sigma # Sigma (RSE)

library(car)
vif(lm.fit)

# age has a high p-value, we can remove it
lm.fit1 = lm(medv ~ . - age, data = Boston)
summary(lm.fit1)

# alternative - we can use update
lm.fit1 = update(lm.fit, ~ . - age)
summary(lm.fit1)

# Adding interaction terms
# lstat:age - include interaction term between lstate and age
# lstate*age - same as above, but also include original lstat and age
summary(lm(medv ~ lstat:age, data = Boston))
summary(lm(medv ~ lstat * age, data = Boston))


# Non-linear transformations
# function I() allows us to include standard operators as ^
lm.fit2 = lm(medv ~ lstat + I(lstat ^ 2), data = Boston)
summary(lm.fit2)

# Using anova function to compare linear and quadratic fit
# in this case the model with lstat + lstat^2 performs better
lm.fit = lm(medv ~ lstat, data = Boston)
anova(lm.fit , lm.fit2)

par(mfrow = c(2, 2))
plot(lm.fit2)
par(mfrow = c(1, 1))

# for higher polynomials we can use the poly() function
lm.fit5 = lm(medv ~ poly(lstat , 5), data = Boston)
summary(lm.fit5)
# in this case adding more polynomials leads to a better fit

# example with log
summary(lm(medv ~ log(rm), data = Boston))


# QUALITATIVE PREDICTORS


names(Carseats)
# predicting sales of car seats
lm.fit = lm(Sales ~ . + Income:Advertising + Price:Age, data = Carseats)
summary(lm.fit)

attach(Carseats)
contrasts(ShelveLoc) # contrasts() shows the encoding used for variables


#####################
# EXERCISES
#####################

# 3.b
gpa = 4.0
iq = 110
gender = 1

salary = 50 + (20 * gpa) + (0.07 * iq) + (35 * gender) + (0.01 * (gpa * iq)) +
  (-10 * (gpa * gender))
salary

# 8.a
mpg.fit = lm(mpg ~ horsepower, data = Auto)
summary(mpg.fit)

predict(mpg.fit, data.frame(horsepower = 98))
predict(mpg.fit, data.frame(horsepower = 98), interval = "confidence")
predict(mpg.fit, data.frame(horsepower = 98), interval = "prediction")

# 8.b
plot(Auto$horsepower, Auto$mpg)
abline(mpg.fit, col = "red", lwd = 2)

# 8.c
par(mfrow = c(2, 2))
plot(mpg.fit)
par(mfrow = c(1, 1))


# 9.a
pairs(Auto)

# 9.b
round(cor(subset(Auto, select = -name)), 2)

library(corrgram)
corrgram(
  subset(Auto, select = -name),
  order = TRUE,
  lower.panel = panel.shade,
  upper.panel = panel.pie,
  text.panel = panel.txt,
  main = "Auto correlations"
)

# 9.c
mpg.fit2 = lm(mpg ~ . - name, data = Auto)
summary(mpg.fit2)

# 9.d
par(mfrow = c(2, 2))
plot(mpg.fit2)
par(mfrow = c(1, 1))

# Residuals vs Fitted - curve copies the data - evidence of non-linearity
# Observation 14 - high leverage

# 9.e
names(Auto)
summary(lm(mpg ~ displacement + weight + year + origin + horsepower, data =
             Auto))
summary(lm(
  mpg ~ displacement + weight + year:origin + horsepower * acceleration,
  data = Auto
))
summary(lm(
  mpg ~ displacement + weight + year * origin + horsepower * acceleration,
  data = Auto
))
summary(lm(
  mpg ~ displacement * weight + year * origin + horsepower * acceleration,
  data = Auto
))

# 9.f
summary(lm(
  mpg ~ poly(displacement, 3) + weight + year + origin + horsepower,
  data = Auto
))
summary(lm(
  mpg ~ poly(displacement, 3) + I(log(weight)) + year + origin + horsepower,
  data = Auto
))

# 10.a
sales.fit = lm(Sales ~ Price + Urban + US, data = Carseats)
summary(sales.fit)

# 10.e
# UrbanYes has a higher P-value so we cannot fully reject the null hypothesis
sales.fit2 = lm(Sales ~ Price + US, data = Carseats)
summary(sales.fit2)

# 10.f
summary(sales.fit)$r.sq
summary(sales.fit2)$r.sq
# RSE - lower is better
summary(sales.fit)$sigma
summary(sales.fit2)$sigma

# 10.g
confint(sales.fit2)

# 10.h
par(mfrow = c(2, 2))
plot(sales.fit2)
par(mfrow = c(1, 1))

plot(predict(sales.fit2), rstudent(sales.fit2))
plot(hatvalues(sales.fit2))

# 11
set.seed(1)
x = rnorm(100)
y = 2 * x + rnorm(100)

# 11.a
y.fit = lm(y ~ x + 0)
summary(y.fit)

# 11.b
x.fit = lm(x ~ y + 0)
summary(x.fit)

# 11.c
# should be the same with axes switched
par(mfrow = c(1, 2))
plot(x, y)
abline(y.fit, col = "red")
plot(y, x)
abline(x.fit, col = "blue")
par(mfrow = c(1, 1))

# 11.f
y.fit2 = lm(y ~ x)
x.fit2 = lm(x ~ y)
summary(y.fit2)
summary(x.fit2)

# 12.a
coef(y.fit)
coef(x.fit)

# 12.b
# also example 11 works
x2 = rnorm(50)
y2 = x2 + rnorm(50)
coef(lm(y2 ~ x2))
coef(lm(x2 ~ y2))

# 12.c
set.seed(1)
x3 = rnorm(50)
y3 = x3 + 5
coef(lm(y3 ~ x3))
coef(lm(x3 ~ y3))

# 13.a
set.seed(1)
x = rnorm(100)

# 13.b
eps = rnorm(100, sd = 0.25 ^ 0.5)

# 13.c
beta0 = -1
beta1 = 0.5
y = beta0 + beta1 * x + eps
length(y)

# 13.d
plot(x, y)

# 13.e
fit = lm(y ~ x)
summary(fit)
coef(fit)

# 13.f
plot(x, y)
abline(beta0, beta1, col = "blue")
abline(fit, col = "red")
legend(
  x = c(1, 2.7),
  y = c(-2.5,-2),
  legend = c("population", "model"),
  col = c("blue", "red"),
  lwd = 2
)

# 13.g
fit2 = lm(y ~ x + I(x ^ 2))

plot(x, y)
abline(beta0, beta1, col = "blue")
abline(fit2, col = "red")
legend(
  x = c(1, 2.7),
  y = c(-2.5,-2),
  legend = c("population", "model"),
  col = c("blue", "red"),
  lwd = 2
)

summary(fit2)
anova(fit, fit2)
coef(fit)
coef(fit2)

# 13.h
eps2 = rnorm(100, sd = 0.01 ^ 0.5)
y2 = beta0 + beta1 * x + eps2
fit3 = lm(y2 ~ x)

plot(x, y2)
abline(beta0, beta1, col = "blue")
abline(fit3, col = "red")
legend(
  x = c(1, 2.7),
  y = c(-3,-2),
  legend = c("population", "model"),
  col = c("blue", "red"),
  lwd = 2
)

summary(fit3)
coef(fit3)

# 13.i
eps3 = rnorm(100, sd = 1)
y3 = beta0 + beta1 * x + eps3
fit4 = lm(y3 ~ x)

plot(x, y3)
abline(beta0, beta1, col = "blue")
abline(fit4, col = "red")
legend(
  x = c(1, 2.7),
  y = c(-3,-2),
  legend = c("population", "model"),
  col = c("blue", "red"),
  lwd = 2
)

summary(fit4)
coef(fit4)


# 14.a
set.seed(1)
x1 = runif(100)
x2 = 0.5 * x1 + rnorm(100) / 10
y = 2 + 2 * x1 + 0.3  * x2 + rnorm(100)

# 14.b
plot(x1, x2)
cor(x1, x2)

# 14.c
fit = lm(y ~ x1 + x2)
summary(fit)
coef(fit)

# 14.d
summary(lm(y ~ x1))

# 14.e
summary(lm(y ~ x2))

# 14.g
x1 = c(x1, 0.1)
x2 = c(x2, 0.8)
y = c(y, 6)

plot(x1, y, col = ifelse(x1 %in% c(0.1), 'red', 'blue'), pch = 20)
plot(x2, y, col = ifelse(x2 %in% c(0.8), 'red', 'blue'), pch = 20)

summary(lm(y ~ x1 + x2))
summary(lm(y ~ x1))
summary(lm(y ~ x2))

# 15.a
data(Boston)

getPValue <- function(df) {
  model = lm(crim ~ Boston[, as.character(df)], data = Boston)
  f = summary(model)$fstatistic
  p = pf(f[1], f[2], f[3], lower.tail = F)
  return(p)
}
result = data.frame(predictors = names(Boston)[-1])
result$pvalue = lapply(result$predictors, getPValue)
result$significant = "NO"
result$significant[result$pvalue < 0.1] = "YES"
result

plot(result$predictors, result$pvalue)

summary(lm(crim ~ chas, data = Boston))
plot(as.factor(Boston$chas), Boston$crim)

# 15.b
fit = lm(crim ~ ., data = Boston)
summary(fit)

# 15.c
getCoef <- function(df) {
  model = lm(crim ~ Boston[, df], data = Boston)
  cf = coef(model)[2]
  names(cf) = df
  return(cf)
}
cf_a = unlist(lapply(names(Boston)[-1], getCoef))
cf_b = coef(fit)[-1]
plot(cf_a, cf_b, pch = 20, col = "blue")
text(cf_a,
     cf_b,
     labels = names(cf_a),
     cex = 0.7,
     pos = 3)
# Nox is way off

# 15.d
summary(lm(crim ~ poly(zn, 3), data = Boston))
summary(lm(crim ~ poly(indus, 3), data = Boston))
summary(lm(crim ~ poly(nox, 3), data = Boston))
summary(lm(crim ~ poly(rm, 3), data = Boston))
summary(lm(crim ~ poly(age, 3), data = Boston))
summary(lm(crim ~ poly(dis, 3), data = Boston))
summary(lm(crim ~ poly(rad, 3), data = Boston))
summary(lm(crim ~ poly(tax, 3), data = Boston))
summary(lm(crim ~ poly(ptratio, 3), data = Boston))
summary(lm(crim ~ poly(black, 3), data = Boston))
summary(lm(crim ~ poly(lstat, 3), data = Boston))
summary(lm(crim ~ poly(medv, 3), data = Boston))
