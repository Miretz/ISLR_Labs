# ISLR page 42

#####################
# LAB
#####################

x <- c(1, 3, 2, 5)
x

x = c(1, 6, 2)
y = c(1, 4, 3)
length(x)
length(y)
x + y

ls()
rm(x, y)
ls()
rm(list = ls())

# Matix
x = matrix(data = c(1, 2, 3, 4),
           nrow = 2,
           ncol = 2)
x
matrix(c(1, 2, 3, 4), 2, 2, byrow = TRUE) # byrow - assign values by rows instead of columns (default)

sqrt(x)
x ^ 2

# rnorm() - random normal variables
# cor() - compute correlation between variables
x = rnorm(50)
y = x + rnorm(50, mean = 50, sd = .1)
cor(x, y)

# for reproducible random use a seed
set.seed(1303)
rnorm (50)

# mean, variance (var), standard deviation (sd)
# appliying sqrt() to var() will give us sd()
set.seed(3)
y = rnorm(100)
mean(y)
var(y)
sqrt(var(y))
sd(y)

# Graphics
x = rnorm(100)
y = rnorm(100)
plot(x, y)

plot(x,
     y,
     xlab = "this is the x-axis",
     ylab = "this is the y-axis",
     main = "Plot of X vs Y")

# Saving a plot to pdf and png (there is also jpeg())
pdf("Figure.pdf")
plot(x, y, col = "green")
dev.off()

png("Figure.png")
plot(x, y, col = "green")
dev.off()

# Sequences
seq(1, 10)
1:10
seq(-pi, pi, length = 50)

# plot using contour() function
x = 1:12 # x values
y = x # y values
f = outer(x, y, function(x, y)
  cos(y) / (1 + x ^ 2)) # z values - matrix wich contains z for each x,y coordinate
contour(x, y, f)
# display more levels
contour(x, y, f, nlevels = 45, add = T)
# different function - t() is for transpose
fa = (f - t(f)) / 2
contour(x, y, fa, nlevels = 15)

# heatmap using image() function
image(x, y, fa)

# 3d plot using persp() function
persp(x, y, fa)
# theta and phi control the camera angle
persp(x, y, fa, theta = 30)
persp(x, y, fa, theta = 30, phi = 20)
persp(x, y, fa, theta = 30, phi = 70)
persp(x, y, fa, theta = 30, phi = 40)

# Indexing data
A = matrix(1:16, 4, 4)
A
A[2, 3] # row, column
A[1:3, 4]
A[c(1, 3), 4]
A[1, 2:4]
A[, 1:3]
A[-4, ] # negative sign means keep all except the one that is specified

dim(A)

# Reading data
Auto = read.table("Auto.data")
fix(Auto) # opens a window with the data

Auto = read.table("Auto.data", header = T, na.strings = "?") # treat "?" as NA
fix(Auto)

# same with CSV
Auto = read.csv("Auto.csv", header = T, na.strings = "?")
fix(Auto)
dim(Auto)

# remove NAs - rows with NA will be removed
Auto = na.omit(Auto)
dim(Auto)


# Additional graphical and numerical summaries
library(ISLR)
head(Auto)

plot(Auto$cylinders, Auto$mpg)

# cylinders is actually a qualitative (categorical) variable instead of quantitative (numerical)
# we can convert it to a factor
Auto$cylinders = as.factor(Auto$cylinders)
# we will get a different plot as cylinders is now a factor
plot(Auto$cylinders, Auto$mpg)

# customize the boxplot
plot(
  Auto$cylinders,
  Auto$mpg,
  col = "red",
  varwidth = T,
  xlab = "cylinders",
  ylab = "MPG"
)

# histogram
hist(
  Auto$mpg,
  col = "blue",
  breaks = 15,
  xlab = "MPG",
  main = "Histogram of Auto MPG"
)

# scatterplots for combinations of features
pairs(Auto)
pairs( ~ mpg + cylinders + horsepower, Auto) # this is how you choose which ones

# we could identify certain points on the plot and display values using identify()
# this launches an interactive tool, click on point to identify, click finish to exit
plot(Auto$horsepower, Auto$mpg)
identify(Auto$horsepower, Auto$mpg, Auto$name)

# Summary
summary(Auto)
summary(Auto$name)
summary(Auto$mpg)

#####################
# EXERCISES
#####################


# Exercise 7 - KNN implementation


X <-
  matrix(
    c(0, 3, 0, 2, 0, 0, 0, 1, 3, 0, 1, 2, -1, 0, 1, 1, 1, 1),
    nrow = 6,
    ncol = 3,
    byrow = TRUE
  )
Y <- c("red", "red", "red", "green", "green", "red")

X0 <- matrix(c(0),
             nrow = 6,
             ncol = 3,
             byrow = TRUE)

euc.dist <- function(x1, x2)
  sqrt(sum((x1 - x2) ^ 2))

distances <- sapply(1:nrow(X), function(i)
  euc.dist(X[i, ], X0[i, ]))

df_distances <- data.frame(distance = distances, label = Y)
df_distances$label <- as.factor(df_distances$label)
df_distances <- df_distances[order(-df_distances$distance), ]
df_distances

# K = 1
df_distances[1, ]

# K = 3
freq <- table(df_distances[1:3, 2])
prop.table(freq)


# Exercise 8 - College data


data("College")
summary(College)
pairs(College[, 1:10])

plot(College$Private,
     College$Outstate,
     col = "red")

College$Elite <- "No"
College$Elite[College$Top10perc > 50] = "Yes"
College$Elite <- as.factor(College$Elite)
summary(College$Elite)

plot(College$Elite,
     College$Outstate,
     col = "red")

par(mfrow = c(2, 2))
hist(College$Apps)
hist(College$Enroll)
hist(College$Books)
hist(College$PhD)
par(mfrow = c(1, 1))


# Exercise 9 - Auto data


data("Auto")
dim(Auto)
summary(Auto)

Auto$cylinders <- as.factor(Auto$cylinders)
Auto$year <- as.factor(Auto$year)
Auto$origin <- as.factor(Auto$origin)
summary(Auto)

range(Auto$mpg)


df_auto <- Auto[-(10:85),]
summary(df_auto)

# Investigate predictors graphically
par(mfrow = c(2, 2))
plot(Auto$mpg, Auto$horsepower)
plot(Auto$weight, Auto$horsepower)
plot(Auto$acceleration, Auto$horsepower)
plot(Auto$cylinders, Auto$weight)
par(mfrow = c(1, 1))

# Exercise 10

library(MASS)
data(Boston)
dim(Boston)
summary(Boston)

# CRIM - per capita crime rate by town
# ZN - proportion of residential land zoned for lots over 25,000 sq.ft.
# INDUS - proportion of non-retail business acres per town.
# CHAS - Charles River dummy variable (1 if tract bounds river; 0 otherwise)
# NOX - nitric oxides concentration (parts per 10 million)
# RM - average number of rooms per dwelling
# AGE - proportion of owner-occupied units built prior to 1940
# DIS - weighted distances to five Boston employment centres
# RAD - index of accessibility to radial highways
# TAX - full-value property-tax rate per $10,000
# PTRATIO - pupil-teacher ratio by town
# LSTAT - % lower status of the population
# MEDV - Median value of owner-occupied homes in $1000's

attach(Boston)
plot(age, crim)
plot(as.factor(rad), crim)
plot(medv, crim)
plot(lstat, crim)

# correlations
correlations <- round(cor(Boston), 2)
correlations <- as.data.frame(as.table(correlations))
correlations <-
  correlations[correlations$Var1 != correlations$Var2,]
correlations <- correlations[order(-correlations$Freq),]
correlations[correlations$Var1 == "crim",]

# Visualize correlations
library(corrgram)
corrgram(
  Boston,
  order = TRUE,
  lower.panel = panel.shade,
  upper.panel = panel.pie,
  text.panel = panel.txt,
  main = "Boston"
)

# how many areas bounds river
chas_factor <- as.factor(chas)
summary(chas_factor)
# 35

# median pupil-teach
summary(ptratio)

# lowest medv
lowest_medv <- Boston[order(medv), ][1, ]
lowest_medv

# Draws histograms with highlighted values
highlight <- function(x, value, col.value, col = NA, ...) {
  pdf(file = NULL)
  hst <- hist(x, ...)
  dev.off()
  idx <- findInterval(value, hst$breaks)
  cols <- rep(col, length(hst$counts))
  cols[idx] <- col.value
  hist(x, col = cols, ...)
}

# Histograms of the data with highlithed value from lowest medv
par(mfrow = c(4, 4))
for (i in 1:ncol(lowest_medv)) {
  highlight(Boston[, i],
            lowest_medv[, i],
            "red",
            ylab = "",
            main = (names(lowest_medv)[i]))
}
par(mfrow = c(1, 1))

# More than 7 rooms
dim(Boston[Boston$rm > 7,])

# More than 8 rooms
dim(Boston[Boston$rm > 8,])


