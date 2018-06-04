

?swiss
data(swiss)

cor(swiss)

round(cor(swiss), 2)

cor.test(swiss$Fertility, swiss$Education)

install.packages("Hmisc")
require("Hmisc")

rcorr(as.matrix(swiss))



?trees
data(trees)
trees[1:5,]

hist(trees$Height)
hist(trees$Girth)
plot(trees$Girth, trees$Height)

abline(lm(trees$Height ~ trees$Girth))

reg1 <- lm(Height ~ Girth, data = trees)
reg1

summary(reg1)
confint(reg1)
predict(reg1)

predict(reg1, interval="prediction")




#EXERCISES

install.packages('mlbench')

#EXERCISE1

library(mlbench)
data("BostonHousing")
housing <- BostonHousing
str(housing)


#EXERCISE2

summary(housing$medv)


#EXERCISE 3

library(ggplot2)
select(c(crim, rm, age, rad, tax, lstat, medv)) 


#EXERCISE 4

library("caret")
set.seed(123)
train <- createDataPartition(y = housing$medv, p = 0.75, list = FALSE)
train <- housing[to_train, ]
test <- housing[-train, ]


#EXERICSE 5

linear_model <- lm(medv ~ crim + rm + tax + lstat, data = train)


#EXERCISE 6

r_squared <- summary(linear_model)$r.squared
print(paste("linear model having r-squared values", round(r_squared), sep = ""))
plot(linear_model)


#EXERCISE 7

liner_model2 <- lm(log(medv) ~ crim + rm + tax + lstat, data = train)


#EXERCISE 8

r_squared2 <- summary(liner_model2)$r.squared
print(paste("second linear model having r-squared values ", round(r_squared2), sep = ""))
plot(liner_model2)

mean(liner_model2$residuals)


#EXERCISE 9

predicted_values <- predict(liner_model2, newdata = test)
results <- data.frame(predicted_values = exp(predicted_values), original = test$medv)


#EXERCISE 10

