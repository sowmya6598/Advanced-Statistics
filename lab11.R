setwd()

#Linear Regression

download.file("http://home.agh.edu.pl/~mmd/_media/dydaktyka/
as-is/krakow-kurdwanow.zip", "krakow-kurdwanow.zip")
unzip("krakow-kurdwanow.zip")
data <- dget("./krakow-kurdwanow")

y <- data$PM25
x <- data$PM10

good <- complete.cases(x,y)
y <- y[good]
x <- x[good]

n <- length(x)
l <- (n*sum(x*y)-sum(x)*sum(y))
m <- sqrt((n*sum(x^2) - sum(x)^2) * (n*sum(y^2) - sum(y)^2))
l/m

cor(x,y)

ymean <- mean(y)
xmean <- mean(x)

y <- y - ymean
x <- x - xmean

mean(y)
mean(x)

sum_of_the_squares <- function (y,x,a) {
  sum <- 0
  for (i in seq_along(y)) {
    sum <- sum + (y[i] - (a*x[i]))^2
  }
  sum
}

find_a <- function (y,x,a_vector) {
  min_sum <- Inf
  min_a <- NA
  for (a in a_vector) {
    sum <- sum_of_the_squares(y,x,a)
      if(sum < min_sum) {
        min_sum <- sum
        min_a <- a
      }
  }
  min_a
}

a <- 10

for (i in 0:10) {
  a <- find_a(y,x,seq(a-10^(-i+1), a+10^(-i+1), 10^(-i)))
}
a

model <- lm(y ~ x)
model$coefficients["x"]

model$coefficients["x"] - a

plot(y~x)
abline(0,a, col = "red", lwd=7)
abline(model, col="blue", lwd=3)

plot(data$PM25~data$PM10)
abline(ymean-a*xmean,a, col="red", lwd=5)
model <- lm(data$PM25 ~ data$PM10)
abline(model, col="white")

#Exercises

#2

pm10<- data$PM10
so2<- data$SO2
no2<- data$NO2

good<- complete.cases(pm10,so2,no2)

pm10<- pm10[good]
so2<- so2[good]
no2<- no2[good]

#3
cor(so2,no2) #0.5756984
cor(so2,pm10)#0.7615869
cor(no2,pm10)#0.7031167

#4
model<- lm(so2~pm10)
model$coefficients[1]
model$coefficients[2]

#5
plot(so2~pm10)
abline(model,col="blue",lwd=3)

#6
sum_of_the_squares_line<- function(y,x,a,b){
  sum<-0
  for(i in seq_along(y)){
    sum<- sum + (y[i] - (a*x[i]+b))^2
  }
  sum
}

sum_of_the_squares_line(so2,pm10,model$coefficients[2],model$coefficients[1])





