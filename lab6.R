#exercise section 3

#exercise 1
download.file("http://home.agh.edu.pl/~mmd/_media/dydaktyka/as-is/krakow-kurdwanow.zip","krakow-kurdwanow.zip")
unzip("krakow-kurdwanow.zip")
data <-dget("./krakow-kurdwanow")

#boxplot - PM2.5 divided into days of the week
boxplot(data$PM25 ~ format(data$date, "%a"), xlab= "weeks", ylab = "PM25")

#PM2.5 histogram
hist(data$PM25, col = "blue")

#PM2.5 change over time
par(mfrow=c(1,1))
with(data, plot(date, PM25))
#title
title("time  ~ PM2.5")
with(data[data$PM25 > 100,], points(date, PM25,col="red"))

#exercise 2
#1
download.file("http://home.agh.edu.pl/~mmd/_media/dydaktyka/as-is/lab4dataset.csv.zip","lab4dataset.csv.zip")
unzip("lab4dataset.csv.zip")
ex4data <-read.csv("./lab4dataset.csv",header=TRUE, sep=";")

library(lattice)

# 2 barchart of goals done
barchart(ex4data$team ~ex4data$gd, main="Teams", 
        xlab="Goals")

# 3 distinction to a team 
d1 <- ex4data[ex4data$team==1,]
d2 <- ex4data[ex4data$team==2,]

bwplot(d1$gs)
bwplot(d2$gs)

# 4 density plot of goals received grouped by a team
densityplot(~ex4data$gs|ex4data$team, main="Goals received",xlab="Team")

# 5 historgram of win matches 
histogram(~ex4data$win|ex4data$team, col = "blue")

# 6 goals received against goals done and play at home
xyplot(ex4data$gs~ex4data$gd + ex4data$home|ex4data$team, main="goals received ~ goals done & pah", ylab="Goals done", xlab="Goals received & play @ home")

# 7 qq plot
qq(ex4data$win ~ ex4data$gs + ex4data$inj | ex4data$team, aspect = 1)