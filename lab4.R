#lab4 

download.file("http://home.agh.edu.pl/~mmd/_media/dydaktyka/as-is/krakow-kurdwanow.zip","krakow-kurdwanow.zip")
unzip("krakow-kurdwanow.zip")
data <-dget("./krakow-kurdwanow")


summary(data$NO2)
boxplot(data$NO2 ~ format(data$date, "%m"), xlab= "months", ylab = "NO2")


#histogram
hist(data$NO2, col = "blue")
abline(v = median(data$NO2), lwd = 5, col ="red")

#4 historgrams with quartely data
par(mfrow = c(2,2))
hist(data[quarters(data$date) == "Q1",]$NO2)
hist(data[quarters(data$date) == "Q2",]$NO2)
hist(data[quarters(data$date) == "Q3",]$NO2)
hist(data[quarters(data$date) == "Q4",]$NO2)

#PM10 and PM2.5 pairs
par(mfrow=c(1,1))
with(data, plot(PM10, PM25))
#title
title("PM10 ~ PM2.5")
#additional markings with data from December
with(data[format(data$date, "%m") == "12",], points(PM10, PM25,
                                                    col="red"))
#legend
legend("topleft", pch = 1, col = "red", legend = "December")
#linear regression graph
model <- lm(data$PM25 ~ data$PM10)
abline(model)

#display num of meeasurements, save as pdf
pdf("plot.pdf")
barplot(table(format(data$date, "%m")))
dev.off()

#save as png
png("plot.png")
barplot(table(format(data$date, "%m")))
dev.off()

#display and save as pdf 
barplot(table(format(data$date, "%m")))
dev.copy(pdf, "plot2.pdf")
dev.off()

#hierachical clustering
distance <- function(x1,y1,x2,y2) {
  sqrt((x2-x1)^2 + (y2-y1)^2)
}
distance(24,13,64,53)

#define a dataframe
x <- c(rnorm(2)+2,rnorm(2)+8,rnorm(2)+2,rnorm(2)+8,rnorm(2)+5)
y <- c(rnorm(2)+2,rnorm(2)+8,rnorm(2)+8,rnorm(2)+2,rnorm(2)+5)
points <- data.frame(cbind(x,y))

#data in graphical form
plot(y ~ x, points)

#dataframe with pairs of points
df <- data.frame(nrow=0,ncol=4)
for(i in 1:10) {
  for(j in 1:10) {
    if(i>j) {
      df[10*(i-1)+j,1] <- points[i,1]
      df[10*(i-1)+j,2] <- points[i,2]
      df[10*(i-1)+j,3] <- points[j,1]
      df[10*(i-1)+j,4] <- points[j,2]
    }
  }
}
df <- df[complete.cases(df),]

#distance between two points
df$dist <- sqrt((df[,3]-df[,1])^2 + (df[,4]-df[,2])^2)

#sort dataframe by distance
df <- df[order(df$dist),]








