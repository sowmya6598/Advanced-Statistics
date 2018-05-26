#lab 9 - Statistical interference
?state.area

data(state.area)

area <- state.area

hist(area)

boxplot(area)
boxplot.stats(area)
summary(area)

#5 last median (with trim=0.5) is the robust one because it is equal to the median value
mean(area)
median(area)
mean(area, trim = .05)
mean(area, trim = .10)
mean(area, trim = .20)
mean(area, trim = .50)

sd(area)
mad(area) # Median absolute deviation
IQR(area)

#part 3 modyfing data

download.file("http://home.agh.edu.pl/~mmd/_media/dydaktyka/
as-is/os.csv.zip", "os.csv.zip")
unzip("os.csv.zip")
OS <- read.csv("./os.csv", header = TRUE)
View(OS)

OS.hi <- subset(OS, Proportion > 0.1)
OS.hi

data(rivers)
river <- rivers

hist(river)
boxplot(river)
#list of outliers
boxplot.stats(river)

#remove outliers
river.low<- river[river <1210]
boxplot(river.low,horizontal=TRUE)
boxplot.stats(river.low)
#the outliers still appear because the he cut off value is not chosen appropriately, if we changed value to 1100 we are
#left with only 1 outlier.

river.low2 <- rivers[river < 1055]
boxplot(river.low2)

#now there's only one outlier left

#transforming variables

data(islands)

hist(islands)
boxplot(islands)

island.z <- scale(islands)
island.z

mean(island.z)
round(mean(island.z),2)

sd(island.z)

attr(island.z, "scaled:center")
attr(island.z, "scaled:scale")

island.z <- as.numeric(island.z)
island.z

islands.ln <- log(islands)

islands.rank <- rank(islands)

continent <- ifelse(islands>1000,1,0)
continent

#comparing means with 2 sample t-test
?sleep
sleep[1:5,]

sd <- sleep[,1:2]

hist(sleep$extra,col="lightgray")
boxplot(extra ~ group, data = sd)

t.test(extra ~ group, data=sd)

t.test(extra~group,data=sd)

t.test(extra~group, data = sd, alternative = "less", conf.level = 0.8)

x<- rnorm(30,mean=20,sd=5)
y<- rnorm(30,mean=22,sd=5)
#different results:
# > x<- rnorm(30,mean=20,sd=5)
# > y<- rnorm(30,mean=22,sd=5)
# > 
#   > t.test(x,y)
# 
# Welch Two Sample t-test
# 
# data:  x and y
# t = -1.6869, df = 57.088, p-value = 0.09707
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -4.2101106  0.3600023
# sample estimates:
#   mean of x mean of y 
# 20.13213  22.05719 
# 
# > x<- rnorm(30,mean=20,sd=5)
# > y<- rnorm(30,mean=22,sd=5)
# > 
#   > t.test(x,y)
# 
# Welch Two Sample t-test
# 
# data:  x and y
# t = -3.0153, df = 53.73, p-value = 0.003915
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -5.608628 -1.128518
# sample estimates:
#   mean of x mean of y 
# 18.51657  21.88514 
t.test(x,y)

#comparing paired means with paired t-test
t1<-rnorm(50,mean=52,sd=6)
dif<- rnorm(50,mean=6,sd=12)
t2 <- t1 + dif
hist(t1)
hist(t2)
hist(dif)
#everytime you run the code the histogram changes, but all values look similar to gaussian distribution
boxplot(t1)
boxplot(t2)
#boxplot also differs, depending on the result of rnorm function

pairs<- data.frame(t1,t2)
require("MASS")

parcoord(pairs,var.label=TRUE)
#after calling this function we see a parallel coordinates plots

t.test(t2,t1,paired=TRUE)

t.test(t2,t1,paired=TRUE,mu=6,alternative="greater",conf.level=0.99)
#result
# ata:  t2 and t1
# t = 0.99093, df = 49, p-value = 0.1633
# alternative hypothesis: true difference in means is greater than 6
# 99 percent confidence interval:
#   3.695011      Inf
# sample estimates:
#   mean of the differences 
# 7.615381 
#first hipothesis is true, second one is false


#exercise
#1
download.file("http://home.agh.edu.pl/mmd/_media/dydaktyka/as-is/mlb2011.zip","mlb2011.zip")
unzip("mlb2011.zip")
data<-read.csv("mlb2011.csv")

data

# 2 - statistics on a dataset
#field advantage value
fieldadv<- data$HomeWins/data$AllWins
boxplot(fieldadv)
#on the plot we can observe two outliers
#boxplot stats
boxplot.stats(fieldadv)
#mean value
mean(fieldadv)
#median value
median(fieldadv)


# 3
#minimum value - the lowest percentage is 43%
min(fieldadv)
minind<- which(fieldadv ==min(fieldadv))
#maximal value - the higear percentage is 59%
max(fieldadv)
maxind<- which(fieldadv ==max(fieldadv))

#team with the highest and lowest percentage
data$Team[maxind]
data$Team[minind]


