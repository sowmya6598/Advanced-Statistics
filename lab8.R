#lab 8 - statistical interference

#part2  -calculating frequencies

groups <- c(
  rep("blue",5010),
  rep("red",4250),
  rep("orange",2630),
  rep("green",4580),
  rep("purple",1460)
)
#frequency table
groups.t1 <- table(groups)
groups.t1

#modify the freq table- display in sorted order

groups.t2 <- sort(groups.t1, decreasing=TRUE)
groups.t2

#proportions of total with two decimal and percentage

prop.table(groups.t2)
round(prop.table(groups.t2),2)
round(prop.table(groups.t2),2)*100

#part 3 - calculating descriptives
require("datasets")
cars

str(cars)
data(cars)

#summary
summary (cars$speed)
summary(cars)

#finenum calculation  
#This function returns Tukey's five number summary (minimum, lower-hinge, median, upper-hinge, maximum)
# for the input data.
fivenum(cars$speed)

#boxplot.stats This function is typically called by another function to gather
# the statistics necessary for producing box plots, but may be invoked separately.
boxplot.stats(cars$speed)

#output:
#   stats	
# a vector of length 5, containing the extreme of the lower whisker, the lower ‘hinge’, the median, the upper ‘hinge’ and the extreme of the upper whisker.
# 
# n	
# the number of non-NA observations in the sample.
# 
# conf	
# the lower and upper extremes of the ‘notch’ (if(do.conf)). See the details.
# 
# out	
# the values of any data points which lie beyond the extremes of the whiskers (if(do.out)).

#psych package

#library docs
help(package="psych")

require("psych")
#the following parameters are being calculated in the output:
#number of vars,  number of values,  mean, standard deviation, median, trimmed mean, median, absoulute deviation,
#min, max, range,  skew, kurtosis, standard error
describe(cars)

#hypothesis tests and confidence interval 
#one sample propotion test with cotinuity correction
prop.test(98,162)

#output:
# data:  98 out of 162, null probability 0.5
# X-squared = 6.7222, df = 1, p-value = 0.009522
# alternative hypothesis: true p is not equal to 0.5
# 95 percent confidence interval:
#   0.5249531 0.6798650
# sample estimates:
#   p 
# 0.6049383 

#confidence interval is [0.52,0.67]

#90% of CI
prop.test(92,162,alt="greater",conf.level = .90)
#now confidence interval is different, [0.51,1.0]

#using a single mean
?quakes

quakes[1:5,]
data(quakes)
mag <- quakes$mag


t.test(mag)

t.test(mag, alternative="greater", mu=4)

#one sample chi-square test
?HairEyeColor
str(HairEyeColor)
HairEyeColor

#marginal frequencies
eyes<- margin.table(HairEyeColor,2)
eyes

#frequencies as proportions with 2 digits
round(prop.table(eyes),2)

chi1 <- chisq.test(eyes)
chi1 #check results

#output:
# data:  eyes
# X-squared = 133.47, df = 3, p-value < 2.2e-16

#compare results
browseURL("https://www.statisticbrain.com/eye-color-distribution-percentages/")

chi2<- chisq.test(eyes, p= c(0.41,0.32,0.15,0.12))
chi2
#output:
# data:  eyes
# X-squared = 6.4717, df = 3, p-value = 0.09079

#13 different X-squared values, much greater p-value in chi2 than in chi1

################exercises################

#1 - open and load dataset
?mtcars #dataset description

mtcars #view dataset

data("mtcars") #read dataset

#2 - descriptive stats

summary(mtcars$mpg)
summary(mtcars$hp)
summary(mtcars$qsec)

# 3 - mean, SD, skewness and kurtosis for mpg, hp , qsec

#mean
mean(mtcars$mpg)
mean(mtcars$hp)
mean(mtcars$qsec)

#standard deviation
sd(mtcars$mpg)
sd(mtcars$hp)
sd(mtcars$qsec)

#skewness - psych package
require(psych)
skew(mtcars$mpg)
skew(mtcars$hp)
skew(mtcars$qsec)

#kurtosis - psych package
kurtosi(mtcars$mpg)
kurtosi(mtcars$hp)
kurtosi(mtcars$qsec)

