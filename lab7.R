#lab 7 part 1

#exercise 1 - random numbers
set.seed(1)
random_numbers<- runif(10, min = 0, max = 1)

#exercise 2
ifelse(random_numbers > 0.5,"head","tail")


#exercise 3 - unfair coin tosses
set.seed(1)
unfair <- rbinom(10, size=1, prob=0.3)

#exercise 4
set.seed(1)
die_roll<- runif(1,0,6)
die_roll_ceil <- ceiling(die_roll)

#exercise 5 - rnorm
set.seed(1)
heights<- rnorm(100, mean =1.7 , sd =0.1)

#exercise 6 - pnorm
#a) what's the probability that a person will be smaller or equal to 1.90?
set.seed(1)
h1 <- pnorm(1.9,mean=1.7,sd=0.1)
#answer: h1 = 0.97

#b) what's the probability that a person will be taller or equal to 1.6?
set.seed(1)
h2<- pnorm(1.6,mean=1.7,sd=0.1)
#answer = 1-h2 = 1-0.15 = 0.85

#exercise 7
set.seed(1)
patients<- rexp(rate=1/50,n=30)
#a) what's the waiting time average?
avge<- mean(patients)
#answer: average waiting time: 46.76
#b) patients expected to leave
set.seed(1)
pexp(50, rate = 1/50, lower.tail = FALSE)
#answer: [1] 0.3678794
#########################
#part 3

#exercise 1
x <-rnorm(100)

#exercise 2
m <- mean(x)
sdev <- sd(x)
va <- var(x)
med <- median(x)

#exercise 3
dec <- seq(0,1,0.1)
quantile(x,dec)

#exercise 4
groups <- split(x,ceiling(seq_along(x)/20))

#exercise 5

#install.packages("ISwR")
#library(ISwR)
#data("juul")
#View(juul)

m1 <- mean(juul$igf1)

#exercise 6
m2<- mean(juul$igf1,na.rm=TRUE)

#exercise 7
naval <- sum(!is.na(juul$igf1))
# 1018 occurences of a number

#exercise 8
descriptiveStatistics <- function(v){
  #get rid of NA values
  v<- na.omit(v)
  m <- mean(v)
  sdev <- sd(v)
  va <- var(v)
  med <- median(v)
  sprintf("mean %f, standard deviation %f, variance %f, median %f", m,sdev,va,med)

}

descriptiveStatistics(juul$igf1)
