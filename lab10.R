#lab 10 - bootstrapping

#1 
file10 = download.file("https://home.agh.edu.pl/~mmd/_media/dydaktyka/as-is/lab10dataset.csv.zip","lab10dataset.csv.zip")

unzip("lab10dataset.csv.zip")

data = read.csv("lab10dataset.csv",sep = ",")
#histogram
hist(data$V1)
#ecdf
Fn <- ecdf(data$V1)
plot(Fn) 
#ecdf of density
xlim = c(-4,4)
plot(density(data$V1),xlim=xlim,main="")
par(new=TRUE)
plot(Fn,xlim=xlim,main="ECDF and density")

#2
boot.mean <- function (dataset,iter){
  result<- c()
  for(i in 1:iter){
    result[i]<- mean(sample(dataset,replace=TRUE))
  }
  return(result)
}

#3 
t.test(boot.mean(data$V1,length(data)))

#4 
meansd<- boot.mean(data$V1,10000)
#histogram
hist(meansd)
#sample mean of data
abline(v=mean(data$V1),col="blue")

#5
quantile(meansd,c(.025,.975))
mean(meansd)
mean(data$V1)
#the difference between means:
#> mean(meansd)
#[1] 0.4718095
#> mean(data$V1)
#[1] 0.4725156

#6
mymedian<- function(data,i){
  return(median(data[i]))
}

results <- boot(data$V1,mymedian,10000)

#7
tt<- t.test(results$t)
str(results)
hist(results$t)
abline(v=results$t0,col="blue")
abline(v = tt$conf.int[1],col="green")
abline(v = tt$conf.int[2],col="green")

#8
library("e1071")  
statistics <- function(dataset){
  a <- boot(dataset,var,10000)
  b <- boot(dataset,kurtosis,10000)
  c <- boot(dataset,max,10000)
  d <- boot(dataset,min,10000)
  
  return(c(a,b,c,d))
}
statistics(data$V1)



