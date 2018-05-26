#excercise 1
#create a matrix
m <- matrix(c(1:10000),100,100)
#multiply by 2
m <- apply(m,c(1, 2) ,fun <- function(x){if(x> 5000) x * 2 else x})
#sum
sum(m[5:6,60:62])
#excercise 2
m2 <- matrix(1,50,50)
m2[26:50,] <- 2
m2[,15] <- m2[,15]/2
sum(m2[11:17,13:19])

#excercise 3
m3 <- matrix(c(1:1250),50,25)
m3[m3 %in% c(1,2,3,5,8)] <- NA 
#exclude na values
m3 <- na.exclude(m3)
#mean of all items
mean(m3)

#excercise 4
m4 <- matrix(c(1:1250),25,50)
m4[m4 %in% c(1,2,3,5)] <- NA 
#exclude na values
m4 <- na.exclude(m4)
#mean of all items
sum(m4)

#excercise 5
m51 <- matrix(seq(2,3600,2),60,30)
m52 <-m51[5:7,3:5]
sum(m52%*%m52)
#excercise 6
m61 <- matrix(seq(1,3600,2),60,30)
m62 <-m61[3:5,5:7]
mean(m62%*%m62)

#excercise 7
m71 <- matrix(seq(2,18000,5),60,60)
m72 <- m71[13:15,15:17]
sum(m72%*%m72)

#excercise 8
m81 <- matrix(2^(1:900),30,30)
m82 <- m81[15:17,13:15]
mean(m82 %*% m82)
