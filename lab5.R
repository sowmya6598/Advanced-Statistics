#excercise9

#part 1- function
s <- function(...) {
  arg <- c(...)
  sum <- 0
  for(i in arg){
    x <- as.numeric(i)
    #part 3
    if(!is.na(x)) sum <- sum + x
  }
  sum
}
#part 2 arguments conversion
s("1","2")
#part 3 
s("1","aaa")
#part 4
s(1:9,"test",1:9) #result is 90

#exercise 10
a <- function(...) {
  c<-0
  arg <- c(...)
  sum <- 0
  for(i in arg){
    x <- as.numeric(i)
    #part 3
    if(!is.na(x)){
      sum <- sum + x
      c <- c+1
    }
  }
  sum/c
}
# part 4
a(1:9,"test",1:9)

#exercise 11
f <- function(x){
  #convesion part 3&4
  arg <- as.integer(x)
  if(!is.na(arg)) {
  if(arg == 0)arg<- 1
  else arg<-arg*f(arg-1)
  }
  #if the value cannot be convertd
  else arg<- NA
  arg
}
#part 4
f(5)
f("lala")
#part 5
f(10)

#exercise 12
max <- function(...){
  arg<-c(...)
  m<- -Inf
  for(i in arg){
    x<-as.numeric(i)
    if(!is.na(x))
    if(x> m) m <- i;
  }
  m
}
# part 4
max("test",1:9,"test")

#exercise 13
min <- function(...){
  arg<-c(...)
  m<- Inf
  for(i in arg){
    x<-as.numeric(i)
    if(!is.na(x))
    if(x< m) m <- i;
  }
  m
}
#part 4
min("test",1:9,"test")

#exercise 14
areeven <- function(...){
  arg<-c(...)
  res<-TRUE
  for(i in arg){
    x<-as.numeric(i)
    if(!is.na(x)){
      if(x %% 2 != 0) res<- FALSE
    }
  }
  res
}
#part 4
areeven(2,4,6,8,"test",10)

#exercise 15
areodd <- function(...){
  arg<-c(...)
  res<-TRUE
  for(i in arg){
    x<-as.numeric(i)
    if(!is.na(x)){
      if(x %% 2 == 0) res<- FALSE
    }
  }
  res
}
#part 4
areodd(1,3,5,7,"test",9)

#exercise 16
fibo <- function(x){
  arg<-as.integer(x)
  if(!is.na(arg)){
  if(arg == 0) res<-0
  if(arg == 1) res <-1
  else res<- fibo(arg-2) + fibo(arg-1)
  }
  else res<- NA
  res
}
#part 3
fiboN <- function(x){
  arg<-as.integer(x)
  for(i in 1:arg){
    cat(fibo(i)," \n")
  }
}
fiboN(5)

#part 4
fibo(10)