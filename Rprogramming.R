makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  # the function set : sets the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  
  
  
  # the function get : gets the value of the matrix
  get <- function(){ x} # get value of x
  
  # the function setSolve : sets the value of the inverse matrix
  setSolve <- function(solve) m <<- solve # caching the inverse matrix of x
  
  # the function getSolve : gets the cached value of the inverse matrix
  getSolve <- function() m # getting the cached inverse matrix of x
  
  # returning the results 
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}

# function cacheSolve : returns the cached inverse of a matrix
cacheSolve <- function(x, ...) {
  m <- x$getSolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setSolve(m)
  m
}

# example 
x <- makeCacheMatrix(matrix(1:4,2))
x
cacheSolve(x)

###########################################

set.seed(20)
X <- rnorm(100)
E <- rnorm(100,0,2)
Y <- 0.5+2*X+E
summary(Y)
plot(X,Y)


set.seed(1)
rpois(5, 2)

library(datasets)
Rprof()
fit <- lm(y ~ x1 + x2)
Rprof(NULL)

#########################
health <- read.csv("hospital-data.csv",header = T,
                   sep = ",")


out <- read.csv("outcome-of-care-measures.csv",header = T,
         sep = ",")

########
best<- function(state, outcome)
{
  outcome1 <- read.csv("outcome-of-care-measures.csv",
                       colClasses = "character")
  if(!any(state == outcome1$State)){
    stop("invalid state")}
  else if((outcome %in% c("heart attack", "heart failure",
                          "pneumonia")) == FALSE) {
    stop(print("invalid outcome"))
  }
  outcome2 <- subset(outcome1, State == state)
  if (outcome == "heart attack") {
    colnum <- 11
  }
  else if (outcome == "heart failure") {
    colnum <- 17
  }
  else {
    colnum <- 23
  }
  min_row <- which(as.numeric(outcome2[ ,colnum]) == 
                     min(as.numeric(outcome2[ ,colnum]), na.rm = TRUE))
  hospitals <- outcome2[min_row,2]
  hospitals <- sort(hospitals)
  return(hospitals[1])
}

####
rankhospital<- function(state, outcome, num = "best")
{
  outcome1 <- read.csv("outcome-of-care-measures.csv",
                       colClasses = "character")
  if(!any(state == outcome1$State)){
    stop("invalid state")}
  else if((outcome %in% c("heart attack", "heart failure",
                          "pneumonia")) == FALSE) {
    stop(print("invalid outcome"))
  }
  outcome2 <- subset(outcome1, State == state)
  if (outcome == "heart attack") {
    colnum <- 11
  }
  else if (outcome == "heart failure") {
    colnum <- 17
  }
  else {
    colnum <- 23
  }
  outcome2[ ,colnum] <- as.numeric(outcome2[ ,colnum])
  outcome3 <- outcome2[order(outcome2[ ,colnum],outcome2[,2]), ]
  outcome3 <- outcome3[(!is.na(outcome3[ ,colnum])),]
  if(num == "best"){
    num <- 1
  }            
  else if (num == "worst"){
    num <- nrow(outcome3)
  }      
  return(outcome3[num,2])
}

########
rankall<- function(outcome, num = "best")
{
  library(dplyr)
  library(magrittr)
  outcome2 <- read.csv("outcome-of-care-measures.csv",
                       colClasses = "character")
  if((outcome %in% c("heart attack", "heart failure",
                     "pneumonia")) == FALSE) {
    stop(print("invalid outcome"))
  }
  if (outcome == "heart attack") {
    colnum <- 11
  }
  else if (outcome == "heart failure") {
    colnum <- 17
  }
  else {
    colnum <- 23
  }
  outcome2[ ,colnum] <- as.numeric(outcome2[ ,colnum])
  
  outcome2 = outcome2[!is.na(outcome2[,colnum]),]
  
  splited = split(outcome2, outcome2$State)
  ans = lapply(splited, function(x, num) {
    x = x[order(x[,colnum], x$Hospital.Name),]
    
    if(class(num) == "character") {
      if(num == "best") {
        return (x$Hospital.Name[1])
      }
      else if(num == "worst") {
        return (x$Hospital.Name[nrow(x)])
      }
    }
    else {
      return (x$Hospital.Name[num])
    }
  }, num)
  
  #Return data.frame with format
  return ( data.frame(hospital=unlist(ans), state=names(ans)) )
}

r <- rankall("heart attack", 4)
as.character(subset(r, state == "HI")$hospital)


##
r <- rankall("pneumonia", "worst")
as.character(subset(r, state == "NJ")$hospital)

#####
r <- rankall("heart failure", 10)
as.character(subset(r, state == "NV")$hospital)
