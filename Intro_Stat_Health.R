g <- read.csv("cancer data for MOOC 1.csv",header = T,
              sep = ",")
g

##################
COPD <- read.csv("COPD_student.csv",header = T,
                 sep = ",",stringsAsFactors = T)

str(COPD)

library(Hmisc)
describe(COPD)
library(gmodels)
## to tabulate data
CrossTable(COPD$copd)
sum(is.na(COPD$COPDSEVERITY))

library(swirl)
lm1 <- lm(MWT1Best~factor(Diabetes)+factor(AtrialFib)
          +factor(Diabetes*AtrialFib),data = COPD)
summary(lm1)

confint(lm1)

library(prediction)
list("Diabetes"=prediction(lm1,at = list("Diabetes"=c(0,1))))

###
lm <- lm(MWT1Best~factor(Diabetes)+factor(IHD)
          +factor(Diabetes*IHD),data = COPD)
summary(lm)

library(mctest)
# to check collinearity by examing the VIF
imcdiag(lm1,method = "VIF",vif = 5)

##############################

cube <- function(x, n) {
  x^3
}

x <- 1:10
if(x > 5) {
  x <- x+1
}

f <- function(x) {
  g <- function(y) {
    y + z
  }
  z <- 4
  x + g(x)
}

z <- 10
f(3)

x <- 5
y <- if(x < 3) {
  NA
} else {
  10
}

h <- function(x, y = NULL, d = 3L) {
  z <- cbind(x, d)
  if(!is.null(y))
    z <- z + y
  else
    z <- z + f
  g <- x + y / z
  if(d == 3L)
    return(g)
  g <- g + 10
  g
}

#############
 
pollutantmean <- function(directory,pollutant,id=1:332){
  repert <- "D:/Desktop/Coursera_Certificats/datasciencecoursera/" 
  data <- NULL
  for (i in 1:length(id)) {
    if(id[i]<10){
      OP <- read.csv(paste(repert,directory,"/","00",id[i],".csv",sep = ""),header = T) 
    }
    else{
      if(id[i]<100){
        OP <- read.csv(paste(repert,directory,"/","0",id[i],".csv",sep = ""),header = T) 
      }
      else{
        OP <- read.csv(paste(repert,directory,"/",id[i],".csv",sep = ""),header = T)
      }
    }
    
    data <- c(data,OP[,pollutant])
  }
  return(mean(data,na.rm = T))
  
}


pollutantmean("specdata",
              "sulfate", 1:10)


pollutantmean("specdata", "nitrate", 70:72)
## [1] 1.706047
pollutantmean("specdata", "nitrate", 23)


############
complete <- function(directory,id=1:332){
  repert <- "D:/Desktop/Coursera_Certificats/datasciencecoursera/" 
  library(mice)
  data <- NULL
  for (i in 1:length(id)) {
    if(id[i]<10){
      OP <- read.csv(paste(repert,directory,"/","00",id[i],".csv",sep = ""),header = T) 
    }
    else{
      if(id[i]<100){
        OP <- read.csv(paste(repert,directory,"/","0",id[i],".csv",sep = ""),header = T) 
      }
      else{
        OP <- read.csv(paste(repert,directory,"/",id[i],".csv",sep = ""),header = T)
      }
    }
    
    data <- rbind(data,cbind(id[i],nrow(cc(OP))))
  }
  data <- as.data.frame(data)
  names(data) <- c("id","nobs")
  return(data)
  
}

complete("specdata", c(2, 4, 8, 10, 12))

###########

corr <- function(directory,threshold=150){
  got <- complete(directory)
  got.ver <- which((got[,2]>threshold)==T)
  if(is.null(got.ver)==T){
    data <- numeric(0)
  }
  else{
    repert <- "D:/Desktop/Coursera_Certificats/datasciencecoursera/" 
    library(mice)
    data <- NULL
    id <- as.numeric(got[got.ver,1])
    for (i in 1:length(got.ver)) {
      if(id[i]<10){
        OP <- read.csv(paste(repert,directory,"/","00",id[i],".csv",sep = ""),header = T) 
      }
      else{
        if(id[i]<100){
          OP <- read.csv(paste(repert,directory,"/","0",id[i],".csv",sep = ""),header = T) 
        }
        else{
          OP <- read.csv(paste(repert,directory,"/",id[i],".csv",sep = ""),header = T)
        }
      }
      nobs <- cc(OP)
      data <- c(data,cor(nobs[,2],nobs[,3]))  
    }
  }
  
  return(data)
  
}
cr <- corr("specdata", 400)
head(cr)

#########
RNGversion("3.5.1")  
set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])
######08
cr <- corr("specdata")                
cr <- sort(cr)   
RNGversion("3.5.1")
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)

##09
cr <- corr("specdata", 129)                
cr <- sort(cr)                
n <- length(cr)    
RNGversion("3.5.1")
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)

###10

cr <- corr("specdata", 2000)                
n <- length(cr)                
cr <- corr("specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))

###

lm2 <- lm(MWT1Best~FVC,data = COPD)
summary(lm2)

confint(lm2)


lm3 <- lm(MWT1Best~FVC+AGE+FEV1,data = COPD)
summary(lm3)

confint(lm3)

par(mfrow=c(2,2))

plot(lm3)
hist(COPD$AGE)


hist(COPD$MWT1Best)

list("summary"=summary(COPD$AGE),
     "inter-quartile range"=IQR(COPD$AGE))

cor.test(COPD$AGE,COPD$MWT1Best,use="complete.obs",
         method = "pearson")

cor.test(COPD$AGE,COPD$MWT1Best,use="complete.obs",
         method = "spearman")



#####################

hw <- read.csv("hw1_data.csv",header = T,
         sep = ",")

###################################
x <- list(a=1:5,b=rnorm(10))
sapply(X = x,FUN = mean)
y <- matrix(rnorm(200),20,10)
apply(y,2,mean)
rowMeans(y)
colMeans(y)
rowSums(y)
x <- c(rnorm(10),runif(10),rnorm(10,1))
f <- gl(3,10)
tapply(x,f,range)
split(x,f)
library(datasets)
s <- split(airquality,airquality$Month)
sapply(s,function(x)colMeans(x[,c("Ozone","Solar.R","Wind")],
                             na.rm = T))

#########
lm(y~x)
traceback()
debug(lm)
browser()
trace(lm)
recover()

####################################
boring_function <- function(x) {
  x
}

library(datasets)
data(iris)
?iris
vv <- subset(iris,Species=="virginica")
mean(vv$Sepal.Length)
data(mtcars)

sapply(split(mtcars$hp, mtcars$cyl), mean)


apply(iris, 2, mean)


rowMeans(iris[, 1:4])


apply(iris[, 1:4], 2, mean)


apply(iris, 1, mean)


apply(iris[, 1:4], 1, mean)


colMeans(iris)

##################
apply(mtcars, 2, mean)


mean(mtcars$mpg, mtcars$cyl)


split(mtcars, mtcars$cyl)


with(mtcars, tapply(mpg, cyl, mean))

Correct

sapply(split(mtcars$mpg, mtcars$cyl), mean)

Correct

tapply(mtcars$mpg, mtcars$cyl, mean)

Correct

tapply(mtcars$cyl, mtcars$mpg, mean)


sapply(mtcars, cyl, mean)


lapply(mtcars, mean)


