Malika <- read.csv("malikath.csv",header = T,
                   sep = ";",dec = ",",stringsAsFactors = F)


str(Malika)

for (i in 1: ncol(Malika)){
  if(class(Malika[,i])=='character'){
    Malika[,i] <- as.factor(Malika[,i])
  }else{
    Malika[,i] <- as.numeric(Malika[,i])
  }
} 

Malika$Foodtrader <- as.factor(Malika$Foodtrader)

############# Communes #############

attach(Malika)

library(bestNormalize)
R <- bestNormalize(Price.100g,standardize = T,
                   allow_orderNorm = T,allow_lambert_s = T,
                   allow_lambert_h = T,allow_exp = T,out_of_sample = F)

RR <- predict(R,newdata=R$x.t,inverse=T)

P <- R$x.t
Malika <- cbind(Malika,P)

all.equal(Price.100g,RR)

plot(density(pab$Price.100g^2))

pab <- subset(Malika,Periods=='Abundance')


curve(dnorm(x,mean(Price.100g^1/2),sd(Price.100g^1/2)),
      add = T,col='blue',lwd=2)

M1 <- aov(powerTransform(Price.100g,family = 'bcPower')~Species+Communes+Periods,
          data = Malika)

summary(powerTransform(M1,data=Malika))
summary(car::powerTransform(M1))

M1 <- aov(Price.100g^2~Species+Communes,data = pab)
## test de normalité des résidus
shapiro.test(M1$residuals)


## test d'homogénéité des variances
bartlett.test(M1$residuals~Periods,data = Malika)
bartlett.test(M1$residuals~Species,data = pab)
bartlett.test(M1$residuals~Communes,data = pab)


library(MASS)
plot(density(Price.100g))
boxcox(aov(Price.100g~Species+Communes+Periods),lambda=seq(0,1,by=.1))

lambda <- tm$x[which.max(tm$y)]

Price.new <- (Price.100g^lambda - 1) / lambda
plot(density(Price.new))
Malika <- cbind(Malika,Price.new)

str(Malika)

plot(density(Price.new))

ks.test(Price.new,"pnorm")
shapiro.test(Price.new)

library(caret)
price.N <- preProcess(as.data.frame(Price.100g),method = 'YeoJohnson')
Malika <- cbind(Malika,Price.new)

library(car)
M2 <- aov(Price.new~Species+Communes+Periods,data = Malika)

shapiro.test(M2$residuals)



powerTransform(Price.100g~Species+Communes+Periods,
               family = 'yjPower')

plot(density(Price.100g))

library(VGAM)


lambda <- seq(-3:3)

Malika$Price.100g <- yeo.johnson(Malika$Price.100g,
                                 lambda=seq(0,2,by=.1),
                                 derivative = 0,inverse = F)

M2 <- aov(Price.100g~Species+Communes+Periods,data = Malika)

plot(M2,2)

## test de normalité des résidus
shapiro.test(M2$residuals)


## test d'homogénéité des variances
bartlett.test(M2$residuals~Periods,data = Malika)
bartlett.test(M2$residuals~Species,data = Malika)
bartlett.test(M2$residuals~Communes,data = Malika)


curve(dnorm(x,mean(Price.new),sd(Price.new)),
      add = T,col='blue',lwd=2)



## test de normalité des résidus
shapiro.test(mod$residuals)


## test d'homogénéité des variances
bartlett.test(mod1$residuals~Variétés,data = taill)


summary(mod1)
summary.lm(mod1)
AIC(mod)
AIC(mod1)
BIC(mod)
BIC(mod1)


## test de Tukey
TukeyHSD(mod1,"Variétés",ordered = T)
TukeyHSD(mod1,"Eau",ordered = T)
TukeyHSD(mod1,"Substrats",ordered = T)
TukeyHSD(mod1,"Statut",ordered = T)
TukeyHSD(mod1,"Variétés:Statut",ordered = T)
TukeyHSD(mod1,"Variétés:Eau",ordered = T)
TukeyHSD(mod1,"Substrats:Statut",ordered = T)
TukeyHSD(mod1,"Substrats:Eau",ordered = T)
###

library(granova)
granova.1w(Hauteur,Variétés)

### moyenne par echantillon ou groupe
moy<-tapply(Hauteur,Variétés,mean)
moy
moy.g<-mean(Hauteur)
moy.g
ecart<-tapply(Hauteur,Variétés,sd)
ecart
ecart.g<-sd(Hauteur)
ecart.g

summary(Hauteur)

table(Hauteur,Variétés)

plot(Variétés,Hauteur,col="lightblue",
     ylab="Hauteur des plants",xlab="Variétés")
points(1:3,moy,pch="@")
abline(h=moy.g,col="red")