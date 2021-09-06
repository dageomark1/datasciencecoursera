######## OBJECTIF 1 #####################
OBJ1 <- read.csv("OBJ1.csv",header = T,
                 sep = ";",dec = ",",stringsAsFactors = F)

str(OBJ1)
OBJ1 <- na.omit(OBJ1)
str(OBJ1)
NUM <- c(1,3,4)
Ob1 <- c(1:7)



for (i in 1:length(NUM)) {
  Ob1 <- Ob1[Ob1!=NUM[i]]
}

## transformation des variables en numeric et factor
for (i in 1:length(OBJ1)) {
  if(length(which(NUM==i))==1){
    OBJ1[,i] <- as.numeric(OBJ1[,i]) 
  }
  else{
    OBJ1[,i] <- as.factor(OBJ1[,i]) 
  } 
}

str(OBJ1)
############# Zip1,ZINB1 ###############
library(pscl)
modpois <- glm(Average.No.of.Aphids~Ants.Presence+
                 Mat.Location+Season,
               data = OBJ1,family = "poisson")

zip1 <- zeroinfl(Average.No.of.Aphids~Month+
                   Mat.Location|
                   Mat.Location+Month,
                 data = OBJ1,dist = "poisson")
summary(zip1)

## Exponentiated coefficients and confident interval
cbind(Exponentiate = exp(coef(zip1)), confint(zip1))
#### Test de VUONG
vuong(zip1,modpois)

### Negative binomial
library(MASS)
negbin <- glm.nb(Average.No.of.Aphids~Ants.Presence+
                   Mat.Location+Season,data = OBJ1)


### Zero inflated negative binomial
zinb1 <- zeroinfl(Average.No.of.Aphids~
                    Mat.Location+Month|
                    Mat.Location+Month,
                  data = OBJ1,dist = "negbin")

summary(zinb1)

## Exponentiated coefficients and confident interval
cbind(Exponentiate = exp(coef(zinb1)), confint(zinb1))

#### Test de VUONG
vuong(zinb1,negbin)
vuong(zip1,zinb1)

############ Logistic regression ###########
logit_obj1 <- glm(Ants.Presence~Average.No.of.Aphids+
                    Mat.Location+Season,data = OBJ1,
                  family = binomial(link = "logit"))
summary(logit_obj1)

## Exponentiated coefficients and confident interval
cbind(Estimate= coef(logit_obj1),Exponentiate = exp(coef(logit_obj1)), 
      confint.default(logit_obj1))



# Pertinence du modèle
# Méthodes : Afin d'étudier la pertinence du modèle de régression logistique, on préconise les méthodes
#suivantes :
# La "règle du pouce",
# Test de Hosmer-Lemeshow,
# Test des résidus de Pearson,
#Test des résidus de la déviance.

library(ResourceSelection)
#si pvaleur > 0.05, 
#on admet que le modèle est bien adapté aux données
hoslem.test(x = as.numeric(OBJ1$Ants.Presence)-1,fitted(logit_obj1,g=10))

######### Test des résidus de Pearson
## Si p-valeur > 0.05, alors on admet que le modèle est bien adapté aux données.
s1 <-  sum(residuals(logit_obj1, type = "pearson")^2)
ddl1 <-  df.residual(logit_obj1)
pvaleur1 = 1 - pchisq(s1, ddl1)
pvaleur1

## Qualité du modèle
## Méthodes : Pour évaluer la qualité du modèle de régression logistique, on préconise :
# Le taux d'erreur ou  La courbe ROC.
pred.prob <- predict(logit_obj1,type = "response")
pred.mod <-  factor(ifelse(pred.prob > 0.5, "1", "0"))
pred.mod

## matrice de confusion
mc <- table(OBJ1$Ants.Presence,pred.mod)
mc

## taux d'erreur :qualité prédictive du modèle est mauvaise lorsque t > 0.5
## Ce taux est la proportion des modalités prédites qui diffèrent des modalités observées
t <- (mc[1, 2] + mc[2, 1]) / sum(mc)
t


##### Courbe ROC
## la fréquence de fausse alarme ("1-specificity")
## la fréquence de bonne détection ("sensitivity")
## Plus la courbe longe les axes x = 0 et y = 1, meilleur est le modèle
## (Autrement dit, plus l'aire sous la courbe ROC est proche de 1, meilleur est le modèle).
library(Epi)
ROC(form = OBJ1$Ants.Presence~Average.No.of.Aphids+
      Mat.Location+Season,
    plot = "ROC",data = OBJ1)


######## OBJECTIF 2 #####################
OBJ2 <- read.csv("OBJ2.csv",header = T,
                 sep = ";",dec = ",",stringsAsFactors = T)

str(OBJ2)
OBJ2 <- na.omit(OBJ2)
str(OBJ2)
NUM <- c(1,3,4,7:9)
Ob2 <- c(1:10)

for (i in 1:length(NUM)) {
  Ob2 <- Ob2[Ob2!=NUM[i]]
}

## transformation des variables en numeric et factor
for (i in 1:length(OBJ2)) {
  if(length(which(NUM==i))==1){
    OBJ2[,i] <- as.numeric(OBJ2[,i]) 
  }
  else{
    OBJ2[,i] <- as.factor(OBJ2[,i]) 
  } 
}

str(OBJ2)
############# Poisson regression ###############

with(OBJ2, tapply(Average.No.of.Aphids, Season, function(x) {
  sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))
}))

OBJ2_cor <- OBJ2[,c(3,7:10)]
str(OBJ2_cor)
library(psych)
corr.test(OBJ2_cor[,-5],use = "complete",method = "pearson")

#Mesures de liaison entre une variable quantitative
#et une variable qualitative
summary(lm(Average.No.of.Aphids~Season,data = OBJ2_cor))$r.squared

fig <- table(OBJ2$Average.No.of.Aphids)
barplot(fig)


############# Zip2,ZINB2 ###############

library(MASS)
library(pscl)
modpois2 <- glm(Average.No.of.Aphids~Ants.Presence+
                  Mat.Location+Season+Average.Temp+
                  Relative.Humidity+Average.Rainfall,
                data = OBJ2,family = "poisson")

zip2 <- zeroinfl(Average.No.of.Aphids~Average.Temp+
                   Relative.Humidity+Average.Rainfall|Average.Temp+
                   Relative.Humidity+Average.Rainfall,
                 data = OBJ2,dist = "poisson")
summary(zip2)

## Exponentiated coefficients and confident interval
cbind(Estimate=coef(zip2),Exponentiate = exp(coef(zip2)),
      confint(zip2))
#### Test de VUONG
vuong(zip2,modpois2)

### Negative binomial
library(MASS)
negbin2 <- glm.nb(Average.No.of.Aphids~Average.Temp+
                    Relative.Humidity+Average.Rainfall,
                  data = OBJ2)


### Zero inflated geometric
zig2 <- zeroinfl(Average.No.of.Aphids~Average.Temp+
                   Relative.Humidity+Average.Rainfall|Average.Temp+
                   Relative.Humidity+Average.Rainfall,
                 data = OBJ2,dist = "geometric")

summary(zig2)

## Exponentiated coefficients and confident interval
cbind(Exponentiate = exp(coef(zig2)), 
      confint(zig2))

#### Test de VUONG
vuong(zig2,negbin2)
vuong(zip2,zig2)


############ Logistic regression ###########
logit_OBJ2 <- glm(Ants.Presence~Average.No.of.Aphids+
                    Mat.Location+Season+
                    Average.Temp+Relative.Humidity+
                    Average.Rainfall,data = OBJ2,
                  family = binomial(link = "logit"))
summary(logit_OBJ2)

## Exponentiated coefficients and confident interval
cbind(Estimate= coef(logit_OBJ2),Exponentiate = exp(coef(logit_OBJ2)), 
      confint.default(logit_OBJ2))

# Pertinence du modèle
# Méthodes : Afin d'étudier la pertinence du modèle de régression logistique, on préconise les méthodes
#suivantes :
# La "règle du pouce",
# Test de Hosmer-Lemeshow,
# Test des résidus de Pearson,
#Test des résidus de la déviance.

library(ResourceSelection)
#si pvaleur > 0.05, 
#on admet que le modèle est bien adapté aux données
hoslem.test(x = as.numeric(OBJ2$Ants.Presence)-1,fitted(logit_OBJ2,g=10))

## Qualité du modèle
## Méthodes : Pour évaluer la qualité du modèle de régression logistique, on préconise :
# Le taux d'erreur ou  La courbe ROC.
pred.prob <- predict(logit_OBJ2,type = "response")
pred.mod <-  factor(ifelse(pred.prob > 0.5, "1", "0"))
pred.mod

## matrice de confusion
mc <- table(OBJ2$Ants.Presence,pred.mod)
mc

## taux d'erreur :qualité prédictive du modèle est mauvaise lorsque t > 0.5
## Ce taux est la proportion des modalités prédites qui diffèrent des modalités observées
t <- (mc[1, 2] + mc[2, 1]) / sum(mc)
t


##### Courbe ROC
## la fréquence de fausse alarme ("1-specificity")
## la fréquence de bonne détection ("sensitivity")
## Plus la courbe longe les axes x = 0 et y = 1, meilleur est le modèle
## (Autrement dit, plus l'aire sous la courbe ROC est proche de 1, meilleur est le modèle).
library(Epi)
ROC(form = OBJ2$Ants.Presence~Average.No.of.Aphids+
      Mat.Location+Season+Average.Temp+
      Relative.Humidity+Average.Rainfall,
    plot = "ROC",data = OBJ2)


######## OBJECTIF 3 #####################
OBJ3 <- read.csv("OBJ3.csv",header = T,
                 sep = ";",dec = ",",stringsAsFactors = T)

str(OBJ3)
OBJ3 <- na.omit(OBJ3)
str(OBJ3)
NUM <- c(2,3)
Ob3 <- c(1:6)

for (i in 1:length(NUM)) {
  Ob3 <- Ob3[Ob3!=NUM[i]]
}

## transformation des variables en numeric et factor
for (i in 1:length(OBJ3)) {
  if(length(which(NUM==i))==1){
    OBJ3[,i] <- as.numeric(OBJ3[,i]) 
  }
  else{
    OBJ3[,i] <- as.factor(OBJ3[,i]) 
  } 
}

str(OBJ3)

############# zip3,ZINB3 ###############
Poisson_OBJ3 <- glm(Average.No.of.Aphids~Ants.Presence+
                      Mat.Location+Month+Season,
                    data = OBJ3,family = "poisson")


library(MASS)
library(pscl)
modpois3 <- glm(Average.No.of.Aphids~Ants.Presence+
                  Mat.Location+Season+Average.Temp+
                  Relative.Humidity+Average.Rainfall,
                data = OBJ3,family = "poisson")

zip3 <- zeroinfl(Average.No.of.Aphids~Ants.Presence|
                Ants.Presence,data = OBJ3,dist = "poisson")
summary(zip3)

## Exponentiated coefficients and confident interval
cbind(Exponentiate = exp(coef(zip3)),
      confint(zip3))
#### Test de VUONG
vuong(zip3,modpois2)
 
### Negative binomial
library(MASS)
negbin3 <- glm.nb(Average.No.of.Aphids~Ants.Presence+
                    Mat.Location+Season+Average.Temp+
                    Relative.Humidity+Average.Rainfall,
                  data = OBJ3)


### Zero inflated geometric
zig3 <- zeroinfl(Average.No.of.Aphids~Ants.Presence|Ants.Presence,
                 data = OBJ3,dist = "negbin")

summary(zig3)

## Exponentiated coefficients and confident interval
cbind(Exponentiate = exp(coef(zig3)), 
      confint(zig3))

#### Test de VUONG
vuong(zig3,negbin3)
vuong(zip3,zig3)


############ Logistic regression ###########
logit_OBJ3 <- glm(Ants.Presence~Mat.Location+Month+Average.No.of.Aphids
                    ,data = OBJ3,
                  family = binomial(link = "logit"))
summary(logit_OBJ3)

## Exponentiated coefficients and confident interval
cbind(Estimate= coef(logit_OBJ3),Exponentiate = exp(coef(logit_OBJ3)), 
      confint.default(logit_OBJ3))

# Pertinence du modèle
# Méthodes : Afin d'étudier la pertinence du modèle de régression logistique, on préconise les méthodes
#suivantes :
# La "règle du pouce",
# Test de Hosmer-Lemeshow,
# Test des résidus de Pearson,
#Test des résidus de la déviance.

library(ResourceSelection)
#si pvaleur > 0.05, 
#on admet que le modèle est bien adapté aux données
hoslem.test(x = as.numeric(OBJ3$Ants.Presence)-1,fitted(logit_OBJ3,g=10))

## Qualité du modèle
## Méthodes : Pour évaluer la qualité du modèle de régression logistique, on préconise :
# Le taux d'erreur ou  La courbe ROC.
pred.prob <- predict(logit_OBJ3,type = "response")
pred.mod <-  factor(ifelse(pred.prob > 0.5, "1", "0"))
pred.mod

### affiche la matrice de confusion avec le nom des modalités

classify <- function(proba)ifelse(proba > 0.5, "Yes", "No")
classified <- classify(predict(logit_OBJ3, OBJ3))
confusion_matrix <- table(OBJ3$Ants.Presence, classified,
      dnn=c("Observed", "predictions"))

## affiche la précision
(accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix))

## specificity measure captures how often 
#the model predicts a negative case correctly

(specificity <- confusion_matrix[1,1]/
    (confusion_matrix[1,1] + confusion_matrix[1,2]))


## Sensitivity It captures how well, when the data has the
# positive class, your model predicts this correctly

(sensitivity <- confusion_matrix[2,2]/
    (confusion_matrix[2,1] + confusion_matrix[2,2]))



## matrice de confusion
mc <- table(OBJ3$Ants.Presence,pred.mod)
mc

## taux d'erreur :qualité prédictive du modèle est mauvaise lorsque t > 0.5
## Ce taux est la proportion des modalités prédites qui diffèrent des modalités observées
t <- (mc[1, 2] + mc[2, 1]) / sum(mc)
t


##### Courbe ROC
## la fréquence de fausse alarme ("1-specificity")
## la fréquence de bonne détection ("sensitivity")
## Plus la courbe longe les axes x = 0 et y = 1, meilleur est le modèle
## (Autrement dit, plus l'aire sous la courbe ROC est proche de 1, meilleur est le modèle).
library(Epi)
ROC(form = OBJ3$Ants.Presence~Average.No.of.Aphids+
      Mat.Location+Month,
    plot = "ROC",data = OBJ3)





######## OBJECTIF 4 #####################
OBJ4 <- read.csv("OBJ4.csv",header = T,
                 sep = ";",dec = ",",stringsAsFactors = T)

str(OBJ4)
OBJ4 <- na.omit(OBJ4)
str(OBJ4)
NUM <- c(1,3,4)
Ob4 <- c(1:9)

for (i in 1:length(NUM)) {
  Ob4 <- Ob4[Ob4!=NUM[i]]
}

## transformation des variables en numeric et factor
for (i in 1:length(OBJ4)) {
  if(length(which(NUM==i))==1){
    OBJ4[,i] <- as.numeric(OBJ4[,i]) 
  }
  else{
    OBJ4[,i] <- as.factor(OBJ4[,i]) 
  } 
}

str(OBJ4)


############# ZIP4,zig4 ###############
library(MASS)
library(pscl)
modpois4 <- glm(Average.No.of.Aphids~Ants.Presence+
                  Mat.Location+Season+Plant.Stage+
                  Aphid.Density.Category,
                data = OBJ4,family = "poisson")

zip4 <- zeroinfl(Average.No.of.Aphids~Plant.Stage|Plant.Stage
                   ,data = OBJ4,dist = "poisson")
summary(zip4)

## Exponentiated coefficients and confident interval
cbind(Exponentiate = exp(coef(zip4)),
      confint(zip4))
#### Test de VUONG
vuong(zip4,modpois4)

### Negative binomial
library(MASS)
negbin4 <- glm.nb(Average.No.of.Aphids~Ants.Presence+
                    Mat.Location+Season+Plant.Stage+
                    Aphid.Density.Category,data = OBJ4)


### Zero inflated negative binomial
zig4 <- zeroinfl(Average.No.of.Aphids~Plant.Stage|Plant.Stage,
                 data = OBJ4,dist = "negbin")

summary(zig4)

## Exponentiated coefficients and confident interval
cbind(Exponentiate = exp(coef(zig4)), 
      confint(zig4))

#### Test de VUONG
vuong(zig4,negbin4)
vuong(zip4,zig4)


############ Logistic regression ###########
logit_OBJ4 <- glm(Ants.Presence~Average.No.of.Aphids+
                    Mat.Location+Season+Plant.Stage+
                    Aphid.Density.Category,data = OBJ4,
                  family = binomial(link = "logit"))
summary(logit_OBJ4)

## Exponentiated coefficients and confident interval
cbind(Estimate= coef(logit_OBJ4),Exponentiate = exp(coef(logit_OBJ4)), 
      confint.default(logit_OBJ4))

# Pertinence du modèle
# Méthodes : Afin d'étudier la pertinence du modèle de régression logistique, on préconise les méthodes
#suivantes :
# La "règle du pouce",
# Test de Hosmer-Lemeshow,
# Test des résidus de Pearson,
#Test des résidus de la déviance.

library(ResourceSelection)
#si pvaleur > 0.05, 
#on admet que le modèle est bien adapté aux données
hoslem.test(x = as.numeric(OBJ4$Ants.Presence)-1,fitted(logit_OBJ4,g=10))

## Qualité du modèle
## Méthodes : Pour évaluer la qualité du modèle de régression logistique, on préconise :
# Le taux d'erreur ou  La courbe ROC.
pred.prob <- predict(logit_OBJ4,type = "response")
pred.mod <-  factor(ifelse(pred.prob > 0.5, "1", "0"))
pred.mod

## matrice de confusion
mc <- table(OBJ4$Ants.Presence,pred.mod)
mc

## taux d'erreur :qualité prédictive du modèle est mauvaise lorsque t > 0.5
## Ce taux est la proportion des modalités prédites qui diffèrent des modalités observées
t <- (mc[1, 2] + mc[2, 1]) / sum(mc)
t


##### Courbe ROC
## la fréquence de fausse alarme ("1-specificity")
## la fréquence de bonne détection ("sensitivity")
## Plus la courbe longe les axes x = 0 et y = 1, meilleur est le modèle
## (Autrement dit, plus l'aire sous la courbe ROC est proche de 1, meilleur est le modèle).
library(Epi)
ROC(form = OBJ4$Ants.Presence~Average.No.of.Aphids+
      Mat.Location+Season+Plant.Stage+
      Aphid.Density.Category,
    plot = "ROC",data = OBJ4)

######## OBJECTIF 5 #####################
BBTV_Rwanda <- read.csv(file = file.choose(),header = T,
                        sep = ";",dec = ",",stringsAsFactors = F)

str(BBTV_Rwanda)
BBTV_Rwanda <- na.omit(BBTV_Rwanda)
str(BBTV_Rwanda)
NUM <- c(1,3,4)
Ob5 <- c(1:10)

for (i in 1:length(NUM)) {
  Ob5 <- Ob5[Ob5!=NUM[i]]
}

## transformation des variables en numeric et factor
for (i in 1:length(BBTV_Rwanda)) {
  if(length(which(NUM==i))==1){
    BBTV_Rwanda[,i] <- as.numeric(BBTV_Rwanda[,i]) 
  }
  else{
    BBTV_Rwanda[,i] <- as.factor(BBTV_Rwanda[,i]) 
  } 
}

str(BBTV_Rwanda)

############# zip5,zig5 ###############
library(MASS)
library(pscl)
modpois5 <- glm(Number.of.Aphids~Ants.Presence+
                  Mattlocation+Season+Sucker+
                  Medium+Mother,
                data = BBTV_Rwanda,family = "poisson")

zip5 <- zeroinfl(Number.of.Aphids~Ants.Presence+
                   Mattlocation+Season+Sucker+
                   Medium+Mother|Ants.Presence+
                   Mattlocation+Season+Sucker+
                   Medium+Mother,
                 data = BBTV_Rwanda,dist = "poisson")
summary(zip5)

## Exponentiated coefficients and confident interval
cbind(Estimate=coef(zip5),Exponentiate = exp(coef(zip5)),
      confint(zip5))
#### Test de VUONG
vuong(zip5,modpois5)

### Negative binomial
library(MASS)
negbin5 <- glm.nb(Number.of.Aphids~Ants.Presence+
                    Mattlocation+Season+Sucker+
                    Medium+Mother,data = BBTV_Rwanda)


### Zero inflated negative binomial
zig5 <- zeroinfl(Number.of.Aphids~Ants.Presence+
                   Mattlocation+Season+Sucker+
                   Medium+Mother|Ants.Presence+
                   Mattlocation+Season+Sucker+
                   Medium+Mother,
                 data = BBTV_Rwanda,dist = "geometric")

summary(zig5)

## Exponentiated coefficients and confident interval
cbind(Estimate=coef(zig5),Exponentiate = exp(coef(zig5)), 
      confint(zig5))

#### Test de VUONG
vuong(zig5,negbin5)
vuong(zip5,zig5)

############# Logistic Regression ###############
logit_BBTV_Rwanda <- glm(Ants.Presence~Number.of.Aphids+
                           Mattlocation+Season+Sucker+
                           Medium+Mother,data = BBTV_Rwanda,
                         family = binomial(link = "logit"))
summary(logit_BBTV_Rwanda)

## Exponentiated coefficients and confident interval
cbind(Estimate= coef(logit_BBTV_Rwanda),Exponentiate = exp(coef(logit_BBTV_Rwanda)), 
      confint.default(logit_BBTV_Rwanda))

# Pertinence du modèle
# Méthodes : Afin d'étudier la pertinence du modèle de régression logistique, on préconise les méthodes
#suivantes :
# La "règle du pouce",
# Test de Hosmer-Lemeshow,
# Test des résidus de Pearson,
#Test des résidus de la déviance.

library(ResourceSelection)
#si pvaleur > 0.05, 
#on admet que le modèle est bien adapté aux données
hoslem.test(x = as.numeric(BBTV_Rwanda$Ants.Presence)-1,fitted(logit_BBTV_Rwanda,g=10))

## Qualité du modèle
## Méthodes : Pour évaluer la qualité du modèle de régression logistique, on préconise :
# Le taux d'erreur ou  La courbe ROC.
pred.prob <- predict(logit_BBTV_Rwanda,type = "response")
pred.mod <-  factor(ifelse(pred.prob > 0.5, "1", "0"))
pred.mod

## matrice de confusion
mc <- table(BBTV_Rwanda$Ants.Presence,pred.mod)
mc

## taux d'erreur :qualité prédictive du modèle est mauvaise lorsque t > 0.5
## Ce taux est la proportion des modalités prédites qui diffèrent des modalités observées
t <- (mc[1, 2] + mc[2, 1]) / sum(mc)
t


##### Courbe ROC
## la fréquence de fausse alarme ("1-specificity")
## la fréquence de bonne détection ("sensitivity")
## Plus la courbe longe les axes x = 0 et y = 1, meilleur est le modèle
## (Autrement dit, plus l'aire sous la courbe ROC est proche de 1, meilleur est le modèle).
library(Epi)
ROC(form = BBTV_Rwanda$Ants.Presence~Number.of.Aphids+
      Mattlocation+Season+Sucker+Medium+Mother,
    plot = "ROC",data = BBTV_Rwanda)


################## SARAH DATA ########################

Sarah <- read.csv(file = file.choose(),header = T,sep = ";"
                  ,dec = ",",stringsAsFactors = T)


str(Sarah)

######################
library(ggplot2)

ggplot(data = Sarah,aes(x = type.of.leaf,y = protein))+ 
  geom_bar(stat="identity",width = .2,
           position="dodge")+
  theme(axis.text.x = element_text(size=8, angle=0,face = "bold"),
        axis.text.y =element_text(size=8, angle=0,face = "bold"))+
  labs(x = "Type of leaf",y = "Protein")+
  theme_classic()+
  scale_x_discrete(labels=c("Primary","Secondary"))


plot(Sarah$type.of.leaf,Sarah$protein)


################# 
sarah_anova <- aov(protein~type.of.leaf+infection.status+
                     type.of.leaf*infection.status,
                   data = Sarah)

summary(sarah_anova)

TukeyHSD(sarah_anova)

sarah_nested_anova <- aov(protein~type.of.leaf+infection.status+
                            type.of.leaf/infection.status,
                          data = Sarah)


summary(sarah_nested_anova)
TukeyHSD(sarah_nested_anova)

