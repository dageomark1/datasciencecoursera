carinedata <- read.csv(file.choose(),header = T,
         sep = ";",dec = ",",stringsAsFactors = F)


carinedata <- read.csv("database_Carine.csv",header = T,
            sep = ";",dec = ",",stringsAsFactors = F)

carine <- carinedata[,-1]

for (i in 1:ncol(carine)){
  
  carine[,i] <- as.factor(carine[,i])
}

str(carine)

##
attach(carine)
# create a cross tabulation of Commune and FVs_oui_non status  
FVs_by_Commune <- table(Commune, FVs_oui_non) 

# output the frequencies of diabetes status by age 
freq_table <- prop.table(FVs_by_Commune, margin = 1) 

# calculate the odds of having diabetes 
odds <- freq_table[, "1"]/freq_table[, "0"] 

############# Logit #############
logit = glm(FVs_oui_non~.,data = carine,family = binomial())
summary(logit)

####
## Exponentiated coefficients and confident interval
cbind(Estimate= coef(logit),
      Oddratio = exp(coef(logit)), 
      confint.default(logit))

cbind(Oddratio = exp(coef(logit)), 
      confint.default(logit))

## c-statistic
require(DescTools) 
Cstat(logit)

## Pertinence du modèle
# run Hosmer-Lemeshow test 
require(ResourceSelection)
hoslem.test(x = logit$y, y = fitted(logit), g = 10)

require(generalhoslem) 
## Loading required package: generalhoslem 
## Warning: package 'generalhoslem' was built under R version 3.5.1 
## Loading required package: reshape 
## Warning: package 'reshape' was built under R version 3.5.1 
## Loading required package: MASS

# run Hosmer-Lemeshow test 
logitgof(obs = logit$y, exp = fitted(logit), g = 10)



library(forestmodel)
forest_model(logit,exponentiate = T)


## M?thodes : Pour ?valuer la Qualit? du mod?le de r?gression logistique, on pr?conise :
# Le taux d'erreur,
# La courbe ROC.
pred1.prob <- predict(logit,type = "response")
pred1.mod <-  factor(ifelse(pred1.prob > 0.5, "1", "0"))
pred1.mod

## matrice de confusion
mc1 <- table(logit$y,pred1.mod)
mc1

## taux d'erreur :qualité prédictive du modèle est mauvaise lorsque t > 0.5
## Ce taux est la proportion des modalit?s pr?dites qui diff?rent des modalit?s observ?es
t1 <- (mc1[1, 2] + mc1[2, 1]) / sum(mc1)
t1


##### Courbe ROC
## la fr?quence de fausse alarme ("1-specificity")
## la fr?quence de bonne d?tection ("sensitivity")
## Plus la courbe longe les axes x = 0 et y = 1, meilleur est le mod?le
## (Autrement dit, plus l'aire sous la courbe ROC est proche de 1, meilleur est le mod?le).
library(Epi)
ROC(form = FVs_oui_non~.,
    plot = "ROC",data = carine)

##### d?tection des valeurs anormales
## si di>1, alors la valeur de l'i?me individu est anormale
cooks.distance(logit)[cooks.distance(logit) > 1]
