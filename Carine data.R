carinedata <- read.csv(file.choose(),header = T,
         sep = ";",dec = ",",stringsAsFactors = F)


carinedata <- read.csv("database_Carine.csv",header = T,
            sep = ";",dec = ",",stringsAsFactors = F)

carine <- carinedata[,-1]

for (i in 1:ncol(carine)){
  
  carine[,i] <- as.factor(carine[,i])
}

str(carine)

############# Logit #############
logit = glm(FVs_oui_non~.,data = carine,family = binomial())
summary(logit)

exp(cbind(coef(logit), confint.default(logit)))
exp(coef(logit))
confint.default(logit)

library(broom)
oddratio <- tidy(logit, confint.default = T, exponentiate = TRUE)
library(knitr)
kable(oddratio)

library(forestmodel)
forest_model(logit,exponentiate = T)

## Pertinence du mod?le
library(ResourceSelection)
hoslem.test(x = as.numeric(carine$FVs_oui_non)-1,
            fitted(logit,g=10))
hoslem.test(carine$FVs_oui_non,fitted(logit),g=10)

## M?thodes : Pour ?valuer la Qualit? du mod?le de r?gression logistique, on pr?conise :
# Le taux d'erreur,
# La courbe ROC.
pred1.prob <- predict(logit,type = "response")
pred1.mod <-  factor(ifelse(pred1.prob > 0.5, "1", "0"))
pred1.mod

## matrice de confusion
mc1 <- table(carine$FVs_oui_non,pred1.mod)
mc1

## taux d'erreur :qualit? pr?dictive du mod?le est mauvaise lorsque t > 0.5
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