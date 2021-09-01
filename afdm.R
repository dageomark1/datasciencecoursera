afdm <- read.csv("afdm.new.csv",header = T,
         sep = ";",dec = ",",stringsAsFactors = F,
         na.strings = "?")

dim(afdm)
names(afdm)
## remplacer une valeur par une autre 
table(afdm[,"Q7"])
which(afdm[,"Q7"]==17.33333333)
afdm[afdm[,"Q7"]==17.33333333,"Q7"] <- 17
afdm$Q7

quanti <- c(1:8)
w <- c(1:25)

for (i in 1:length(quanti)) {
  w <- w[w!=quanti[i]]
}


#### dichotomisation
afdm$Q15 <- ifelse(afdm$Q15>1,1,0)
afdm$Q15 <- ifelse(afdm$Q15<=1,"not receive other seed",
                   ifelse(afdm$Q15 > 1 & afdm$Q15<=6,"receive other seed",NA))

## transformation des variables en numeric et factor
for (i in 1:length(afdm)) {
  if(length(which(quanti==i))==1){
    afdm[,i] <- as.numeric(afdm[,i]) 
  }
  else{
    afdm[,i] <- as.factor(afdm[,i]) 
  } 
}

afdm <- afdm[,c(quanti,w)]
dim(afdm)
str(afdm)

## nombre de valeur manquantes par colonne
as.matrix(apply(afdm, MARGIN = 2, function(x){sum(is.na(x))}))
c(Q2,Q3,Q4,Q5,Q6,Q11,Q17,Q19,Q20)
c(Q2,Q3,Q4,Q5,Q6,Q11,Q18,Q20,Q21) ## avec rogueing

afdm <- afdm[,-9]

### imputation des valeurs manquantes
## utilisant "rpart" et "pls_rege"
f.rpart.pls_rege <- function(data){
  res <- data
  library(dlookr)
  library(randomForest)
  for (i in 1:ncol(res)) {
    if(length(which(is.na(res[, i]) == T)) != 0){
      if(is.factor(res[,i]) == T){
        res[,i] <- as.factor(as.character(imputate_na(.data = res, xvar = i,method = "mice")))
      }
      else{
        res[,i] <- as.numeric(imputate_na(.data = res, xvar = i,method = "median"))
      }
    }
  }
  
  return(res)
}

afdm.impute <- f.rpart.pls_rege(afdm)
str(afdm.impute)

#### tabulations
table(afdm.impute$Q9)

########## stats descriptives a travers les graphiques "qqplot" 
# et "scatterplot" pour l'analyse uni-bidimensionnelle 

fun.qqnorm <- function(data){
  dispo.var <- c(3, 4)
  for(i in 1:12){
    if(i == 1){
      win.graph(width = 20, height = 15)
      par(mfrow = dispo.var)
    }
    else{
      if(i == prod(dispo.var) + 1){
        win.graph(width = 20, height = 15)
        par(mfrow = dispo.var)
      }
    }
    
    x <- data[, i]
    
    qqnorm(x,xlab = "theoretical quantity",ylab = "Empirical quantity",main = names(data)[i])
    qqline(x,col=2)
    
    i <- i+1
    
    if(i==13){
      savePlot(filename = "qqplot_1_11",type = "jpeg")
      dev.off()
    }
    else{
      if(i==13){
        savePlot(filename = "qqplot_12_22",type = "jpeg")
        dev.off()
      }
    }
  }
}

fun.qqnorm(afdm.impute)

## fonction de stats dispersion 
### fonction pr plot variables quanti
fun.plot <- function(data){
  dispo.var <- c(3, 4)
  pos.i <- 1:12
  pos.i <- pos.i[pos.i != 11]
  for(i in 1:length(pos.i)){
    if(i == 1){
      win.graph(width = 20, height = 15)
      par(mfrow = dispo.var)
    }
    else{
      if(i == prod(dispo.var) + 1){
        win.graph(width = 20, height = 15)
        par(mfrow = dispo.var)
      }
    }
    
    plot(x=data[,pos.i[i]],y=data[,11],xlab = names(data)[pos.i[i]],ylab = names(data)[11],main = "")
    
    i <- i+1
    
    if(i==length(pos.i)+1){
      savePlot(filename = "plot_1_11",type = "jpeg")
      dev.off()
    }
  }
}

##
fun.plot(afdm.impute)

####  function plot quali
fun.plot.quali <- function(data){
  dispo.var <- c(3, 4)
  for(i in 1:11){
    if(i == 1){
      win.graph(width = 20, height = 15)
      par(mfrow = dispo.var)
    }
    else{
      if(i == prod(dispo.var) + 1){
        win.graph(width = 20, height = 15)
        par(mfrow = dispo.var)
      }
    }
    
    plot(x=data[,12+i],y=data[,11],xlab = names(data)[12+i],ylab = names(data)[11],main = "",
         ylim = range(data[,11]), las = 1,
         boxwex = 0.25, outwex = 0.5,
         staplewex = 0.75, staplelty = 1,
         outpch = 1, outcol = "magenta",
         col = rainbow(7),
         names = levels(data[,12+i]))
    
        i <- i+1
    
    if(i==12){
      savePlot(filename = "plot_13_23",type = "jpeg")
      dev.off()
    }
  }
}

##
#afdm.impute$Q31 <- as.factor(ifelse(afdm.impute$Q31 == "F","1","2"))
fun.plot.quali(afdm.impute)

## matrice de corrélation
m.cor <- cor(afdm.impute[,1:12])
library(corrplot)
corrplot(m.cor, type="lower", order="alphabet",method = "number",
         tl.col="black", tl.srt=45,cl.ratio = 0.1,
         number.cex =0.75,col = c("black","red","blue"))

library(xlsx)
write.xlsx(x = cor,file = "cor.afdm.xlsx")

library(pls)
pls_reg<- plsr(Q11~.,data=afdm.impute,ncomp=5)

pls_reg$coefficients


scoreplot(pls_reg, comps = 1:5)
plot(scores(pls_reg), comps = 1:5)
plot(pls_reg, plottype = "scores", comps = 1:5)

loadingplot(pls_reg, comps = 1:5)
loadingplot(pls_reg, comps = 1:5,legendpos ="topright") # With legend
loadingplot(pls_reg, comps = 1:5, scatter = TRUE) # Plot as scatterplots


corrplot(pls_reg, comps = 1:2)
corrplot(pls_reg, comps = 1:3)

# Fonction pour effectuer une regression PLS

reg.pls.fun <- function(data,formula,validation="CV",title){
  library(pls)
  
  mod.pls <- plsr(formula = formula,
                  data = data, 
                  validation = validation)
  
  x11(width = 15, height = 8.6, pointsize = 16)
  par(mfrow = c(2, 1))
  plot(RMSEP(mod.pls), main = title)
  
  # Identification du nombre de dimensions ayant la plus faible erreur de validation croisee
  RMSE <- RMSEP(mod.pls)
  nbre.dim <- which.min(RMSE$val[estimate = "adjCV", , ]) - 1
  
  if(nbre.dim > 0){
    # Mise en oeuvre de la regression PLS avec le nombre precedemment determine
    mod.pls <- plsr(formula = formula,
                    data = data, 
                    ncomp = nbre.dim)
    
    plot(RMSEP(mod.pls), main = title)
  }
  
  savePlot(filename = paste("res_reg_pls", "RMSE", sep = "_"), 
           type = "png", device = dev.cur())
  
  
  print(coef(mod.pls))
  coefficients <- coef(mod.pls)
  sum.coef <- sum(sapply(coefficients, abs))
  coefficients <- coefficients*100/sum.coef
  coefficients <- sort(coefficients[, 1, 1])
  
  x11(width = 15, height = 8.6, pointsize = 16)
  layout(matrix(c(1,1, 2,3), 2, 2, byrow = TRUE))
  barplot(coefficients, main = title)
  barplot(head(coefficients, 5))
  barplot(tail(coefficients, 5))
  
  savePlot(filename = paste("res_reg_pls", "diagramme", sep = "_"), 
           type = "png", device = dev.cur())
}

reg.pls.fun(data = afdm.impute,formula = Q11~.,title = "reg.pls_Q1")
### Stat Multidimensionnelles
library(FactoMineR)
library(factoextra)
library(FactoInvestigate)

mixte <- FAMD(afdm.impute,ncp = 10,graph = F)

## crit?re de KAISER-GUTTMAN 1+2*sqrt(p-1/n-1)
1+2*sqrt(22/122)
library(xtable)
mixte$eig
xtable(mixte$eig)

fviz_eig(mixte,choice = "eigenvalue",geom = c("bar","line")
         ,addlabels = T,barfill = "black", 
         barcolor = "black", linecolor = "black",
         ggtheme = theme_classic(base_size = 20,
         base_line_size =2),ylim = c(0, 4))

## contribution all variables
fviz_contrib(mixte,choice = "var",axes = c(1,2)
             ,addlabels = T,fill = "black", 
             color = "black", linecolor = "black",
             ggtheme = theme_classic(base_size = 15,
             base_line_size =2))

contr.afdm <- round(mixte$var$contrib[,1:2],3)
xtable(contr.afdm)

### connaitre le nombre de pls_regalit? totale,ici 30
length(mixte$var$contrib[,1])
#### contribution moyenne d'un facteur aux pls_regalit?s 
## est = 100/nbre total de pls_regalit?s, ici 1,96
100/21

################# Quantitatives ################
fviz_famd_var(X = mixte,choice = "quanti.var",repel = T,
              col.var.sup = "black",col.var = "contrib",
              gradient.cols=rainbow(7),
              ggtheme=theme_minimal())

## qualit? de repr?sentation
fviz_famd_var(mixte,col.var = "cos2",
              repel = T,gradient.cols=rainbow(7),
              ggtheme = theme_classic(base_size = 15,base_line_size =2,base_rect_size = 2),choice = "quanti.var")
              


fviz_famd_var(X = mixte,choice = "var",repel = T,
              col.var.sup = "black",
              gradient.cols=rainbow(7),
              ggtheme=theme_minimal())

subset(mixte$var$contrib,subset = mixte$var$contrib[,i]>=2,
       select = mixte$var$contrib[,1:5])

##################### Qualitatives ###################
fviz_famd_var(mixte,col.var = "cos2",col.quali.sup = "black",
              repel = T,gradient.cols=rainbow(7),
              ggtheme=theme_minimal(),choice = "quali.var")

fviz_famd_var(X = mixte,choice = "quali.var",repel = T,
              col.var.sup = "black",col.var = "red",
              gradient.cols=rainbow(7),
              ggtheme=theme_minimal())

############# Individus ##############
fviz_famd_ind(X = mixte,axes = c(1,2),repel = T,addEllipses = F,
              gradient.cols=rainbow(7),col.ind = "cos2")

fviz_ellipses(X = mixte,habillage = c("Q1","Q2","Q4","Q7","Q8","Q9"),
              addEllipses = T)

fviz_mfa_ind(X = mixte,repel = T,addEllipses = F,habillage = c("Q1","Q2","Q4","Q7","Q8","Q9"))

#
HCPC(mixte,nb.clust = -1,consol = T,description = T)

Investigate(HCPC(mixte))

### 

### transformation boxcox
f.boxcox <- function(y){
  library(TSA)
  if(all(y > 0)==T){
    res <- BoxCox.ar(y = y)
    lambda <- res$mle
    print("transformation de y\n")
    if(lambda == 0){
      y.trans <- log(y)
    }
    else{
      y.trans <- (y^lambda - 1)/lambda
    }
  }
  else{
    y <- y+1
    res <- BoxCox.ar(y = y)
    lambda <- res$mle
    print("transformation de y+1\n")
    if(lambda == 0){
      y.trans <- log(y)
    }
    else{
      y.trans <- (y^lambda - 1)/lambda
    }
  }
  
  res.def <- new.env()
  res.def <-as.list(res.def)
  res.def$lambda <- lambda
  res.def$y.trans <- y.trans
  
  return(res.def)
}



b.trans <- afdm.impute
pos.i <- 1:11
pos.i <- pos.i[pos.i != 11]
for (i in 1:10) {
  b.trans[,pos.i[i]] <- fboxcox(y = b.trans[,pos.i[i]])
}
str(b.trans)

plot(b.trans$Q8,b.trans$Q11)

res <- fboxcox(y = cool.data.impute[,7])


res <- BoxCox.ar(y = cool.data.impute[,7])

str(res)



##  pls_reg?le GAM
library(mgcv)
length(names(table(afdm.impute$Q6))) ## permet de voir le nbre de pls_regalit? = ddl

### choix de s() parce que le R?(adj) est plus grand et plus de variables
##  sont significatives que dans le cas des te, ti et t2, 
## d'o? pour annexe : reprendre GAM avec te, ti et t2
## Q2, Q3, q6, Q7, Q10, q12

####### r?gression binomiale
#afdm.impute[afdm.impute[,"Q11"]>0,"Q11"] <- 1
glm_pois <- glm(Q11~Q1+Q2+Q3+Q4+Q5+Q6+Q7+Q8+Q9+Q10+
              Q12+Q13+Q14+Q15+Q16+Q17+Q18+Q19
            +Q20+Q21+Q22+Q23,data = afdm.impute,family = poisson("log"))


summary(glm_pois)

library(ResourceSelection)
hoslem.test(x = afdm.impute$Q11,fitted(afdm_pois,g=10))
##Test de Cameron et Trivedi:Si p-valeur < 0,05, 
##on affirme la dispersion anormale
library(AER)
dispersiontest(afdm_pois)


length(names(table(afdm.impute$Q10)))

afdm.gam <- gam(Q11~s(Q1,k=12,fx=T,bs="tp")
                +s(Q2,k=18,fx=T,bs="tp")
                +s(Q3,k=12,fx=T,bs="tp")
                +s(Q4,k=10,fx=T,bs="tp")
                +s(Q5,k=10,fx=T,bs="tp")
                +s(Q6,k=8,fx=T,bs="tp")
                +s(Q7,k=6,fx=T,bs="tp")
                +s(Q8,k=6,fx=T,bs="tp")
                +s(Q9,k=8,fx=T,bs="tp")
                +s(Q10,k=4,fx=T,bs="tp")
                
                +Q13+Q14+Q15+Q16+Q17+Q18+Q19
                +Q20+Q21+Q22+Q23,data = afdm.impute, method = "REML")

gam.check(afdm.gam)
summary(afdm.gam)
par(mfrow=c(2,2))
plot.gam(afdm.gam,pages = 1)
concurvity(afdm.gam,full = F)
#
length(names(table(afdm.impute$Q5)))


## BON
library(mgcv)
afdm.gam1 <- gam(Q11~s(Q1,k=12,bs="tp",fx = T)
                 +s(Q2,k=16,bs="tp",fx = T)
                 +s(Q3,k=6,bs="tp",fx = T)
                 +s(Q4,k=4,bs="tp",fx = T)
                 +s(Q5,k=6,bs="tp",fx = T)
                 +s(Q6,k=7,bs="tp",fx = T)
                 +s(Q7,k=5,bs="tp",fx = T)
                 +s(Q8,k=6,bs="tp",fx = T)
                 +s(Q9,k=8,bs="tp",fx = T)
                 +s(Q10,k=14,bs="tp",fx = T)
                 +s(Q12,k=13,bs="tp",fx = T)
                 +Q13+Q14+Q15+Q16+Q17+Q18+Q19
                 +Q20+Q21+Q22+Q23,data = afdm.impute,
                method = "REML")
gam.check(afdm.gam1)
summary(afdm.gam1)
plot.gam(afdm.gam1)
vis.gam(afdm.gam1)
concurvity(afdm.gam1,full = F)
par(mfrow=c(2,2))
#
### pour exporter summary vers latek
library(stargazer)
library(apsrtable)
apsrtable(afdm.gam1,title = "gam",align = T,
          keep.stat = "n",single.row = T)


afdm.gam1 <- gam(Q11~s(Q1,k=sample(1:20, size = 1,replace = F))
                 +s(Q2,k=sample(1:20, size = 1,replace = F))
                 +s(Q3,k=sample(1:20, size = 1,replace = F))
                 +s(Q4,k=sample(1:20, size = 1,replace = F))
                 +s(Q5,k=sample(1:20, size = 1,replace = F))
                 +s(Q6,k=sample(1:18, size = 1,replace = F))
                 +s(Q7,k=sample(1:8, size = 1,replace = F))
                 +s(Q8,k=sample(1:8, size = 1,replace = F))
                 
                 +s(Q10,k=sample(1:9, size = 1,replace = F))
                 
                 +Q13+Q14+Q15+Q16+Q17+Q18+Q19
                 +Q20+Q21+Q22+Q23,data = afdm.impute,
                 method = "REML")

gam.check(afdm.gam1)
summary(afdm.gam1)
par(mfrow=c(2,2))
plot.gam(afdm.gam1,pages = 1)
concurvity(afdm.gam1,full = F)
predict(afdm.gam1)

# Test
pos.var <- 1:12
pos.var <- pos.var[pos.var != 11]
nb.pls_reg.tot <- numeric(length(pos.var))
for(i in 1:length(pos.var)){
  nb.pls_reg.tot[i] <- length(names(table(afdm.impute[, pos.var[i]])))
  
  i <- i + 1
  
  if(i == length(pos.var) + 1){
    nb.pls_reg.tot <- as.data.frame(t(nb.pls_reg.tot))
    
    colnames(nb.pls_reg.tot) <- names(afdm.impute)[pos.var]
    rownames(nb.pls_reg.tot) <- NULL
  }
}

nb.pls_reg.tot
prod(nb.pls_reg.tot)

i <- 1
valeur.sortie <- -8

while(i < 10000 & valeur.sortie < 0){
  
}
####################################



####### other plots
# How many farmers grow > 3 varieties
library(ggplot2)
#ggplot(afdm.impute)+
 # geom_bar(aes(x=Q15, fill=Q23),stat = "count")

ggplot(afdm.impute, aes(Q15)) +
  geom_bar(fill = "black",width = 0.2)+ 
  theme_classic(base_size = 20,base_line_size =2)+
  scale_x_discrete(name="number of Varieties")+
  labs(title = "number of farmers grow > 3 varieties")

##
## Associated crop cycle
ggplot(afdm.impute, aes(Q18)) +
  geom_bar(fill = "black",width = 0.2)+ 
  theme_classic(base_size = 20,base_line_size =2)+
  scale_x_discrete(name="Associated crop cycle")+
  labs(title = "number of farmers make Associated crop cycle")
                                                                                         


## How many used only TC plants?/ suckers
ggplot(afdm.impute, aes(Q20)) +
  geom_bar(fill = "black",width = 0.2)+ 
  theme_classic(base_size = 20,base_line_size =2)+
  scale_x_discrete(name="Type of planting material")+
  labs(title = "number of farmers used only TC plants?/ suckers")

## How many farmers used multiple seed sources?
ggplot(afdm.impute, aes(Q14)) +
  geom_bar(fill = "black",width = 0.2)+ 
  theme_classic(base_size = 20,base_line_size =2)+
  scale_x_discrete(name="seed sources")+
  labs(title = "number of farmers used multiple seed sources")
  

### rogueing per year farmer delaration
ggplot(afdm.impute, aes(Q12))+
  geom_histogram(fill = "black",bins = 30,color="red")+ 
  theme_classic(base_size = 20,base_line_size =2)+
  scale_x_continuous(name="rogueing per year declaration")+
  labs(title = "number of farmers rogued bananas per year")


## number of Hedge presence:yes
ggplot(afdm.impute, aes(Q13)) +
  geom_bar(fill = "black",width = 0.2)+ 
  theme_classic(base_size = 20,base_line_size =2)+
  scale_x_discrete(name="number of Hedge presence:yes")+
  labs(title = "How many farms had a hedge \n (at least one one/two side) and all sides?")

## What is the distribution (histogram) of the land sizes? 
ggplot(afdm.impute, aes(Q1)) +
  geom_histogram(fill = "black",bins = 30,color="red")+ 
  theme_classic(base_size = 20,base_line_size =2)+
  scale_x_continuous(name="land sizes")+
  labs(title = "the distribution of the land sizes")


############################################################
afdm_ok <- read.csv(file = file.choose(),header = T,sep = ";"
         ,dec = ",",na.strings = c(0))
afdm_imputed_ok <- na.omit(afdm_ok)
str(afdm_imputed_ok)


## remplacer une valeur par une autre 
table(afdm.impute[,"Q11"])
which(afdm.impute[,"Q11"]==0)
afdm.impute[afdm.impute[,"Q11"]==0,"Q11"] <- 0.00001
afdm.impute$Q11



library(gamlss)
beta_reg_bbtd <- gamlss(I(Q11/100) ~ Q1+Q2+Q3+Q4+Q5+Q6+Q7+Q8+Q9+Q10+Q12+
                     Q13+Q14+Q15+Q16+Q17+Q18+Q19+Q20+Q21+Q22+Q23,
                   family = BE(log),method=mixed(1,20),
                   data = afdm.impute)

summary(beta_reg_bbtd)

library(betareg)
beta_reg_bbtd <- betareg(Q11a ~ Q1+Q2+Q3+Q4+Q5+Q6+Q7+Q8+Q9+Q10+Q12+
                          Q13+Q14+Q15+Q16+Q17+Q18+Q19+Q20+Q21+Q22+Q23,
                        data = afdm.impute)


### exploration des variables
attach(afdm.impute)

table(Q14)
table(Q15)
table(Q16)

afdm.impute$Q14

summary(Q5)

#### dichotomisation
afdm.impute$Q14 <- ifelse(afdm.impute$Q14>0,1,0)

############## Poisson regression
reg_pois <- glm(Q8~Q1+Q2+Q3+Q4+Q5+Q6+Q7+Q9+Q10+Q11+Q12+Q13+Q14+
                Q15+Q16+Q17+Q18+Q19+Q20+Q21+Q22+Q23+Q24+Q25,
                data = afdm.impute, family = "poisson")
summary(reg_pois)
afdm.impute$Q15

# run Hosmer-Lemeshow test 
library(ResourceSelection)
HL <- hoslem.test(x = full_model$y, y = fitted(full_model), g = 10)

