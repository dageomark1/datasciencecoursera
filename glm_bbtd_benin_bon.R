library(xlsx)
pop1<- read.xlsx("bbtd_plosone_bon.xlsx",header = T,
               sheetName = "Feuil1")
dim(pop1)
str(pop1)

names(pop1)

quali <- c(1:7,11:15,20:23,25)
w <- c(1:25)

for (i in 1:length(quali)) {
  w <- w[w!=quali[i]]
}

## transformation des variables en numeric et factor
for (i in 1:length(pop1)) {
  if(length(which(quali==i))==1){
    pop1[,i] <- as.factor(pop1[,i]) 
  }
  else{
    pop1[,i] <- as.numeric(pop1[,i]) 
  } 
}

bbtd_Ben <- pop1[,c(quali,w)]
dim(bbtd_Ben)
str(bbtd_Ben)
# bbtd_Ben <- na.omit(bbtd_Ben)
nrow(na.omit(bbtd_Ben))

bbtd_Ben$Q1 <- factor(bbtd_Ben$Q1,levels = c(1,2),
                        labels=c("box culture","field culture"),
                        ordered = T)

bbtd_Ben$Q7 <- factor(bbtd_Ben$Q7,levels = c(1,2,3,4,5,6,7,8,
                                                 9,10,11,12,13,14),
                        labels=c("Maize","Beans","Soy","Cassava","Yam",
                                 "Sweet potato","Rice","Mil","Sorghum",
                                 "Fruits crops","Tomato","Pepper","Others",
                                 "Pure banana"),ordered = T)

### Les plots pour stat descriptive
library(ggplot2)

bbtd_QA <- aggregate(x = bbtd_Ben$Q13, 
                     by = list(QA = bbtd_Ben$QA, 
                               Q1 = bbtd_Ben$Q1), FUN = sum)
names(bbtd_QA)[3] <- "Q13"
bbtd_QA <- bbtd_QA[order(bbtd_QA[, 1], 
                         bbtd_QA[, 2], 
                         decreasing = FALSE), ]

Q13_mean <- mean(bbtd_Ben$Q13)
Q13_sd <- sd(bbtd_Ben$Q13)

## bbtd en fonction des locatit?s
ggplot(data = bbtd_Ben,aes(x = QA,y = Q13))+ 
  geom_bar(stat="identity",width = .2,
           position="dodge",fill="grey")+
  labs(x = "Localities",
       y = "Maximum number Observed cases of BBTD")+
  scale_x_discrete(labels=c("Akpro-Misserete", "Dangbo", "Sakete",
                            "Houeyogbe","Adjarra",
                            "Athieme"))+theme_classic()+
  theme(axis.text.x = element_text(color="black",size=15, angle=0),
        axis.text.y = element_text(color="black",size=15, angle=0),
        axis.title.x =element_text(size = 15),
        axis.title.y =element_text(size = 15))+
  geom_errorbar(aes(ymin=Q13_mean-Q13_sd,
                    ymax=Q13_mean+Q13_sd), width=.2,
                position = position_dodge(0.05),colour="black")


ggplot(data = bbtd_Ben,aes(x = Q1,y = Q13))+ 
  geom_bar(stat="identity",width = .2,
           position="dodge",fill="grey")+
  labs(x = "Cultivation system",y = "Maximum number Observed cases of BBTD")+
  scale_x_discrete(labels=c("Backyard gardens","Open gardens"))+
  theme_classic()+
  theme(axis.text.x = element_text(color="black",size=15, angle=0),
        axis.text.y = element_text(color="black",size=15, angle=0),
        axis.title.x =element_text(size = 15),
        axis.title.y =element_text(size = 15))+
  geom_errorbar(aes(ymin=Q13_mean-Q13_sd,
                    ymax=Q13_mean+Q13_sd), width=.2,
                position = position_dodge(0.05),colour="black")


ggplot(data = bbtd_Ben,aes(x = Q2,y = Q13))+ 
  geom_bar(stat="identity",width = .2,
           position="dodge",fill="grey")+
  labs(x = "Crop on plot 1 year before current banana crop",
       y = "Maximum number Observed cases of BBTD")+
  theme_classic()+
  scale_x_discrete(labels=c("None(Fallow)","Maize","Cassava",
                            "Banana","Tomatoes","Others"))+
  theme(axis.text.x = element_text(color="black",size=15, angle=0),
        axis.text.y = element_text(color="black",size=15, angle=0),
        axis.title.x =element_text(size = 15),
        axis.title.y =element_text(size = 15))+
  geom_errorbar(aes(ymin=Q13_mean-Q13_sd,
                    ymax=Q13_mean+Q13_sd), width=.2,
                position = position_dodge(0.05),colour="black")


## culture associ?es et bbtd
ggplot(data = bbtd_Ben,aes(x = Q7,y = Q13))+ 
  geom_bar(stat="identity",width = .2,
           position="dodge",fill="grey")+
  labs(x = "Crops associated",
       y = "Maximum number Observed cases of BBTD")+
  theme_classic()+
  scale_x_discrete(labels=c("Maize","Beans","Cassava",
                            "Yam","Fruit crops","Tomatoes",
                            "Others","Pure banana"))+
  theme(axis.text.x = element_text(color="black",size=15, angle=0),
        axis.text.y = element_text(color="black",size=15, angle=0),
        axis.title.x =element_text(size = 15),
        axis.title.y =element_text(size = 15))+
  geom_errorbar(aes(ymin=Q13_mean-Q13_sd,
                    ymax=Q13_mean+Q13_sd), width=.2,
                position = position_dodge(0.05),
                colour="black")

ggplot(data = bbtd_Ben,aes(x = Q14,y = Q13))+ 
  geom_bar(stat="identity",width = .2,
           position="dodge",fill="grey")+
  labs(x = "Presence of aphids on the mother plant",
       y = "Maximum number Observed cases of BBTD")+
  theme_classic()+
  scale_x_discrete(labels=c("No","Yes"))+
  theme(axis.text.x = element_text(color="black",size=15, angle=0),
        axis.text.y = element_text(color="black",size=15, angle=0),
        axis.title.x =element_text(size = 15),
        axis.title.y =element_text(size = 15))+
  geom_errorbar(aes(ymin=Q13_mean-Q13_sd,
                    ymax=Q13_mean+Q13_sd), width=.2,
                position = position_dodge(0.05),
                colour="black")

ggplot(data = bbtd_Ben,aes(x = Q17,y = Q13))+ 
  geom_bar(stat="identity",width = .2,
           position="dodge",fill="grey")+
  labs(x = "Presence of aphids on the suckers",
       y = "Maximum number Observed cases of BBTD")+
  theme_classic()+
  scale_x_discrete(labels=c("No","Yes"))+
  theme(axis.text.x = element_text(color="black",size=15, angle=0),
        axis.text.y = element_text(color="black",size=15, angle=0),
        axis.title.x =element_text(size = 15),
        axis.title.y =element_text(size = 15))+
  geom_errorbar(aes(ymin=Q13_mean-Q13_sd,
                    ymax=Q13_mean+Q13_sd), width=.2,
                position = position_dodge(0.05),
                colour="black")


ggplot(data = bbtd_Ben,aes(x = Q23,y = Q13))+ 
  geom_bar(stat="identity",width = .2,
           position="dodge",fill="grey")+
  labs(x = "Received planting material from neighbours",
       y = "Maximum number Observed cases of BBTD")+
  theme_classic()+scale_x_discrete(labels=c("No","Yes"))+
  theme(axis.text.x = element_text(color="black",size=15, angle=0),
        axis.text.y = element_text(color="black",size=15, angle=0),
        axis.title.x =element_text(size = 15),
        axis.title.y =element_text(size = 15))+
  geom_errorbar(aes(ymin=Q13_mean-Q13_sd,
                    ymax=Q13_mean+Q13_sd), width=.2,
                position = position_dodge(0.05),
                colour="black")

ggplot(data = bbtd_Ben,aes(x = Q30,y = Q13))+ 
  geom_bar(stat="identity",width = .2,
           position="dodge",fill="grey")+
  labs(x = "Destruction of existing banana mats on the plot",
       y = "Maximum number Observed cases of BBTD")+
  theme_classic()+scale_x_discrete(labels=c("No","Yes"))+
  theme(axis.text.x = element_text(color="black",size=15, angle=0),
        axis.text.y = element_text(color="black",size=15, angle=0),
        axis.title.x =element_text(size = 15),
        axis.title.y =element_text(size = 15))+
  geom_errorbar(aes(ymin=Q13_mean-Q13_sd,
                    ymax=Q13_mean+Q13_sd), width=.2,
                position = position_dodge(0.05),
                colour="black")


ggplot(data = bbtd_Ben,aes(x = Q35,y = Q13))+ 
  geom_bar(stat="identity",width = .2,
           position="dodge",fill="grey")+
  labs(x = "Cutting/rooting banana mats infected with BBTD",
       y = "Maximum number Observed cases of BBTD")+
  theme_classic()+scale_x_discrete(labels=c("No","Yes"))+
  theme(axis.text.x = element_text(color="black",size=15, angle=0),
        axis.text.y = element_text(color="black",size=15, angle=0),
        axis.title.x =element_text(size = 15),
        axis.title.y =element_text(size = 15))+
  geom_errorbar(aes(ymin=Q13_mean-Q13_sd,
                    ymax=Q13_mean+Q13_sd), width=.2,
                position = position_dodge(0.05),
                colour="black")


#do graph with wind speed, vegetation and bbtd Q13

ggplot(data = bbtd_Ben,aes(x = Q43.East,y = Q13))+ 
  geom_bar(stat="identity",width = .2,
           position="dodge",fill="grey")+
  labs(x = "Type of vegetation in immediate limit.East",
       y = "Maximum number Observed cases of BBTD")+
  theme_classic()+
  scale_x_discrete(labels=c("Seasonal crop","Annual crops ",
                            "Perennial crops ","Fruit trees ",
                            "Forestry (Non-Fruit)",
                            "No vegetation"))+
  theme(axis.text.x = element_text(color="black",size=15, angle=0),
        axis.text.y = element_text(color="black",size=15, angle=0),
        axis.title.x =element_text(size = 15),
        axis.title.y =element_text(size = 15))+
  geom_errorbar(aes(ymin=Q13_mean-Q13_sd,
                    ymax=Q13_mean+Q13_sd), width=.2,
                position = position_dodge(0.05),
                colour="black")


ggplot(data = bbtd_Ben,aes(x = Q43.South,y = Q13))+ 
  geom_bar(stat="identity",width = .2,
           position="dodge",fill="grey")+
  labs(x = "Type of vegetation in immediate limit.South",
       y = "Maximum number Observed cases of BBTD")+
  theme_classic()+
  scale_x_discrete(labels=c("Seasonal crop",
                            "Perennial crops ","Fruit trees ",
                            "Forestry (Non-Fruit)",
                            "No vegetation"))+
  theme(axis.text.x = element_text(color="black",size=15, angle=0),
        axis.text.y = element_text(color="black",size=15, angle=0),
        axis.title.x =element_text(size = 15),
        axis.title.y =element_text(size = 15))+
  geom_errorbar(aes(ymin=Q13_mean-Q13_sd,
                    ymax=Q13_mean+Q13_sd), width=.2,
                position = position_dodge(0.05),
                colour="black")



## north
ggplot(data = bbtd_Ben,aes(x = Q43.North,y = Q13))+ 
  geom_bar(stat="identity",width = .2,
           position="dodge",fill="grey")+
  labs(x = "Type of vegetation in immediate limit.North",
       y = "Maximum number Observed cases of BBTD")+
  theme_classic()+
  scale_x_discrete(labels=c("Seasonal crop","Annual crops ",
                            "Perennial crops ","Fruit trees ",
                            "Forestry (Non-Fruit)",
                            "No vegetation"))+
  theme(axis.text.x = element_text(color="black",size=15, angle=0),
        axis.text.y = element_text(color="black",size=15, angle=0),
        axis.title.x =element_text(size = 15),
        axis.title.y =element_text(size = 15))+
  geom_errorbar(aes(ymin=Q13_mean-Q13_sd,
                    ymax=Q13_mean+Q13_sd), width=.2,
                position = position_dodge(0.05),
                colour="black")



## west
ggplot(data = bbtd_Ben,aes(x = Q43.West,y = Q13))+ 
  geom_bar(stat="identity",width = .2,
           position="dodge",fill="grey")+
  labs(x = "Type of vegetation in immediate limit.West",
       y = "Maximum number Observed cases of BBTD")+
  theme_classic()+
  scale_x_discrete(labels=c("Seasonal crop",
                            "Perennial crops ","Fruit trees ",
                            "Forestry (Non-Fruit)",
                            "No vegetation"))+
  theme(axis.text.x = element_text(color="black",size=15, angle=0),
        axis.text.y = element_text(color="black",size=15, angle=0),
        axis.title.x =element_text(size = 15),
        axis.title.y =element_text(size = 15))+
  geom_errorbar(aes(ymin=Q13_mean-Q13_sd,
                    ymax=Q13_mean+Q13_sd), width=.2,
                position = position_dodge(0.05),
                colour="black")



## courbe
ggplot(bbtd_Ben) +
  aes(x = Q12, y = Q13) +
  geom_smooth(method = "loess") +
  geom_point()+
  labs(x = "Banana density ",
       y = "Maximum number Observed cases of BBTD")+
  theme_classic()+
  theme(axis.text.x = element_text(color="black",size=15, angle=0),
        axis.text.y = element_text(color="black",size=15, angle=0),
        axis.title.x =element_text(size = 15),
        axis.title.y =element_text(size = 15))


plot(bbtd_Ben$Q12,bbtd_Ben$Q13)
abline(lm(Q13 ~ Q12, data=bbtd_Ben), col='red')
lines(lowess(bbtd_Ben$Q12, bbtd_Ben$Q13),col = "red")



correl <- bbtd_Ben[,c("Q13","Q12","Q11","Q40.East",
                      "Q40.West","Q40.South","Q40.North")]
names(correl) <- c("Observed case of BBTD","Density banana",
                   "Age banana",
                   "Distance near plot in East",
                   "Distance near plot in West",
                   "Distance near plot in South",
                   "Distance near plot in North")
str(correl)

m.cor <- cor(correl)

names(bbtd_Ben)[33:35]
names(bbtd_Ben)[33:35] <- c("Q40.South",
                              "Q40.West","Q40.North")

m.cor <- cor(bbtd_Ben[,c("Q13","Q12","Q11",
                           "Q40.East","Q40.South",
                           "Q40.West","Q40.North")])
library(corrplot)

legend.info <- c("Q13 : Observed case of BBTD \n (response variable)",
                 "Q12 : Density of banana mats on the plot",
                 "Q11 : Age (planting date)",
                 "Q40.East : Distance from target field to \n nearby banana field in East",
                 "Q40.West : Distance from target field to \n nearby banana field in West",
                 "Q40.South : Distance from target field to \n nearby banana field in South",
                 "Q40.North : Distance from target field to \n nearby banana field in North ")

dev.off() # To reset the graphics pars to defaults
par(mar = c(par('mar')[1:3], 0)) # Optional, removes extraneous right inner margin space
plot.new()
l <- legend(0, 0, bty = 'n', legend.info, 
            plot = F, pch = "*", lty = 1, x.intersp = 0, y.intersp = 0.75)
# Calculate right margin width in ndc
w <- grconvertX(l$rect$w, to = 'ndc') - grconvertX(0.35, to = 'ndc')
par(omd = c(0, 1-w, 0, 1))

par(mar=c(0,0,6,0))

corrplot(m.cor, type="lower", order="alphabet",method = "number",
         tl.col="black", tl.srt=45,cl.ratio = 0.1,
         number.cex =1.75,col = c("black","red","blue"),
         diag = F,addgrid.col = NA,tl.cex = 1.2)

 legend(par('usr')[2], par('usr')[4], bty = 'n', xpd = NA,
        legend.info, x.intersp = 0, y.intersp = 0.75)
 
 round(m.cor,2)
 
 
legend("topright",
       legend = legend.info,
       horiz = F, xpd = NA,bty = 'n')

# 
# 
# legend(locator(1),legend = c("Q13:Observed case of BBTD","Q12:Density banana",
#                              "QD:Average Wind speed","Q11:Age of banana plants",
#                              "Q40.East:Distance target field, near_East banana field "),
#        border = F,yjust = 0.5)

### bbtd en fonction de la pr?c?dente culture(Q2)  il y a 1 an sur
## la parcelle suivant les locatit?s
p2 <- ggplot(data = bbtd_Ben,aes(x = QA,y = Q13,fill=Q2))+ 
  geom_bar(stat="identity",width = .5,position=position_dodge())+
  theme_classic(base_size = 20,base_line_size =1)+
  labs(x = "Localit?", y = "Nombre de cas \n de BBTD(Q13)")+
  scale_fill_discrete(name = "culture pass?e(Q2)", 
                      labels = c("Aucune","ma?s","Manioc","banane",
                                 "tomate","autres ? pr?ciser"))+
  scale_x_discrete(labels=c("Akpro-Miss?r?t?", "Dangbo", "Sak?t?",
                            "Hou?yogb?","Adjarra","Athi?m?"))


## bbtd en fonction du type de vari?t?(Q5) suivant les locatit?s
p3 <- ggplot(data = bbtd_Ben,aes(x = QA,y = Q13,fill=Q5))+ 
  geom_bar(stat="identity",width = .5,position=position_dodge())+
  theme_classic(base_size = 20,base_line_size =1)+
  labs(x = "Localit?", y = "Nombre de cas \n de BBTD(Q13)")+
  scale_fill_discrete(name ="Type de vari?t?(Q5)" , 
                      labels = c("plantain","banane douce",
                                 "plantain + douce"))+
  scale_x_discrete(labels=c("Akpro-Miss?r?t?", "Dangbo", "Sak?t?",
                            "Hou?yogb?","Adjarra","Athi?m?"))


## bbtd en fonction de culture associ?e(Q7) suivant les locatit?s
p4 <- ggplot(data = bbtd_Ben,aes(x = QA,y = Q13,fill=Q7))+ 
  geom_bar(stat="identity",width = .5,position=position_dodge())+
  theme_classic(base_size = 20,base_line_size =1)+
  labs(x = "Localit?", y = "Nombre de cas \n de BBTD(Q13)")+
  scale_fill_discrete(name = "culture associ?e(Q7)", 
                      labels = c("ma?s","haricot","Manioc","Igname","fruitiers" 
                                 ,"tomate","autres","pure banane"))+
  scale_x_discrete(labels=c("Akpro-Miss?r?t?", "Dangbo", "Sak?t?",
                            "Hou?yogb?","Adjarra","Athi?m?"))


## bbtd en fonction de mat?riel de plantation(Q8) suivant les locatit?s
p5 <- ggplot(data = bbtd_Ben,aes(x = QA,y = Q13,fill=Q8))+ 
  geom_bar(stat="identity",width = .5,position=position_dodge())+
  theme_classic(base_size = 20,base_line_size =1)+
  labs(x = "Localit?", y = "Nombre de cas \n de BBTD(Q13)")+
  scale_fill_discrete(name = "mat?riel de plantation(Q8)", 
                      labels = c("Rejets","Plantules de MP" 
                                 ,"Rejets+Plantules de MP",
                                 "Rejets, Plantules de MP \n et Vitroplants"))+
  scale_x_discrete(labels=c("Miss?r?t?", "Dangbo", "Sak?t?",
                            "Hou?yogb?","Adjarra","Athi?m?"))

## bbtd en fonction de source d'approviss(Q9) suivant les locatit?s
p6 <- ggplot(data = bbtd_Ben,aes(x = QA,y = Q13,fill=Q9))+ 
  geom_bar(stat="identity",width = .5,position=position_dodge())+
  theme_classic(base_size = 20,base_line_size =1)+
  labs(x = "Localit?" , 
       y = "Nombre de cas \n de BBTD(Q13)")+
  scale_fill_discrete(name ="Source d'approvissionnement \n de plant(Q9)", 
                      labels = c("ONGs","Amis","Instituts","propre champ"))+
  scale_x_discrete(labels= c("Miss?r?t?", "Dangbo", "Sak?t?",
                             "Hou?yogb?","Adjarra","Athi?m?"))

## nbre de cas dans chaque localit?
p7 <- ggplot(data = bbtd_Ben,aes(x = QA,y = Q13,fill=QA))+ 
  geom_bar(stat="identity",width = .5,position=position_dodge())+
  theme_classic(base_size = 20,base_line_size =1)+
  labs(x = "Localit?s(QA)", y = "Nombre de cas \n de BBTD(Q13)")+
  scale_fill_discrete(name = "Localit?", 
                      labels = c("Akpro-Miss?r?t?", "Dangbo", "Sak?t?",
                                 "Hou?yogb?","Adjarra","Athi?m?"))+
  scale_x_discrete(labels = c("Akpro-Miss?r?t?", "Dangbo", "Sak?t?",
                              "Hou?yogb?","Adjarra","Athi?m?"))

###############################################################
library(xlsx)
carte <- read.xlsx(file = file.choose(),
                   sheetName = "Feuil2",
                   header = T)
library(stringr)
str(carte)
str_trim(unlist(str_split(string ="10, 8",pattern= "," )))

split.fun <- function(x){
  return(str_trim(unlist(str_split(string =x,
                                   pattern= "," ))))
}

split.fun("10")

carte2 <- NULL
for(i in 1:nrow(carte)){
  carte.info <- split.fun(carte$Field.ID[i])
  for(j in 1:length(carte.info)){
    carte2 <- rbind(carte2,
                    cbind(carte.info[j], 
                          carte[i, ]))
  }
}

names(carte2)[1] <- "Field.ID.2"
carte2 <- carte2[, c(2, 1, 3:12)]

View(carte2)

group.fun <- function(x){
  return(ifelse(x < 0.001, "G1", 
                ifelse(x < 0.01, "G2", 
                       ifelse(x < 0.05, "G3", "G4"))))
}

carte2$p.value_Grp <- group.fun(carte2$p.value)

View(carte2[, c(1, 2, 4, 9, 13)])
p.value_Grp <- aggregate(x = carte2$p.value_Grp,
                         by = list(p.value_Grp = carte2$p.value_Grp,
                                   Locations = carte2$Locations),
                         FUN = length)
p.value_Grp <- p.value_Grp[order(p.value_Grp[, 1],
                                 p.value_Grp[, 2],
                                 decreasing = FALSE), ]
View(p.value_Grp)


library(xlsx)
rest <- read.xlsx(file = file.choose(),
                  sheetName = "Feuil2",
                  header = T)

which(names(rest)=="EMPLACEMENT_LOCALITE")
names(rest)[10]

rest2 <- rest[,c(9,10)]
prov.id.tot <- 1:71
for(i in 1:nrow(carte2)){
  prov.id.tot <- prov.id.tot[prov.id.tot != as.numeric(carte2[i, 2])]
}

length(prov.id.tot)
rest2 <- rest2[order(rest2[, 2], decreasing = FALSE), ]
rest2[prov.id.tot, ]

str(rest2)
location.fun <- function(x){
  locations.tot <- c("Akpro-Misserete", "Dangbo", "Sakete",
                     "Houeyogbe","Adjarra","Athieme")
  return(locations.tot[x])
}

location.fun(rest2[, 1])
rest2$locations <- location.fun(rest2[, 1])
str(p.value_Grp)
names(table(p.value_Grp$p.value_Grp))
rest2$p.value_Grp <- "G5"
str(rest2)
aggregate(x = rest2$p.value_Grp,
          by = list(p.value_Grp = rest2$p.value_Grp,
                    Locations = rest2$locations),
          FUN = length)

p.value_Grp <- rbind(p.value_Grp, 
                     aggregate(x = rest2$p.value_Grp,
                               by = list(p.value_Grp = rest2$p.value_Grp,
                                         Locations = rest2$locations),
                               FUN = length))

View(p.value_Grp)

group.2.fun <- function(x){
  if(as.numeric(substr(x, 2, 2)) != 1){
    return(paste("G", 
                 as.numeric(substr(x, 2, 2)) - 1, 
                 sep = ""))
  }
  else{
    return(x)
  }
}

p.value_Grp$p.value_Grp.2 <- as.factor(p.value_Grp$p.value_Grp)
levels(p.value_Grp$p.value_Grp.2)
levels(p.value_Grp$p.value_Grp.2) <- c("G1", "G1",
                                       "G2", "G3", "G4")
p.value_Grp$p.value_Grp.2

p.value_Grp.3 <- p.value_Grp[p.value_Grp[,4]!="G4",]

#### graphique

ggplot(data = p.value_Grp,aes(x =Locations ,y = x,fill=p.value_Grp))+ 
  geom_bar(aes(fill=p.value_Grp),stat="identity",width = .2)+
  labs(x = "Localities",y = "Occurrence")+
  theme_classic()+
  scale_fill_manual(name= "Risk levels",
                    labels = c("High risk","Medium risk",
                               "Low risk","Not significant",
                               "Not detected"),
                    values = c("red4","orange","steelblue",
                               "gray","slategray"))+
  theme(axis.text.x = element_text(color="black",size=15, angle=0),
        axis.text.y = element_text(color="black",size=15, angle=0),
        axis.title.x =element_text(size = 15),
        axis.title.y =element_text(size = 15))


# ggplot(data = p.value_Grp,aes(x =Locations ,y = x))+ 
#   geom_col(aes(fill=p.value_Grp),width = .5,
#            position=position_dodge2(width = 0.5,
#                                     preserve = "single"))+
#   theme(axis.text.x = element_text(size=8, angle=0,face = "bold"),
#         axis.text.y =element_text(size=8, angle=0,face = "bold"))+
#   labs(x = "Localities",y = "Occurrence")+
#   theme_classic()+
#   scale_fill_manual(name= "Risk levels",
#                     labels = c("High risk","Medium risk",
#                                "Low risk","Not significant",
#                                "Not detected"),
#                     values = c("red4","orange","steelblue",
#                                "gray","slategray"))

ggplot(data = p.value_Grp,aes(x =Locations ,y = x,fill=p.value_Grp.2))+ 
  geom_bar(aes(fill=p.value_Grp.2),stat="identity",width = .5,
           position=position_dodge(width = 0.5,
                                   preserve = "single"))+
  theme(axis.text.x = element_text(size=8, angle=0,face = "bold"),
        axis.text.y =element_text(size=8, angle=0,face = "bold"))+
  labs(x = "Localities",y = "Occurrence")+
  theme_classic()+
  scale_fill_manual(name= "Risk levels",
                    labels = c("High risk",
                               "Low risk","Not significant",
                               "Not detected"),
                    values = c("red4","steelblue",
                               "gray","slategray"))

### GLM 

## Avec Q43 West,North et Est mais sans Q12( NON )
poiss1_glm <- glm(Q13~Q1+Q2+Q5+Q7+Q8+Q9+Q14+Q17+Q23+QD+Q35+Q40.Est+
                    Q40.West+Q40.North+Q40.South+Q30+QE+Q43.Est
                  +QD+Q43.West+Q43.North+Q12,data = bbtd_Ben, family = "poisson")
summary(poiss1_glm)


#############################################################
## Bon modele (BON A UTILISER POUR l'ANALYSE sans Q43 West,North)
## 
poiss2_glm <- glm(Q13~Q1+Q2+Q5+Q7+Q8+Q9+Q14+Q17+Q23+Q30+Q40.East+
                    Q40.West+Q40.North+Q40.South+Q35+Q11+Q12+QD+QE
                  +Q43.East+Q43.South,data = bbtd_Ben, 
                  family = "poisson")
summary(poiss2_glm)

round(exp(coef(poiss2_glm)),4)
round(exp(cbind(coef(poiss2_glm))),4)

library(broom)
tidy(poiss2_glm, conf.int = F, exponentiate = TRUE)

library(jtools)
library(broom.mixed)
library(ggstance)
# plot regression coefficients for poisson.model2
plot_summs(poiss2_glm, scale = TRUE, exp = TRUE)

### pour exporter summary vers latek
library(stargazer)
library(apsrtable)
apsrtable(poiss2_glm, digits=1, align="l", 
          stars=1, model.counter=0, order="rl",
          coef.rows=1, col.hspace="3em", float="sidewaystable")

stargazer(poiss2_glm,type = "latex",out = "latex")

## test de Hosmer-Lemeshow
# pvaleur > 0.05, on admet que le mod?le est bien adapt? aux donn?es

library(ResourceSelection)
hoslem.test(x = poiss2_glm$y,fitted(poiss2_glm),g = 10)

##Test de Cameron et Trivedi:Si p-valeur < 0,05, 
##on affirme la dispersion anormale
library(AER)
dispersiontest(poiss2_glm)

#########################################
## negative binomial regression
library(MASS)
negbin_bbtd <- glm.nb(Q13~Q2+Q7+Q14+Q17+Q23+Q40.East+
                  Q40.West+Q40.North+Q40.South+Q35+Q11+Q12+QD
                +Q43.East+Q43.South,data = bbtd_Ben)

summary(negbin_bbtd)
cbind(Estimate= coef(negbin_bbtd),
      Odd_Ratio = exp(coef(negbin_bbtd)), 
      confint.default(negbin_bbtd))

# run Hosmer-Lemeshow test 
library(ResourceSelection)
hoslem.test(x = negbin_bbtd$y, y = fitted(negbin_bbtd), g = 10)


library(DHARMa)
resnb <- simulateResiduals(negbin_bbtd,refit = T)
plot(resnb)
testDispersion(resnb, plot=F)



varis <- c("Q1","Q2","Q5","Q7","Q8","Q9","Q14","Q17",
           "Q23","QD","Q35","Q40.Est","Q40.West",
           "Q40.North","Q40.South","Q30","QE","Q11","Q12",
           "QD","Q43.Est","Q43.South")

varis
library(ResourceSelection)
result <- NULL
pois_R2 <- glm(Q13~1,data = bbtd_Ben,family = "poisson")
hosm <- hoslem.test(x = bbtd_Ben$Q13,fitted(pois_R2))
for (i in 1:length(varis)) {
  comb <- combn(varis,i)
  for (j in 1:ncol(comb)) {
    
    #as.fomula cr?e une formule pour un mod?le avec un grand nombre de variable
    frml <- as.formula(paste('Q13 ~ ', paste(comb[,j], collapse = "+")))
    pois_R2 <- glm(frml,data = bbtd_Ben,family = "poisson")
    
    hosm <- hoslem.test(x = bbtd_Ben$Q13,fitted(pois_R2)) 
    
    result <- rbind(result,cbind(i,hosm$p.value))
  }
}







### lsmeans
library(lsmeans)
lsmeans(poiss2_glm,pairwise~Q1)


# fit model --------------------------------------------------------------------
model <- glm(Number ~ Distance + Rain + Method, family = "poisson")

# use emmeans ------------------------------------------------------------------
library(emmeans)
# all post-hoc comparisons:
pairs(emmeans(model, ~ Distance))

# just the contrast you wanted:
contrast(emmeans(model, ~ Distance), method = "trt.vs.ctrl", ref = 1)
# see ?contrast-methods for more information on types of methods of contrasts
# as well as how to specify the reference (that is, it should be the level value
# and not the label)

### pairwise comparison
library(multcomp)
glht(poiss2_glm, linfct=mcp(Q7="Tukey"))




library(car)
Anova(poiss2_glm, type="II", test="Wald")

#########################################################


## Avec Q13(nbre de plant malade) comme variable a expliqu?e
## Avec Q43 South,Est et North; sans Q45.Est

## Pas Bon modele ############ 
poiss3_glm <- glm(Q13~Q1+Q2+Q5+Q7+Q8+Q9+Q11+Q14+Q17+Q23+QD+Q35+Q40.Est+
                    Q40.West+Q40.North+Q40.South+Q30+QE+QD+
                    Q43.South+Q43.North+Q12,data = bbtd_Ben, family = "poisson")
summary(poiss3_glm)

library(ResourceSelection)
hoslem.test(x = bbtd_Ben$Q13,fitted(poiss3_glm,g=10))


## sans Q12 et Q43.South,Q43.North
M1 <- glm(Q13~Q1+Q2+Q5+Q7+Q8+Q9+Q14+Q17+Q23+QD+Q35+Q40.Est+
            Q40.West+Q40.North+Q40.South+Q30+QE+QD+Q11+
            Q43.Est+Q43.South+Q43.North,data = bbtd_Ben,
          family = "poisson")
summary(M1)

library(ResourceSelection)
hoslem.test(x = bbtd_Ben$Q13,fitted(M1,g=10))
##Test de Cameron et Trivedi:Si p-valeur < 0,05, 
##on affirme la dispersion anormale
library(AER)
dispersiontest(M1)

## sans Q12 et Q43.West,Q43.North
M2 <- glm(Q13~Q1+Q2+Q5+Q7+Q8+Q9+Q14+Q17+Q23+QD+Q35+Q40.Est+
            Q40.West+Q40.North+Q40.South+Q30+QE+QD+Q11+
            Q43.Est+Q43.South,data = bbtd_Ben,
          family = "poisson")
summary(M2)

library(ResourceSelection)
hoslem.test(x = bbtd_Ben$Q13,fitted(M2,g=10))
##Test de Cameron et Trivedi:Si p-valeur < 0,05, 
##on affirme la dispersion anormale
library(AER)
dispersiontest(poiss2_glm)

##################################



## sans Q12 et Q43.Est,Q43.North
M3 <- glm(Q13~Q1+Q2+Q5+Q7+Q8+Q9+Q14+Q17+Q23+QD+Q35+Q40.Est+
            Q40.West+Q40.North+Q40.South+Q30+QE+QD+Q11+
            Q43.West+Q43.South,data = bbtd_Ben,
          family = "poisson")
summary(M3)

library(ResourceSelection)
hoslem.test(x = bbtd_Ben$Q13,fitted(M3,g=10))


## sans Q12 et Q43.West,Q43.North
M4 <- glm(Q13~Q1+Q2+Q5+Q7+Q8+Q9+Q14+Q17+Q23+QD+Q35+Q40.Est+
            Q40.West+Q40.North+Q40.South+Q30+QE+QD+Q11+
            Q43.Est+Q43.North,data = bbtd_Ben,
          family = "poisson")
summary(M4)

library(ResourceSelection)
hoslem.test(x = bbtd_Ben$Q13,fitted(M4,g=10))


## sans Q12 et Q43.West,Q43.North (MODELE TRES BON A UTILISER)
M5 <- glm(Q13~Q1+Q2+Q5+Q7+Q8+Q9+Q14+Q17+Q23+QD+Q35+Q40.Est+
            Q40.West+Q40.North+Q40.South+Q30+QE+QD+Q11+
            Q43.South+Q43.North,data = bbtd_Ben,
          family = "poisson")
summary(M5)

library(ResourceSelection)
hoslem.test(x = bbtd_Ben$Q13,fitted(M5,g=10))


M6 <- glm(Q13~Q1+Q2+Q5+Q7+Q8+Q9+Q14+Q17+Q23+QD+Q35+Q40.Est+
            Q40.West+Q40.North+Q40.South+Q30+QE+QD+Q11+
            Q43.West+Q43.Est,data = bbtd_Ben,
          family = "poisson")
summary(M6)

library(ResourceSelection)
hoslem.test(x = bbtd_Ben$Q13,fitted(M6,g=10))


M7 <- glm(Q13~Q1+Q2+Q5+Q7+Q8+Q9+Q14+Q17+Q23+QD+Q35+Q40.Est+
            Q40.West+Q40.North+Q40.South+Q30+QE+QD+Q11+
            Q43.West+Q43.North,data = bbtd_Ben,
          family = "poisson")
summary(M7)

library(ResourceSelection)
hoslem.test(x = bbtd_Ben$Q13,fitted(M7,g=10))

anova(M1,M2,M3,M4,M5,M6,M7,test="Chisq")



## AVEC Q12

## avec Q12
Ma <- glm(Q13~Q1+Q2+Q5+Q7+Q8+Q9+Q14+Q17+Q23+QD+Q35+Q40.Est+
            Q40.West+Q40.North+Q40.South+Q30+QE+QD+Q11+
            Q43.Est+Q43.South+Q43.North+Q12,data = bbtd_Ben,
          family = "poisson")
summary(Ma)

library(ResourceSelection)
#pvaleur > 0.05, on admet que le mod?le est bien adapt? aux donn?es
hoslem.test(x = bbtd_Ben$Q13,fitted(Ma,g=10))
##Test de Cameron et Trivedi:Si p-valeur < 0,05, 
##on affirme la dispersion anormale
library(AER)
dispersiontest(Ma)

## avec Q12
Mb <- glm(Q13~Q1+Q2+Q5+Q7+Q8+Q9+Q14+Q17+Q23+QD+Q35+Q40.Est+
            Q40.West+Q40.North+Q40.South+Q30+QE+QD+Q11+
            Q43.Est+Q43.South+Q12,data = bbtd_Ben,
          family = "poisson")
summary(Mb)

library(ResourceSelection)
hoslem.test(x = bbtd_Ben$Q13,fitted(Mb,g=10))
##Test de Cameron et Trivedi:Si p-valeur < 0,05, 
##on affirme la dispersion anormale
library(AER)
dispersiontest(Mb)


## avec Q12
Mc <- glm(Q13~Q1+Q2+Q5+Q7+Q8+Q9+Q14+Q17+Q23+QD+Q35+Q40.Est+
            Q40.West+Q40.North+Q40.South+Q30+QE+QD+Q11+
            Q43.West+Q43.South+Q12,data = bbtd_Ben,
          family = "poisson")
summary(Mc) ## Bon aussi

library(ResourceSelection)
hoslem.test(x = bbtd_Ben$Q13,fitted(Mc,g=10))


## avec Q12
Md <- glm(Q13~Q1+Q2+Q5+Q7+Q8+Q9+Q14+Q17+Q23+QD+Q35+Q40.Est+
            Q40.West+Q40.North+Q40.South+Q30+QE+QD+Q11+
            Q43.Est+Q43.North+Q12,data = bbtd_Ben,
          family = "poisson")
summary(Md) ## Pas Bon

library(ResourceSelection)
hoslem.test(x = bbtd_Ben$Q13,fitted(Md,g=10))


## avec Q12
Me <- glm(Q13~Q1+Q2+Q5+Q7+Q8+Q9+Q14+Q17+Q23+QD+Q35+Q40.Est+
            Q40.West+Q40.North+Q40.South+Q30+QE+QD+Q11+
            Q43.South+Q43.North+Q12,data = bbtd_Ben,
          family = "poisson")
summary(Me)

library(ResourceSelection)
hoslem.test(x = bbtd_Ben$Q13,fitted(Me,g=10))


## avec Q12
Mf <- glm(Q13~Q1+Q2+Q5+Q7+Q8+Q9+Q14+Q17+Q23+QD+Q35+Q40.Est+
            Q40.West+Q40.North+Q40.South+Q30+QE+QD+Q11+
            Q43.West+Q43.Est+Q12,data = bbtd_Ben,
          family = "poisson")
summary(Mf)

library(ResourceSelection)
hoslem.test(x = bbtd_Ben$Q13,fitted(Mf,g=10))



## avec Q12
Mg <- glm(Q13~Q1+Q2+Q5+Q7+Q8+Q9+Q14+Q17+Q23+QD+Q35+Q40.Est+
            Q40.West+Q40.North+Q40.South+Q30+QE+QD+Q11+
            Q43.West+Q43.North+Q12,data = bbtd_Ben,
          family = "poisson")
summary(Mg)

library(ResourceSelection)
hoslem.test(x = bbtd_Ben$Q13,fitted(Mg,g=10))

anova(Ma,Mb,Mc,Md,Me,Mf,Mg,test="Chisq")


##################################################


## les tests

#########
varselect <- glm(Q13~.,data = popselect,family = "poisson")



####
library(stats)
step(poiss_glm, direction = "both", k = 2)

