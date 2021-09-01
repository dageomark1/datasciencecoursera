hf <- read.csv("HF-data.csv",sep = ",",stringsAsFactors = T)
str(hf)
hf$diabetes
hfdata <- na.omit(hf)
str(hfdata)

quan <- c(1,2,3,4,24,25,31)
quali <- c(1:31)

for (i in 1:length(quan)) {
  
  quali <- quali[quali!=quan[i]]
  
}

for (i in 1:length(hfdata)) {
  
  if(length(which(quan==i))==1){
    hfdata[,i] <- as.numeric(hfdata[,i])
  }
  else{
    hfdata[,i] <- as.factor(hfdata[,i])
  }
}

str(hfdata)

gender <- as.factor(hfdata[,"gender"]) 
fu_time <- hfdata[,"fu_time"] 
death <- hfdata[,"death"] # binary variable (numeric) 

library(survival)
library(ggplot2)
attach(hfdata)

km_fit <- survfit(Surv(fu_time, death) ~ 1)
plot(km_fit)

summary(km_fit, times = c(1:7,30,60,90*(1:10))) 


km_gender_fit <- survfit(Surv(fu_time, death) ~ gender) 

plot(km_gender_fit)

survdiff(Surv(fu_time,death)~gender,rho = 0)

## age dichotomisé
age_65plus <- ifelse(hfdata[,"age"]>=65,1,0) # dichotomise age
hfdata$age_cat <- ifelse(age<65,"under 65",
                     ifelse(age>=65,"65 and above",NA))

survdiff(Surv(fu_time,death)~age_cat,rho = 0)

library(survminer)

table(copd,exclude = NULL)

cox <- coxph(Surv(fu_time,death)~age+gender+prior_dnas+
               ethnicgroup+copd,data = hfdata)
summary(cox)

### l'hypothèse de proportionnalité de Hazard
coxzph <- cox.zph(cox)
coxzph
plot(coxzph)
###########

quintile_5groups <- hfdata[,'quintile'] # best start with the original data set, not from “quintile” 

quintile_5groups[quintile_5groups==0] <- 5 

quintile_5groups <- factor(quintile_5groups) 

table(quintile_5groups, exclude=NULL) 

cox1 <- coxph(Surv(fu_time,death)~age+gender+copd+quintile_5groups+
               ethnicgroup,data = hfdata)

summary(cox1)
####
quintile_5groups <- hfdata[,'quintile'] # best start with the original data set, not from “quintile” 

quintile_5groups[quintile_5groups==0] <- 5 

quintile_5groups <- factor(quintile_5groups) 

table(quintile_5groups, exclude=NULL) 


#####
fit <- coxph(Surv(fu_time, death) ~ gender) # fit the desired model

temp <- cox.zph(fit)# apply the cox.zph function to the desired model

round(temp$table,digits = 6) # display the results

plot(temp) # plot the curves
######################################################
cox_final <- coxph(Surv(fu_time,death)~age+gender+prior_dnas+
               ethnicgroup+copd+los+cancer+cabg+crt+defib+
             dementia+diabetes+hypertension+ihd+mental_health+
               arrhythmias+obesity+pvd+renal_disease+
               valvular_disease+metastatic_cancer+pacemaker+
               pneumonia+prior_appts_attended+pci+stroke+
               senile+quintile,data = hfdata)


#########
# generate cognitive impairment variable (senility and dementia combined)

cog_imp <- as.factor(ifelse(hfdata$dementia == 1 | hfdata$senile == 1, 1, 0))


cox_final <- coxph(Surv(fu_time,death)~age+gender+ethnicgroup
                     +ihd+valvular_disease+pvd+stroke+
                     copd+pneumonia+hypertension+
                     renal_disease+cancer+metastatic_cancer+
                     mental_health+cog_imp+
                     los+prior_dnas,data = hfdata)
summary(cox_final)

###
cox_fin <- coxph(Surv(fu_time,death)~age+gender+valvular_disease+
                     pneumonia+metastatic_cancer+cog_imp,data = hfdata)
summary(cox_fin)
tem <- cox.zph(cox_fin)
print(tem)



fit <- coxph(Surv(fu_time, death) ~ age + gender + valvular_disease + pneumonia + 
               
               metastatic_cancer + cog_imp) # test them all in the same model 

temp <- cox.zph(fit)  

print(temp) 
############
fit <- coxph(Surv(fu_time, death) ~ copd)
temps <- cox.zph(fit = fit,transform = "rank")
round(temps$table,digits = 6)
