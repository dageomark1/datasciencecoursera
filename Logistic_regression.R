diabete <- read.csv("diabetes.csv",header = T,
         sep = ",",stringsAsFactors = T)


dim(diabete)
names(diabete)
str(diabete)

diabete$age_cat <- ifelse(diabete$age<45,"<45 ans",
                  ifelse(diabete$age>=45 & diabete$age<65,"45-64 ans",
                         ifelse(diabete$age>=65 & diabete$age<75,"65-74 ans",
                                ifelse(diabete$age>=75,"75 ans et plus",NA))))



diabete$age_cat <- factor(diabete$age_cat,
                   levels = c("<45 ans","45-64 ans",
                            "65-74 ans","75 ans et plus"),
                   ordered = T)

table(diabete$age_cat)

t <- table(diabete$age_cat,diabete$gender,exclude = NULL)

prop.table(t,margin = 1)
round(100 * prop.table(t, margin = 1), digits = 1)
# this will sum up the gender totals to give an overall total
addmargins(t,margin = 1)

#############
attach(diabete)
# create a cross tabulation of age and diabetes status  
dm_by_age <- table(age, dm) 

# output the frequencies of diabetes status by age 
freq_table <- prop.table(dm_by_age, margin = 1) 

# calculate the odds of having diabetes 
odds <- freq_table[, "yes"]/freq_table[, "no"] 

# calculate the log odds 
logodds <- log(odds) 

# plot the ages found in the sample against the log odds of having diabetes 
plot(rownames(freq_table), logodds) 

########### GLM ########
logit_reg <- glm(dm~gender,family = binomial(link = "logit"),
                 data = diabete)
summary(logit_reg)
exp(logit_reg$coefficients)
##########################################
## AGE
summary(age)
plot(density(age))
hist(age)
### BMI
diabete$bmi <- height/weight
summary(bmi)
hist(bmi)
plot(bmi)
plot(density(bmi,na.rm = T))
### HDL
summary(hdl)
hist(hdl)
plot(hdl)
plot(density(hdl,na.rm = T))
#### chol
summary(chol)
hist(chol)
plot(chol)
plot(density(chol,na.rm = T))
###############################################
chol.no.na <- chol[is.na(chol)==0]

# calculate the odds of having diabetes by gender 
odds_gender <- dm_by_gender_prop[, "yes"]/dm_by_gender_prop[, "no"] 

# calculate the log odds 
logodds_gender <- log(odds_gender) 

# define the age variable (continuous) 
age <- age <- diabete[,"age"] 


# create a cross tabulation of age and diabetes status  
dm_by_age <- table(age, dm) # not including NA values because there aren't that many 

# output the frequencies of diabetes status by age 
dm_by_age_prop <- prop.table(dm_by_age, margin = 1) 

# calculate the odds of having diabetes 
odds_age <- dm_by_age_prop[, "yes"]/dm_by_age_prop[, "no"] 

# calculate the log odds 
logodds_age <- log(odds_age) 

# plot the ages found in the sample against the log odds of having diabetes 
plot(rownames(dm_by_age_prop), logodds_age) 

#bmi 
height <- diabete[,"height"] 
weight <- diabete[,"weight"] 
height.si <- height*0.0254 
weight.si <- weight*0.453592 
bmi <- weight.si/height.si^2 


# categorising BMI 

bmi_categorised <- ifelse(bmi < 18.5, "underweight", 
                          ifelse(bmi >= 18.5 & bmi <= 25, "normal", 
                                 ifelse(bmi > 25 & bmi <= 30, "overweight", 
                                        ifelse(bmi > 30, "obese", NA)))) 

# make sure that it is treated as a factor/categorical variable and ordering the levels within the factor for the table 
bmi_categorised <- factor(bmi_categorised, levels = c("underweight", "normal", "overweight","obese")) 

# create a cross tabulation of BMI and diabetes status  
dm_by_bmi_categorised <- table(bmi_categorised, dm) # not including NA values because there aren't that many 

# output the frequencies of diabetes status by BMI 
dm_by_bmi_categorised_prop <- prop.table(dm_by_bmi_categorised, margin = 1) 

# calculate the odds of having diabetes 
odds_bmi_categorised <- dm_by_bmi_categorised_prop[, "yes"]/dm_by_bmi_categorised_prop[, "no"] 

# calculate the log odds 
logodds_bmi_categorised <- log(odds_bmi_categorised) 

# plot the BMI categories found in the sample against the log odds of having diabetes 
dotchart(logodds_bmi_categorised) 

############ GLM ##########
logit_mult <- glm(dm~age+gender+bmi,
                  family = binomial(link = "logit"),data = diabete)

summary(logit_mult)
## Exponentiated coefficients and confident interval
cbind(Estimate= coef(logit_mult),Exponentiate = exp(coef(logit_mult)), 
      confint.default(logit_mult))

# The standard errors are all pretty small, so that’s fine. 

###### QUIZ
diabete$insurance <- as.factor(diabete$insurance)
logit_multi <- glm(dm~age+chol+bmi+hdl+bp.1s+bp.1d+factor(gender)+
                     factor(location)+factor(frame)+
                     factor(insurance)+factor(smoking),
                  family = binomial(link = "logit"),
                  data = diabete)

summary(logit_multi)
## Exponentiated coefficients and confident interval
cbind(Estimate= coef(logit_multi),
      Exponentiate = exp(coef(logit_multi)), 
      confint.default(logit_multi))

## c-statistic
require(DescTools) 
Cstat(full_model)


# run Hosmer-Lemeshow test 
require(ResourceSelection)
HL <- hoslem.test(x = full_model$y, y = fitted(full_model), g = 10)

require(generalhoslem) 
## Loading required package: generalhoslem 
## Warning: package 'generalhoslem' was built under R version 3.5.1 
## Loading required package: reshape 
## Warning: package 'reshape' was built under R version 3.5.1 
## Loading required package: MASS 
# run Hosmer-Lemeshow test 
logitgof(obs = full_model$y, exp = fitted(full_model), g = 10) 
