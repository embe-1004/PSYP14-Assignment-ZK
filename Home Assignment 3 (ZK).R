   #####################
   #                   #  
   # HOME ASSIGNMENT 3 #
   #                   #
   #####################

   
## --- Packages from exercise 12+13: --- ##
   
   library(psych) # for describe		
   library(tidyverse) # for tidy code and ggplot		
   library(cAIC4) # for cAIC		
   library(r2glmm) # for r2beta		
   library(lme4) # for lmer	
   library(lmerTest) # to get singificance test in lmer	
   library(MuMIn) # for r.squaredGLMM	
   library(optimx) # for optimx optimizer
   

## --- Function to extract standardized beta coefficients: --- ##

stdCoef.merMod <- function(object) {sdy <- sd(getME(object,"y")) 
sdx <- apply(getME(object,"X"), 2, sd)	
sc <- fixef(object)*sdx/sdy	
se.fixef <- coef(summary(object))[,"Std. Error"]	
se <- se.fixef*sdx/sdy	
return(data.frame(stdcoef=sc, stdse=se))	
}	
   

## --- Load the data: --- ##

data_sample_3 = read.csv("https://tinyurl.com/ha-dataset3")

data_sample_4 = read.csv("https://tinyurl.com/ha-dataset4")

 # Asign sex, hospital and ID as factors: 

data_sample_3 = data_sample_3 %>% 
   mutate(sex = factor(sex), hospital = factor(hospital), ID = factor(ID))

data_sample_4 = data_sample_3 %>% 
   mutate(sex = factor(sex), hospital = factor(hospital), ID = factor(ID))


## --- View the data: --- ##

summary(data_sample_3)

summary(data_sample_4)

 # ID_182 is coded wrong, it is called femlae instead of female. I don't know
 # how to recode this so I chose to exlude it from the two datasets.

data_sample_3 = data_sample_3 %>% 
   slice(-c(182))

data_sample_4 = data_sample_4 %>% 
   slice(-c(182))


## --- Build a linear mixed model on data_sample_3: --- ##

rand_int_model = lmer(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + (1|hospital), data = data_sample_3)


## --- Model coefficients and confidence intervals: --- ##

summary(rand_int_model)

 # Confidence intervals of the coefficients for all fixed effect predictors 
 #  --> compare them to the ones obtained in assignment 1.

confint(rand_int_model)

stdCoef.merMod(rand_int_model)


## --- Marginal and conditional R squared: --- ## 

r.squaredGLMM(rand_int_model)

 # Marginal R^2 with confidence intervals:

r2beta(rand_int_model, method = "nsj", data = data_sample_3)


## --- Predict pain on data_sample_4: --- ##

pred_sample_4 = predict(rand_int_model, data_sample_4)

RSS = sum((data_sample_4[, "pain"] - pred_sample_4)^2)

mod_mean <- lm(pain ~ 1, data = data_sample_4)

TSS = sum((data_sample_4[, "pain"] - predict(mod_mean))^2)

R2 = 1-(RSS/TSS)
R2

# --- Build a new linear mixed effects model: --- ##

 # Just include the most influential predictors from the previous model, and 
 # allow for both random intercept and random slope. 

summary(rand_int_model)

 # The most influential predictor is cortisol_serum 

rand_slope_model = lmer(pain ~ cortisol_serum + (cortisol_serum|hospital), data = data_sample_3)


# --- Visualize the fitted regression lines for each hospital: --- ##

data_sample_3_withpreds = data_sample_3

data_sample_3_withpreds$pred_slope = predict(rand_slope_model)

ggplot(data_sample_3_withpreds, aes(y = pain, x = cortisol_serum, group = hospital)) +
   geom_point(size = 3) + 
   geom_line(color='red', aes(y=pred_slope, x = cortisol_serum))+
   facet_wrap( ~ hospital, ncol =5)

