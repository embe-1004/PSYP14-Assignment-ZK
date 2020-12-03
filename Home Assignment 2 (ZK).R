  #####################
  #                   #  
  # HOME ASSIGNMENT 2 #
  #                   #
  #####################

  
## --- Packages: --- ##
  
  library(psych) # for describe	
  library(car) # for residualPlots, vif, pairs.panels, ncvTest	
  library(lmtest) # bptest	
  library(sandwich) # for coeftest vcovHC estimator		
  library(lmboot) # for wild bootsrapping	
  library(tidyverse) # for tidy code
  library(lm.beta) # for lm.beta	
  library(gridExtra) 	
  
  
## --- Load the data for data_sample_1: --- ##

data_sample_1 = read.csv("https://tinyurl.com/ha-dataset1")

data_sample_1 = data_sample_1 %>% 
  mutate(sex = factor(sex))

view(data_sample_1)

summary(data_sample_1)

## --- Exclude cases that was removed in assignment 1: --- ##

 # I chose to exclude one more case where the income was negative.

data_sample_1 = data_sample_1 %>% 
  slice(-c(93, 109, 150))

summary(data_sample_1)

## --- Create the initial model: --- ##

initial_model = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + weight + IQ + household_income, data = data_sample_1)


## --- Data and model diagnostics: --- ##

summary(data_sample_1)

summary(initial_model)

 # Cook's distance: 

initial_model %>% 
  plot(which=4)

data_sample_1 %>% 
  slice(c(110, 101, 3))


## --- Normality: --- ##

# QQ plot

initial_model %>% 
  plot(which = 2)


# Histogram 

residuals_init_mod = enframe(residuals(initial_model))

residuals_init_mod %>% 
  ggplot()+
  aes(x = value) +
  geom_histogram()


# Skew and kurtosis:

describe(residuals(initial_model))


## --- Linearity: --- ##

initial_model %>% 
  residualPlots()


## --- Homoscedasticity : --- ##

# Scale location: 

initial_model %>% 
  plot(which = 3)

# NCV-test:

initial_model %>% 
  ncvTest()

# The Breush-Pagan test:

initial_model %>% 
  bptest()

## --- Multicollinearity: --- ##

initial_model %>% 
  vif()


## --- Backward regression: --- ## 

step(initial_model, direction = "backward")

backward_model = lm(pain ~ age + sex + pain_cat + mindfulness + cortisol_serum + household_income, data = data_sample_1)

summary(backward_model)


## --- Compare the initial model with the backward model: --- ## 

AIC(initial_model)

AIC(backward_model)


## --- The theory based model: --- ##

theory_based_model = lm(pain ~ age + sex + pain_cat + mindfulness + cortisol_serum + STAI_trait, data = data_sample_1) 

summary(theory_based_model)


## --- Normality: --- ##

# QQ plot

backward_model %>% 
  plot(which = 2)

theory_based_model %>% 
  plot(which = 2)


# Histogram 

residuals_back_mod = enframe(residuals(backward_model))

residuals_back_mod %>% 
  ggplot()+
  aes(x = value) +
  geom_histogram()

residuals_theory_mod = enframe(residuals(theory_based_model))

residuals_theory_mod %>% 
  ggplot()+
  aes(x = value) +
  geom_histogram()


# Skew and kurtosis:

describe(residuals(backward_model))

describe(residuals(theory_based_model))


## --- Linearity: --- ##

backward_model %>% 
  residualPlots()

theory_based_model %>% 
  residualPlots()


## --- Homoscedasticity : --- ##

# Scale location: 

backward_model %>% 
  plot(which = 3)

theory_based_model %>% 
  plot(which = 3)

# NCV-test:

backward_model %>% 
  ncvTest()

theory_based_model %>% 
  ncvTest()

# The Breush-Pagan test:

backward_model %>% 
  bptest()

theory_based_model %>% 
  bptest()

## --- Multicollinearity: --- ##

backward_model %>% 
  vif()

theory_based_model %>% 
  vif()


## --- Compare the backward model with the theory-based model: --- ## 

AIC(backward_model)
AIC(theory_based_model)


 # Anova not appropriate because they are not nested models. 

summary(backward_model)$adj.r.squared
summary(theory_based_model)$adj.r.squared


## --- New data: --- ##

data_sample_2 = read.csv("https://tinyurl.com/ha-dataset2")

data_sample_2 = data_sample_2 %>% 
  mutate(sex = factor(sex))

view(data_sample_2)

summary(data_sample_2)


## --- Make predictions with the new dataset: --- ##

 # Backward model:
 # lm(pain ~ age + sex + pain_cat + mindfulness + cortisol_serum + household_income, data = data_sample_2)

pred_backward_model <- predict(backward_model, data_sample_2)

 # Theory-based model:
 # lm(pain ~ age + sex + pain_cat + mindfulness + cortisol_serum + STAI_trait, data = data_sample_2)

pred_theory_based_model <- predict(theory_based_model, data_sample_2)

 # The sum of squared residuals: 

RSS_backward_model = sum((data_sample_2[, "pain"] - pred_backward_model)^2)

RSS_theory_based_model = sum((data_sample_2[, "pain"] - pred_theory_based_model)^2)


RSS_backward_model
RSS_theory_based_model


## --- The final coefficient table: --- ##

 # Zoltan's function: 

coef_table = function(model){	
  require(lm.beta)	
  mod_sum = summary(model)	
  mod_sum_p_values = as.character(round(mod_sum$coefficients[,4], 3))		
  mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"] = substr(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"], 2, nchar(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"]))		
  mod_sum_p_values[mod_sum_p_values == "0"] = "<.001"		
  
  
  mod_sum_table = cbind(as.data.frame(round(cbind(coef(model), confint(model), c(0, lm.beta(model)$standardized.coefficients[c(2:length(model$coefficients))])), 2)), mod_sum_p_values)		
  names(mod_sum_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value")		
  mod_sum_table["(Intercept)","Std.Beta"] = "0"		
  return(mod_sum_table)	
}	


coef_table(theory_based_model)

coef_table(backward_model)
