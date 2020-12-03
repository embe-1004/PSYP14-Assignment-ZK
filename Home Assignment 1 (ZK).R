   #####################
   #                   #  
   # HOME ASSIGNMENT 1 #
   #                   #
   #####################
   
   
## --- Load the data: --- ##
   
data_sample_1 = read.csv("https://tinyurl.com/ha-dataset1")


## --- Check the data: --- ##

view(data_sample_1)

data_sample_1 = data_sample_1 %>% 
   mutate(sex = factor(sex))

data_sample_1 %>% 
   summary()

 # When looking at the summary of the data I can see that there is a coding 
 # error in the variable age, the maximum value is 444 - and there is no way 
 # someone can be 444 years old. There is also an error in the variable STAI_trait,
 # the scale is 20-80 and the summary shows the lowest value of 3.9 which is 
 # not within the given scale. 
 # Therefore I will remove these observations (ID 93 + 150) from the dataset: 

data_sample_1 = data_sample_1 %>% 
   slice(-c(93, 150))

data_sample_1 %>% 
   summary()


## --- Build models to conduct a hierarchical regression: --- ##

 # Model 1 should contain the predictors: age and sex.

model_1 <- lm(pain ~ age + sex, data = data_sample_1)

summary(model_1)


 # Model 2 should contain the predictors: age, sex, STAI (STAI_trait), pain catastrophizing
 # (pain_cat), mindfulness, and cortisol measures (cortisol_serum + cortisol_saliva). 

model_2 <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + cortisol_saliva, data = data_sample_1)

summary(model_2)


## --- Run data and model diagnostistics: --- ## 

 # Visualization:

data_sample_1 %>% 	
   mutate(rownum = row.names(data_sample_1)) %>% 	
   ggplot() +	
   aes(x = age, y = pain, label = rownum) +	
   geom_label()

data_sample_1 %>% 	
   mutate(rownum = row.names(data_sample_1)) %>% 	
   ggplot() +	
   aes(x = sex, y = pain, label = rownum) +	
   geom_label()

data_sample_1 %>% 	
   mutate(rownum = row.names(data_sample_1)) %>% 	
   ggplot() +	
   aes(x = STAI_trait, y = pain, label = rownum) +	
   geom_label()

data_sample_1 %>% 	
   mutate(rownum = row.names(data_sample_1)) %>% 	
   ggplot() +	
   aes(x = pain_cat, y = pain, label = rownum) +	
   geom_label()

data_sample_1 %>% 	
   mutate(rownum = row.names(data_sample_1)) %>% 	
   ggplot() +	
   aes(x = mindfulness, y = pain, label = rownum) +	
   geom_label()

data_sample_1 %>% 	
   mutate(rownum = row.names(data_sample_1)) %>% 	
   ggplot() +	
   aes(x = cortisol_serum, y = pain, label = rownum) +	
   geom_label()

data_sample_1 %>% 	
   mutate(rownum = row.names(data_sample_1)) %>% 	
   ggplot() +	
   aes(x = cortisol_saliva, y = pain, label = rownum) +	
   geom_label()


 # Cook's distance:

model_1 %>% 
   plot(which=4)

model_2 %>% 
   plot(which=4)


 # Check the cases with high Cook's distance:

    # Model 1:
data_sample_1 %>% 
   slice(c(139, 126, 98))

    # Model 2:
data_sample_1 %>% 
   slice(c(112, 98, 68))


## --- Normality: --- ##

 # QQ plot

model_1 %>% 
   plot(which = 2)

model_2 %>% 
   plot(which = 2)


 # Histogram 

residuals_model_1 = enframe(residuals(model_1))

residuals_model_1 %>% 
   ggplot()+
   aes(x = value) +
   geom_histogram()

residuals_model_2 = enframe(residuals(model_2))

residuals_model_2 %>% 
   ggplot()+
   aes(x = value) +
   geom_histogram()

 # Skew and kurtosis:

describe(residuals(model_1))

describe(residuals(model_2))


## --- Linearity: --- ##

model_1 %>% 
   residualPlots()

model_2 %>% 
   residualPlots()


## --- Homoscedasticity : --- ##

 # Scale location: 

model_1 %>% 
   plot(which = 3)

model_2 %>% 
   plot(which = 3)

 # NCV-test:

model_1 %>% 
   ncvTest()

model_2 %>% 
   ncvTest()

 # The Breush-Pagan test:

model_1 %>% 
   bptest()

model_2 %>% 
   bptest()

## --- Multicollinearity: --- ##

model_1 %>% 
   vif()

model_2 %>% 
   vif()

 # The vif-test indicates that there is a case of data multicollinearity. 
 # Check the correlation between cortisol_serum and cortisol_saliva:

data_sample_1 %>% 
   select(pain, age, sex, STAI_trait, pain_cat, mindfulness, cortisol_serum, cortisol_saliva) %>%
   pairs.panels(col = "red", lm = T)

 # Because there is a problem with multicollinierarity in this model I choose 
 # to remove the variable cortisol_saliva from the model. I choose this one 
 # because serum cortisol is often regarded in medical research as more
 # reliably related to stress. 
 # (Don't foreget to do all the data and model diagnostics again! - I will not 
 # put all the above codes down here again, I kept the same name for the model 
 # and will just use the ones above)

model_2 = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = data_sample_1)

summary(model_2)


## --- Model comparison: --- ##

AIC(model_1)
AIC(model_2)

anova(model_1, model_2)


## --- Make the final coefficient table: --- ##

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

coef_table(model_1)

coef_table(model_2)


   