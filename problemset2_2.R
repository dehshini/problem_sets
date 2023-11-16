#load the dataset as nepalA

#quick view the dataset
head(nepalA)

#filter the dataset and select age <= 12 and omit NAs of height, weight, armcir
nepaldata = nepalA %>% 
  filter(age <= 12) %>% 
  filter(!is.na(height), !is.na(weight), !is.na(armcirc))

#create gender variable, replace 1 and 2 by male and female. 
nepaldata = nepaldata %>% 
  mutate(gender = ifelse(sex==1, "Male", "Female"))

#scatter-plot of weight against age, color it by gender
#use ggplot
g = ggplot(nepaldata, aes(x=age, y=weight))
g = g + geom_point(aes(shape=gender, colour=gender), position = "jitter") + 
  xlab("age in months") +
  ylab("weight in kg") + 
  theme_minimal() +
  geom_smooth(method=lm, colour="black") + 
  geom_smooth(method=lm, aes(x=age, y=modelA$fitted.values, colour=gender))

#create a multiple regression of weight on age and gender
model1 = lm(weight ~ age + gender, data=nepaldata)
summary(model1)
confint(model1)

#create an ancova model for weight on age and gender(male reference)
modelA = lm(weight ~ age + gender + age:gender, data=nepaldata)
summary(modelA)

#fit a line for the fitted values for for model A(there will be 2, one for males and females)
qplot(x=jitter(age), y=weight, color=gender, shape=gender, data=nepaldata,
      xlab="Age in months", ylab="Weight in kg") +
  geom_line(aes(x = age, y=modelA$fitted.values, color=gender))

#plot the residuals on y and predictor on x
qplot(y=modelA$residuals, x=jitter(age), color=gender, shape=gender,
      data=nepalData, ylab="Residuals", xlab="Age in months")

#use gg
res_v_x = ggplot(data = nepaldata)
res_v_x = res_v_x +
  geom_point(position = "jitter", 
             alpha=1, 
             size=1.5, 
             aes(x=age, y=modelA$residuals, colour=gender)) +
  xlab("age in months") +
  ylab("residuals") +
  theme_minimal()

#create a new variable for the linear spline with the age knot at 4months
nepaldata = nepaldata %>% 
  mutate(age_splined = ifelse(age > 4, age-4, 0))

#create for new. model for the splined age variable.
modelB = lm(weight ~ age*gender + age_splined*gender, data=nepaldata)
summary(modelB)
confint(modelB)

#plot the fitted values from modelB against age
qplot(x=jitter(age), y=weight, color=gender, shape=gender,
      data=nepaldata, xlab="Age in months", ylab="Weight in kg") +
  geom_line(aes(x = age, y=modelB$fitted.values, color=gender))

#use ggplot
gB = ggplot(nepaldata, aes(x=age, y=weight))
gB = gB + geom_point(aes(shape=gender, colour=gender), position = "jitter") + 
  xlab("age in months") +
  ylab("weight in kg") + 
  theme_minimal()
gB = gB + geom_line(aes(x = age, y=modelB$fitted.values, color=gender))

#anova test
anova(modelA, modelB)

#plot the residuals from modelB against age
gB_res = ggplot(data=nepaldata)
gB_res = gB_res + geom_point(aes(x=age, y=modelB$residuals, colour=gender, shape=gender), position = "jitter") +
  xlab("age in months") +
  ylab("residuals") +
  theme_minimal()
gB_res = gB_res + geom_hline(yintercept=0, color="red")

#######################
#part 2 of problem set#
#######################
#use same dataset

#sample mean weight for each month of age
nepaldata %>% group_by(age) %>% summarise(mean=mean(weight))

#ggplot it. #add means to plot "g"
g + stat_summary(aes(x=age, y=weight), fun = mean, colour="red", linewidth=1.2, geom="line")

#regress weight on age
model2 = lm(weight ~ age, data=nepaldata)
summary(model2)

#gglot it. # add regression line to plot "g"
g + geom_smooth(method=lm, colour="black")

#plot residuals vs age for model2
g2 = ggplot(data=nepaldata, aes(x=age, y=model2$residuals, colour=gender, shape=gender))
g2 + xlab("age in months") + 
  ylab("residuals of model2") + 
  geom_point(aes(), position="jitter") + 
  geom_smooth(aes(x=age, y=model2$residuals), method=loess) + 
  theme_minimal() + 
  geom_hline(yintercept = 0, colour="black")

#regress weight on the different ages
model3 = lm(weight ~ as.factor(age), data=nepaldata)
summary(model3)

#lets do a multi-spline model. 
#create new variables
nepaldata = nepaldata %>%
  mutate(age_sp1 = ifelse(age > 2, age-2, 0)) %>%
  mutate(age_sp2 = ifelse(age > 4, age-4, 0)) %>%
  mutate(age_sp3 = ifelse(age > 6, age-6, 0))

#regress weight on the new variables
model4 = lm(weight ~ age + age_sp1 + age_sp2 + age_sp3, data=nepaldata)
summary(model4)

#add the predicted values from model4 to the first graph "g"
g + geom_line(aes(x = age, y=model4$fitted.values), color="red", linewidth=1.4)

#compare this multi-splined model to the linear model
anova(model2, model4)

#do AIC comparison for the 3 models.
AIC(model2, model3, model4)

#use glm to create the 3 models
model2g = glm(weight~age, data=nepaldata)
model3g = glm(weight~as.factor(age), data=nepaldata)
model4g = glm(weight~age+age_sp1+age_sp2+age_sp3, data=nepaldata)

#get the log-likelihoods from the glm models
logLik(model2g)
logLik(model3g)
logLik(model4g)

#AIC
AIC(model2g)-2
AIC(model3g)-2
AIC(model4g)-2
