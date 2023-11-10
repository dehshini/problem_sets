#load the dataset as nepalA

#quick view the dataset
head(nepalA)

#filter the dataset and select age <= 12 and omit NAs of height, weight, armcir
nepaldata = nepalA %>% 
  filter(age <= 12) %>% 
  filter(!is.na(height), !is.na(weight), !is.na(armcirc))

#replace 1 and 2 by male and female. 
nepaldata = nepaldata %>%
  mutate(gender = recode_factor(sex, `1`="Male", `2`="Female"))

#Make a scatter-plot of weight against age, color it by gender
qplot(x=jitter(age), y=weight, color=gender, shape=gender,
      data=nepaldata, xlab="Age in months", ylab="Weight in kg")

#use ggplot
g = ggplot(nepalData, aes(x=age, y=weight, colour=gender))
g = g + geom_point(size=1)

#create a multiple regression of weight on age and gender
model1 = lm(weight ~ age + gender, data=nepaldata)
summary(model1)
confint(model1)

modelA = lm(weight ~ age + gender + age:gender, data=nepaldata)
summary(modelA)


qplot(x=jitter(age), y=weight, color=gender, shape=gender, data=nepaldata,
      xlab="Age in months", ylab="Weight in kg") +
  geom_line(aes(x = age, y=modelA$fitted.values, color=gender))