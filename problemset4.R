#tidyverse
#load the dataset as nepalibf
nepalibf = read_csv("datasets/nepalibf.csv")
head(nepalibf)
summary(nepalibf)
table(nepalibf$age_chld)

nepalibf %>% 
  group_by(bf) %>% 
  summarise(meanage=mean(age_chld),
            medianage=median(age_chld),
            iqrage=IQR(age_chld), 
            meanage_m=mean(age_mom),
            medianage_m=median(age_mom),
            iqrage_m=IQR(age_mom),
            mean_p=mean(parity),
            median_p=median(parity),
            iqr_p=IQR(parity))

# create plots for this
f1 = ggplot(data=bf_app)
f1 + geom_boxplot(aes(x=breastfeeding, y=age_chld, color=breastfeeding), size=0.5) + 
  ylab("child age(months)")+ 
  theme_minimal() 
f1 + geom_boxplot(aes(x=breastfeeding, y=age_mom, color=breastfeeding), size=0.5) +
  ylab("age of mom (years)") +
  theme_minimal()
f1 + geom_boxplot(aes(x=breastfeeding, y=parity, color=breastfeeding), size=0.5) +
  ylab("parity") +
  theme_minimal()

#try for less than 2 years olds
bf_app2 = nepalibf %>% 
  filter(age_chld<24) %>% 
  na.omit()

# create plots for this
f2 = ggplot(data=bf_app2)
f2 + geom_boxplot(aes(x=breastfeeding, y=age_chld, color=breastfeeding), size=0.5) + 
  ylab("child age(months)")+ 
  theme_minimal() 
f2 + geom_boxplot(aes(x=breastfeeding, y=age_mom, color=breastfeeding), size=0.5) +
  ylab("age of mom (years)") +
  theme_minimal()
f2 + geom_boxplot(aes(x=breastfeeding, y=parity, color=breastfeeding), size=0.5) +
  ylab("parity") +
  theme_minimal()

#omit NAs
#create binary gender variable, breastfeeding variable
nepalibf = nepalibf %>% 
  na.omit() %>% 
  mutate(gender = if_else(sex_chld==1, "female", "male"), 
         breastfeeding = if_else(bf==1, "yes", "no"))

#create ggplots
g1 = ggplot(data=nepalibf)
g1 + geom_boxplot(aes(x=breastfeeding, y=age_chld, color=breastfeeding), size=0.5) + 
  ylab("child age(months)")+ 
  theme_minimal() 

g1 + geom_boxplot(aes(x=breastfeeding, y=age_mom, color=breastfeeding), size=0.5) +
  ylab("age of mom (years)") +
  theme_minimal()

g1 + geom_boxplot(aes(x=breastfeeding, y=parity, color=breastfeeding), size=0.5) +
  ylab("parity") +
  theme_minimal()

#proportion breastfed by gender of child
CT = xtabs(~ sex_chld + bf, data=nepalibf)
addmargins(CT)
prop.table(CT, margin=1)
addmargins(prop.table(CT, margin=1), margin=2)


#center childs age at the mean 
nepalibf = nepalibf %>%
  mutate(agechld_cen = age_chld - mean(age_chld))

#log regression of breastfeeding status on gender and age of child
m1 = glm(bf ~ sex_chld + agechld_cen, 
         data=nepalibf, 
         family=binomial(link="logit"))
summary(m1)
exp(m1$coefficients)
exp(confint(m1))

#add interaction term
m2 = glm(bf~agechld_cen*sex_chld, 
         data=nepalibf, 
         family=binomial(link="logit"))
summary(m2)

#regress bf on gender and age
m3 = glm(bf~age_chld+factor(gender) , 
         data=nepalibf, 
         family = binomial(link="logit"))

#add the predicted values to the df
nepalibf = nepalibf %>% 
  mutate(phat = predict(m3, type="response"))

#create a scatterplot
g4 = ggplot(data=nepalibf, aes(x=age_chld, y=phat, color=gender, shape=gender))
g4+geom_point()+
  xlab("Child's age in months")+
  ylab("Predicted prevalence of Breast-feeding")+
  theme_minimal()

#Hosmer-Lemeshow goodness of fit test
hos = hoslem.test(nepalibf$bf, nepalibf$phat, g=10)
hos
hos$observed
hos$expected


