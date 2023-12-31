#read in the data as ce621

#create new variable "agegen", to indicate the four age-gender groups.
ce621 = ce621 %>% mutate(agegen=case_when(sex=="Male" & age<=60 ~ "m <= 60",
                                          sex=="Male" & age>60 ~ "m > 60",
                                          sex=="Female" & age<=60 ~ "f <= 60",
                                          sex=="Female" & age>60 ~ "f > 60"))

#inspect the data using boxplot
boxplot(totchg ~ agegen, data = ce621, ylab = "Total charge in dollars")

#summarize totchg according to groups
ce621 %>% 
  group_by(agegen) %>% 
  summarize(obs=n(), 
            mean=mean(totchg),
            median=median(totchg),
            min=min(totchg),
            max=max(totchg),
            sd=sd(totchg)
            )

#linear regression of total charges on the age-gender groups
model1 = lm(totchg ~ as.factor(agegen), data = ce621 )
anova(model1)
summary(model1)

#use general linear model to do it(total charge on age-gender groups)
model2 = glm(totchg ~ as.factor(agegen), data=ce621, family = gaussian(link = "identity"))
anova(model2)
summary(model2)

#boxplot the residuals for age-gender groups
boxplot(model1$residuals ~ ce621$agegen, ylab = "Rediduals")


?ggplot
g = ggplot(data = model1, mapping = aes(x=fitted.values, y=residuals))
g = g+geom_boxplot()

qplot(x=model1$fitted.values, y=model1$residuals, xlab="Predicted values", ylab="Residuals")

qplot(x=jitter(model1$fitted.values), y=model1$residuals, xlab="Predicted values", ylab="Residuals")

#create new column for log of totchg 
ce621 = ce621 %>% mutate(logtotchg=log10(totchg))
#boxplot the log of totchg for the different groups
boxplot(logtotchg ~ agegen, data = ce621, ylab = "log of total charge")

model3 = lm(logtotchg ~ as.factor(agegen), data = ce621)

boxplot(model3$residuals ~ ce621$agegen, ylab = "logResiduals")


#function to obtain regression coefficients. this will be used by the boot function.
bs = function(formula, data, indices) 
{ d = data[indices,] # allows boot to select sample
  fit = lm(formula, data=d)
  return(coef(fit))
}

#how else could i have gotten the coefficients of the regression with a function?
regcoef = function(formula, data, i)
{
  fitline = lm(formula, data = data[i,])
  return(coefficients(fitline))
}

# bootstrapping with 250 replications
results = boot(data=ce621, statistic=bs, R=250,
               formula=totchg~agegen)

#bootstrap with the new formula as the statistic and with 1000 replicates.
results2 = boot(data=ce621, statistic = regcoef, R=1000, formula=totchg~agegen)

# view results
results
results2
#both results very similar

# get 95% confidence intervals from the bootstrap
boot.ci(results, type="norm", index=1) # intercept (f <=60)
boot.ci(results, type="norm", index=2) # f >60
boot.ci(results, type="norm", index=3) # m <=60
boot.ci(results, type="norm", index=4) # m >60

# get 95% confidence intervals from the regression model
confint(model1)


#use gglot to graph the linear model
g = ggplot(data = ce621)
g = g+
  geom_point(aes(x=agegen, y=residuals))+
  geom_smooth(method = "lm", se = FALSE)+
  lab(title = "Residuals")
