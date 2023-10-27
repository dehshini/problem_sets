#load tidyverse
##problem 1
#open the data file and label it ps1

#group data
ps1.1 = filter(ps1, group==1)
ps1.2 = filter(ps1, group==2)

#create stem and leaf plot based on groups(remember to choose your scale if you want)
stem(ps1.1$deaths)
stem(ps1.2$deaths)

#create box plot for the 
boxplot(deaths ~ group, data=ps1)

#problem 2
#open the data file and label it ce621

#group data, create variables for sexes
ce621.male = filter(ce621, sex=="Male")
ce621.female = filter(ce621, sex=="Female")

#create stem and leaf
stem(ce621.male$totchg)
stem(ce621.female$totchg)

#create summary tables 
#males using totchg
summary(ce621.male$totchg)
sd(ce621.male$totchg)
quantile(ce621.male$totchg, c(0,.1,.25,.5,.75,.9,1))
#females using totchg
summary(ce621.female$totchg)
sd(ce621.female$totchg)
quantile(ce621.female$totchg, c(0,.1,.25,.5,.75,.9,1))

#open the entire dataset ce621entire and label it ce621e

#filter by year(1995) and label it ce621e95
ce621e95 = filter(ce621e, year==1995)

#create the group "agecat" from the column "age"
ce621e95$agecat = cut(ce621e95$age, c(0, 50, 64, 100), right=TRUE, labels=c("<=50","51-64",">=65"))

#create the box plot 
boxplot(totchg ~ sex + agecat, data=ce621e95, names=c("F <= 50", "M <= 50","F 51-64", "M 51-64", "F >=65", "M >=65"))

#create a new box plot with log scale
ce621e95$log10chg = log10(ce621e95$totchg)
boxplot(log10chg ~ sex + agecat, data=ce621e95, names=c("F <= 50", "M <= 50","F 51-64", "M 51-64", "F >=65", "M >=65"))


