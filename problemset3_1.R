#working with the 2019class621.csv file as class621

#create a boxplot of the heights in the data 
boxplot(class621$height)

#create a box plot of the weights in the data
boxplot(class621$weight)

#filter to remove the extreme outliers
class621norm = filter(class621, weight < 250)
boxplot(class621norm$weight)

#create a new table with the calculated BMI added at the end 
class621bmi = mutate(class621, bmi=(weight/height^2)*704.5 )

class621filtered = filter(class621bmi, bmi < 40)

#create new tables for males and females from the bmi table. assume male=1, female=2
class621filtered.F = filter(class621filtered, gender==2)
class621filtered.M = filter(class621filtered, gender==1)

#create stem and leaf plots for males and females
stem(class621filtered.M$bmi)
stem(class621filtered.F$bmi)

#summarize the table of male(1) BMIs
summary(class621filtered.M$bmi)
sd(class621filtered.M$bmi)

#summarize the table of female(2) BMIs
summary(class621filtered.F$bmi)
sd(class621filtered.F$bmi)

#do the quantiles 
quantile(class621filtered.F$bmi, c(.005, .025, .25, .75, .975, .995))
quantile(class621filtered.M$bmi, c(.005, .025, .25, .75, .975, .995))

#calculate Probabilities
#for males, Pr(X<25) is. note: this is code to calculate Pr under normal distribution.
pnorm(25, mean = 24.97, sd = 4.084464, lower.tail = TRUE)

#for females, 
pnorm(30, mean = 22.76, sd = 3.704175, lower.tail = FALSE)

#calculate actual probabilities 
class621filtered$bmicatg = cut(class621filtered$bmi, breaks=c(0,25,30,60))
CT = table(class621filtered$bmicatg, class621filtered$sex)
addmargins(CT)
prop.table(CT, margin=2)
?cut

#create qq plots
qqnorm(class621filtered$bmi)
#add the line y=x
qqline(class621filtered$bmi)
#add gridlines at Q1,Q2,Q3
abline(h=quantile(class621filtered$bmi, c(.25,.5,.75), na.rm=TRUE), lty=2) 
abline(v=qnorm(c(.25,.5,.75)), lty=2)

#make qq plot for females and males
qqnorm(class621filtered.F$bmi) 
qqline(class621filtered.F$bmi)
abline(h=quantile(class621filtered.F$bmi, c(.25,.5,.75), na.rm=TRUE), lty=2) 
abline(v=qnorm(c(.25,.5,.75)), lty=2)

qqnorm(class621filtered.M$bmi)
qqline(class621filtered.M$bmi)
abline(h=quantile(class621filtered.M$bmi, c(.25,.5,.75), na.rm=TRUE), lty=2) 
abline(v=qnorm(c(.25,.5,.75)), lty=2)

qqplot(class621filtered.F$bmi, class621filtered.M$bmi, xlim=c(15,40), ylim=c(15,40), xlab = "females", ylab = "males")
#make qq plot for males(vertical axis) vs females(horizontal)
qqplot(class621filtered.F$bmi, class621filtered.M$bmi, xlim=c(5,40), ylim=c(5,40)) 
abline(a=0, b=1)
qqplot()

