#balt621 dataset
#load tidyverse, load the dataset
library(tidyverse)
balt621 = read.csv("/Users/dehshini/Desktop/datasets/balt621.csv", header=TRUE)


balt621 %>%
  group_by(season) %>%
  summarize(n_pm10 = sum(!is.na(pm10)), mean_pm10 = mean(pm10, na.rm=TRUE),
            n_mortality = sum(!is.na(death)), mean_mortality = mean(death))

#stratify days into 5 pollution strata for each season

#for winter
pm10winter <- filter(balt621, season=="Winter")
quintilesWinter = quantile(pm10winter$pm10, c(0,.2,.4,.6,.8,1), na.rm=TRUE)
pm10winter$pm_group = cut(pm10winter$pm10, breaks=quintilesWinter, labels=1:5)
table(pm10winter$pm_group)

#for summer
pm10summer <- filter(balt621, season=="Summer")
quintilesSummer = quantile(pm10summer$pm10, c(0,.2,.4,.6,.8,1), na.rm=TRUE)
pm10summer$pm_group = cut(pm10summer$pm10, breaks=quintilesSummer, labels=1:5)
table(pm10summer$pm_group)

#for autumn
pm10autumn <- filter(balt621, season=="Autumn")
quintilesAutumn = quantile(pm10autumn$pm10, c(0,.2,.4,.6,.8,1), na.rm=TRUE)
pm10autumn$pm_group = cut(pm10autumn$pm10, breaks=quintilesAutumn, labels=1:5)
table(pm10autumn$pm_group)

#spring
pm10spring <- filter(balt621, season=="Spring")
quintilesSpring = quantile(pm10spring$pm10, c(0,.2,.4,.6,.8,1), na.rm=TRUE)
pm10spring$pm_group = cut(pm10spring$pm10, breaks=quintilesSpring, labels=1:5)
table(pm10spring$pm_group)


#for each seasonal strata, calculate the mean mortality in the lowest and highest of the five pollution strata. 

#for the lowest stratum in winter
pm10winter %>% filter(pm_group==1) %>% summarize(mean=mean(death), sd=sd(death), n=n())
#for the highest stratum in winter
pm10winter %>% filter(pm_group==5) %>% summarize(mean=mean(death), sd=sd(death), n=n())

#for lowest in summer
pm10summer %>% filter(pm_group==1) %>% summarize(mean=mean(death), sd=sd(death), n=n())
#for highest in summer
pm10summer %>% filter(pm_group==5) %>% summarize(mean=mean(death), sd=sd(death), n=n())

#for lowest in spring
pm10spring %>% filter(pm_group==1) %>% summarize(mean=mean(death), sd=sd(death), n=n())
#for highest in spring
pm10spring %>% filter(pm_group==5) %>% summarize(mean=mean(death), sd=sd(death), n=n())

#for lowest in autumn
pm10autumn %>% filter(pm_group==1) %>% summarize(mean=mean(death), sd=sd(death), n=n())
#for highest in autumn
pm10autumn %>% filter(pm_group==5) %>% summarize(mean=mean(death), sd=sd(death), n=n())

#compare means of lowest and highest pollution days. 
#run t test to check null hypothesis that there is a no difference in means
pm10winter.15 = pm10winter %>% filter(pm_group==1 | pm_group==5) 
t.test(death ~ pm_group, data=pm10winter.15, var.equal=FALSE)

#spring
pm10spring.15 = pm10spring %>% filter(pm_group==1 | pm_group==5) 
t.test(death ~ pm_group, data=pm10spring.15, var.equal=FALSE)

#summer
pm10summer.15 = pm10summer %>% filter(pm_group==1 | pm_group==5) 
t.test(death ~ pm_group, data=pm10summer.15, var.equal=FALSE)

#autumn
pm10autumn.15 = pm10autumn %>% filter(pm_group==1 | pm_group==5) 
t.test(death ~ pm_group, data=pm10autumn.15, var.equal=FALSE)

#new code
pm10winter %>% filter(pm_group==1) %>% summarize(mean=mean(death), sd=sd(death), n=n()) %>% summarize(lower=mean-1.98*sd/sqrt(n), upper=mean+1.98*sd/sqrt(n))

var.test()




