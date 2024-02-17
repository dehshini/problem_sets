# problem set 2 term 3
# environment
library(tidyverse)
library(survival)
library(ggfortify)
library(GGally)

# read in dataset pbctrial
setwd("/Users/dehshini/code/R/problem_sets")
pbctrial <- read.csv("./datasets/pbctrial.csv", header = TRUE)
str(pbctrial)
head(pbctrial)
tail(pbctrial)
View(pbctrial)
summary(pbctrial)

#check for missing values
sum(is.na(pbctrial))

#explore the data
pbctrial %>% 
    group_by(drug) %>% 
    summarise(
        n = n(),
        mean_age = mean(ageyr),
        sd_age = sd(ageyr),
        mean_bil = mean(bil),
        sd_bil = sd(bil),
        nmale = sum(sex == "Male"),
        pmale = sum(sex == "Male") / n,
        nfemale = sum(sex == "Female"),
        pfemale = sum(sex == "Female") / n,
        ndeath = sum(death == 1),
        pdeath = sum(death == 1) / n,
        med_survtime = median(survyr),
        hist1 = sum(histo == 1),
        p_hist1 = sum(histo == 1) / n,
        hist2 = sum(histo == 2),
        p_hist2 = sum(histo == 2) / n,
        hist3 = sum(histo == 3),
        p_hist3 = sum(histo == 3) / n,
        hist4 = sum(histo == 4),
        p_hist4 = sum(histo == 4) / n
    )

boxplot(death~drug, data = pbctrial)
boxplot(pbctrial$bil)
ggpairs(pbctrial1[, c("sex", "bil", "death", "drug", "histo")])

#check for normal distribution of continuous variables
hist(pbctrial$ageyr)
hist(pbctrial$bil)

#set survival object
pbctrial$survival <- with(pbctrial, Surv(survyr, death == 1))

#estimate survival curves
km.all <- survfit(
    survival ~ 1,
    data = pbctrial,
    conf.type = "log-log",
    conf.int = 0.95,
    type = "kaplan-meier"
    )

km.all
summary(km.all)


#estimate survival curves for drug group
km.drug <- survfit(
    survival ~ drug,
    data = pbctrial,
    conf.type = "log-log",
    type = "kaplan-meier",
    conf.int = 0.95
)

#plot survival curves
#use ggplot2
g <- ggplot(km.drug)
g +
geom_line(aes(x = time, y = surv, color = strata)) +
theme_minimal()

#autoplot
autoplot(km.all)
autoplot(km.drug)

#check if survival curves are similar
# log rank test for equality of survivor functions
survdiff(survival ~ drug, data = pbctrial)

# complimentary log-log plot
plot(
    km.drug,
    fun = "cloglog",
    col=c("red", "blue"),
    lwd=2
)

#fit cox models

# model1. survival on drug
model1 <- coxph(survival ~ drug, data = pbctrial)
summary(model1) 

# model2. survival on sex, bilirubin level, and histological stage
model2 <- coxph(survival ~ sex + bil + factor(histo), data = pbctrial)
summary(model2)

#model3. survival on drug, sex, bilirubin level, and histological stage
model3 <- coxph(survival ~ drug + sex + bil + factor(histo), data = pbctrial)
summary(model3)

#model4. survival on drug, sex, bilirubin level, and histological stage, with strata
model4 <- coxph(survival ~ drug + sex + bil + factor(histo) + strata(drug), data = pbctrial)
summary(model4)
