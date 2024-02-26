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
addmargins(table(pbctrial$drug, pbctrial$sex))

#check for missing values
sum(is.na(pbctrial))

#explore the data
#summarize the data
table1 <- pbctrial %>% 
    group_by(drug) %>% 
    summarise(
        n = n(),
        mean_age = mean(ageyr),
        sd_age = sd(ageyr),
        mean_bil = mean(bil),
        sd_bil = sd(bil),
        nmale = sum(sex == "Male"),
        pmale = sum(sex == "Male") / n * 100,
        nfemale = sum(sex == "Female"),
        pfemale = sum(sex == "Female") / n * 100,
        ndeath = sum(death == 1),
        pdeath = sum(death == 1) / n * 100,
        med_survtime = median(survyr),
        hist1 = sum(histo == 1),
        p_hist1 = sum(histo == 1) / n * 100,
        hist2 = sum(histo == 2),
        p_hist2 = sum(histo == 2) / n * 100,
        hist3 = sum(histo == 3),
        p_hist3 = sum(histo == 3) / n * 100,
        hist4 = sum(histo == 4),
        p_hist4 = sum(histo == 4) / n * 100
    )

#summarize the agecat variable within druggroup and give proportions
pbctrial %>% 
    group_by(drug, agecat) %>% 
    summarise(
        n = n()
    )

#transpose table
table2 <- t(table1[, -1])
table2 <- as.data.frame(table2)
table2 <- as_tibble(table2)
colnames(table2) <- c("Placebo", "DPCA")
table2
str(table2)

#visualize data
ggpairs(pbctrial1[, c("ageyr", "sex", "bil", "death", "drug", "histo")])

#check for normal distribution of continuous variables
hist(pbctrial$ageyr)
hist(pbctrial$bil)

#categorize bil
pbctrial <- pbctrial %>% 
    mutate(bil_gt_3 = ifelse(bil < 3.45, 0, 1))

#count bil_gt_3 numbers
pbctrial %>% 
    group_by(drug, bil_gt_3) %>% 
    summarise(
        n = n()
    )


#set survival object
pbctrial$survival <- with(pbctrial, Surv(survyr, death == 1))

#estimate survival curve, overall
km.all <- survfit(
    survival ~ 1,
    data = pbctrial,
    conf.type = "log-log",
    conf.int = 0.95,
    type = "kaplan-meier"
)
summary(km.all)

#estimate survival curves for drug group
km.drug <- survfit(
    survival ~ drug,
    data = pbctrial,
    conf.type = "log-log",
    type = "kaplan-meier",
    conf.int = 0.95
)
summary(km.drug)

#estimate survival curves for sex group
km.sex <- survfit(
    survival ~ sex,
    data = pbctrial,
    conf.type = "log-log",
    type = "kaplan-meier",
    conf.int = 0.95
)
summary(km.sex)

#estimate survival curves for bil group
#may not make sense to include
km.bil <- survfit(
    survival ~ bil_gt_3,
    data = pbctrial,
    conf.type = "log-log",
    type = "kaplan-meier",
    conf.int = 0.95
)
summary(km.bil)

#estimate survival curves for histo group
km.histo <- survfit(
    survival ~ factor(histo),
    data = pbctrial,
    conf.type = "log-log",
    type = "kaplan-meier",
    conf.int = 0.95
)
summary(km.histo)

#estimate survival curves for age group
km.agecat  <- survfit(
    survival ~ agecat,
    data = pbctrial,
    conf.type = "log-log",
    type = "kaplan-meier",
    conf.int = 0.95
)
summary(km.agecat)

#plot survival curves
#autoplot
autoplot(km.all)
autoplot(km.drug) +
    labs(
        title = "Survival curves by drug group",
        x = "Time (years)",
        y = "Survival probability"
    ) +
        theme(plot.title = element_text(hjust = 0.5))

ggsurvplot(km.drug,
    legend = "right",
    legend.title = "Drug",
    legend.labs = c("Placebo", "DPCA")
)




autoplot(km.sex)
autoplot(km.bil) #now making sense after categorizing.
autoplot(km.histo)
autoplot(km.agecat)

#create the plot for the drug group
ggsurvplot(
    km.drug, 
    data = pbctrial, 
    conf.int = TRUE,
    pval = TRUE,
    legend = "bottom",
    legend.title = "Drug",
    legend.labs = c("Placebo", "DPCA"),
    )

km.drugadj <- survfit(
    survival ~ agecat+sex+factor(histo)+drug+bil_gt_3,
    data = pbctrial,
    conf.type = "log-log",
    type = "kaplan-meier",
    conf.int = 0.95,
    strata = drug
)
summary(km.agecat)




#check if survival curves are similar
# log rank test for equality of survivor functions
survdiff(survival ~ drug, data = pbctrial) # not significant, p=0.7
survdiff(survival ~ sex, data = pbctrial) ## significant p=0.04
survdiff(survival ~ factor(histo), data = pbctrial) #significant p=1e-11
survdiff(survival ~ agecat, data = pbctrial) #significant p=0.002
survdiff(survival ~ bil_gt_3, data = pbctrial)#significant p=<2e-16


# complimentary log-log plots
plot(
    km.drug,
    fun = "cloglog",
    col=c("red", "blue"),
    lwd=2
)
#looks proportional

plot(
    km.sex,
    fun = "cloglog",
    col = c("red", "blue"),
    lwd = 2
)
#kinda proportional

plot(
    km.histo,
    fun = "cloglog",
    col = c("red", "blue", "green", "orange"),
    lwd = 2
)
#weird looking

plot(
    km.agecat,
    fun = "cloglog",
    col = c("red", "blue", "green"),
    lwd = 2
)
#looks proportional
plot(
    km.bil,
    fun = "cloglog",
    col = c("red", "blue"),
    lwd = 2
)
#looks proportional

###############
#fit cox models
###############
# model1. survival on drug
model1 <- coxph(survival ~ drug, data = pbctrial)
summary(model1)
tidy(model1) #not significant

model1b <- coxph(survival ~ sex, data = pbctrial)
summary(model1b)
tidy(model1b) #significant

model1c <- coxph(survival ~ factor(histo), data = pbctrial)
summary(model1c)
tidy(model1c) #significant

model1d <- coxph(survival ~ agecat, data = pbctrial)
summary(model1d)
tidy(model1d) #significant

model1e <- coxph(survival ~ factor(bil_gt_3), data = pbctrial)
summary(model1e)
tidy(model1e) #significant

# model2. survival on sex, bilirubin level, and histological stage
model2 <- coxph(survival ~ drug + sex + bil_gt_3 + factor(histo), data = pbctrial)
summary(model2)
tidy(model2)
AIC(model2) #1168

#model3. survival on drug, sex, bilirubin level, and histological stage
model3 <- coxph(survival ~ drug + sex + bil_gt_3 + factor(histo) + agecat, data = pbctrial)
summary(model3)
tidy(model3)
AIC(model3) #1165

#model4. survival on drug, sex, bilirubin level, and histological stage
model4 <- coxph(survival ~ drug + bil_gt_3, data = pbctrial)
summary(model4)
tidy(model4)
AIC(model4) #1199

#select model 3 based on AIC

