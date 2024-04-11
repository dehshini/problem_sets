# problem set 4, term 3
# environment
library(tidyverse)
library(gtsummary)
library(janitor)

# read in dataset lymphoma
setwd("/Users/dehshini/code/R/problem_sets")

framdata <- read_csv("./datasets/FraminghamPS4bin.csv")

#explore the data
str(framdata)
View(framdata)
summary(framdata)
unique(framdata$tbin)

#check for missing values
sum(is.na(framdata))

sum(framdata$D)
dim(framdata)

#not sure about this. check!!!
framdata  %>% 
    group_by(factor(tbin)) %>%
    summarise(
        n = n(), #number of observations
        n_D = max(D), #number of deaths
        t_persontime = max(Y), #total person time
        inc_rate = round(n_D / t_persontime, 6) #incidence rate
    ) %>%
    adorn_totals()

###
#641 rows, 13 columns
#568 missing values
# there are 5 time bins: 0, 1825, 3650, 5475, 7300


# convert some variables to factor

framdata <- framdata %>%
    mutate(
        gender = factor(gender, levels = c(0, 1), labels = c("Male", "Female")),
        cursmoke = factor(cursmoke, levels = c(0, 1), labels = c("No", "Yes")),
        diabetes = factor(diabetes, levels = c(0, 1), labels = c("No", "Yes")),
        bpmeds = factor(bpmeds, levels = c(0, 1), labels = c("No", "Yes")),
        bmicat = factor(bmicat, levels = c(1, 2, 3, 4), labels = c("Underweight", "Normal", "Overweight", "Obese")),
        #tbin = factor(tbin, levels = c(0, 1825, 3650, 5475, 7300), labels = c("0-5yrs", "5-10yrs", "10-15yrs", "15-20yrs", "20-25yrs")),
        agecat = factor(agecat, levels = c(1, 2, 3, 4), labels = c("30-39", "40-49", "50-59", "60-69"))
        )

str(framdata)
summary(framdata)


#build gt table
labels_for_table <- list(
    gender ~ "Gender",
    agecat ~ "Age",
    diabetes ~ "Diagnosed Diabetes",
    cursmoke ~ "Smoking",
    bpmeds ~ "BP treatment",
    bmicat ~ "BMI",
    tbin ~ "Time Bin"
)

tbl_summary(
    data=framdata,
    by = tbin,
    missing = "ifany",
    include = c("agecat", "diabetes", "cursmoke", "bpmeds", "bmicat", "gender"),
    label = list(
        gender ~ "Gender",
        agecat ~ "Age",
        diabetes ~ "Diagnosed Diabetes",
        cursmoke ~ "Smoking",
        bpmeds ~ "BP treatment",
        bmicat ~ "BMI",
        tbin ~ "Time Bin"
    ),
    digits = all_continuous() ~ 2,
) %>% 
    add_overall() %>%
    modify_caption("**Table 1. Baseline Characteristics Stratified by Time Bin**") %>%
    modify_spanning_header(all_stat_cols() ~ "**Time Bin**") %>%
    as_gt()
    #gt::gtsave(filename = "table12.docx")

#build models
#poisson regression

#############################
#univariate models

#model1, covariate = gender
model1cons <- glm(D ~ 1,
    offset = log(Y),
    data = framdata,
    family = poisson(link = "log")
)
summary(model1cons)
exp(coefficients(model1cons))

#model1, covariate = gender
model1g <- glm(D ~ gender,
    offset = log(Y),
    data = framdata,
    family = poisson(link = "log")
)
summary(model1g)
exp(coefficients(model1g))

#model1smoke
model1s <- glm(D ~ cursmoke,
    offset = log(Y),
    data = framdata,
    family = poisson(link = "log")
)
summary(model1s)
exp(coefficients(model1s))

#model1bmi
model1b <- glm(D ~ bmicat,
    offset = log(Y),
    data = framdata,
    family = poisson(link = "log")
)
summary(model1b)
exp(coefficients(model1b))

#model1age
model1a <- glm(D ~ agecat,
    offset = log(Y),
    data = framdata,
    family = poisson(link = "log")
)
summary(model1a)
exp(coefficients(model1a))

#model1diabetes
model1d <- glm(D ~ diabetes,
    offset = log(Y),
    data = framdata,
    family = poisson(link = "log")
)
summary(model1d)
exp(coefficients(model1d))

#model1bp
model1bp <- glm(D ~ bpmeds,
    offset = log(Y),
    data = framdata,
    family = poisson(link = "log")
)
summary(model1bp)
exp(coefficients(model1bp))


##########################
#multivariate models

#model2, covariate = gender + time
model2 <- glm(D ~ gender + tbin,
    offset = log(Y),
    data = framdata,
    family = poisson(link = "log")
)
summary(model2)
exp(coefficients(model2))
AIC(model2) #2622

model2b <- glm(D ~ gender + factor(tbin),
    offset = log(Y),
    data = framdata,
    family = poisson(link = "log")
)
summary(model2b)
exp(coefficients(model2))
AIC(model2) #2621

#model3, covariate = gender + time + agecat
model3 <- glm(D ~ gender + tbin + agecat,
    offset = log(Y),
    data = framdata,
    family = poisson(link = "log")
)
summary(model3)
exp(coefficients(model3))
AIC(model3) #1847

#model5, covariate = gender + time + agecat + bmicat
model5 <- glm(D ~ gender + tbin + agecat + bmicat,
    offset = log(Y),
    data = framdata,
    family = poisson(link = "log")
)
summary(model5)
exp(coefficients(model5))
AIC(model5) #1828

#model6, covariate = gender + time + agecat + bmicat + diabetes
model6 <- glm(D ~ gender + tbin + agecat + bmicat + diabetes,
    offset = log(Y),
    data = framdata,
    family = poisson(link = "log")
)
summary(model6)
exp(coefficients(model6))
AIC(model6) #1782

#model7, covariate = gender + time + agecat + bmicat + diabetes + bpmeds
model7 <- glm(D ~ gender + tbin + agecat + bmicat + diabetes + bpmeds,
    offset = log(Y),
    data = framdata,
    family = poisson(link = "log")
)
summary(model7)
exp(coefficients(model7))
AIC(model7) #1750

#model8, all covariates and interactions with tbin
model8 <- glm(D ~ gender + tbin + agecat + bmicat + diabetes + bpmeds + tbin*gender + tbin*agecat + tbin*bmicat + tbin*diabetes + tbin*bpmeds,
    offset = log(Y),
    data = framdata,
    family = poisson(link = "log")
)
summary(model8)
exp(coefficients(model8))
AIC(model8) #1756

#model9, all covariates with disjoint time bins
model9 <- glm(D ~ gender + factor(tbin) + agecat + bmicat + diabetes + bpmeds + cursmoke,
    offset = log(Y),
    data = framdata,
    family = poisson(link = "log")
)
summary(model9)
exp(coefficients(model9))
AIC(model9) #1706


#model10, all covariates with disjoint time bins and interactions
model10 <- glm(D ~ gender + factor(tbin) + agecat + bmicat + diabetes + bpmeds + cursmoke + factor(tbin)*gender + factor(tbin)*agecat + factor(tbin)*bmicat + factor(tbin)*diabetes + factor(tbin)*bpmeds + factor(tbin)*cursmoke,
    offset = log(Y),
    data = framdata,
    family = poisson(link = "log")
)
summary(model10)
exp(coefficients(model10))
AIC(model10) #1748


AIC(model1, model2, model3, model5, model6, model7, model8, model9, model10)

#choose model 9. it has the lowest AIC

# poisson makes some assumptions about the data
# it assumes the data is not overdispersed, thus mean=variance
# the rate is constant within time bins
# independence of the observations 

#check for overdispersion
#model9$deviance / model9$df.residual # 1.04


# Pearson chi-square goodness-of-fit test (like poisgof in Stata)
X2 <- sum(residuals(model9, type = "pearson")^2)
X2
df <- model9$df.residual
df

X2/df

pval <- 1 - pchisq(X2, df)
pval


# Negative binomial regression
library(MASS)
library(pscl)

#negative binomial regression
model1n <- glm.nb(D ~ gender + offset(log(Y)), data = framdata)
summary(model1n)

# covariate = all covariates with disjoint time bins
model2n <- glm.nb(
    D ~ gender + agecat + bmicat + diabetes + bpmeds + cursmoke + factor(tbin) +
    offset(log(Y)),
    data = framdata
    )
summary(model2n)

#do one for model9
model9n <- glm.nb(
    D ~ gender + factor(tbin) + agecat + diabetes + bpmeds + cursmoke +
    offset(log(Y)),
    data = framdata
)
summary(model9n)
exp(coefficients(model9n))
exp(confint(model9n))

# Calculate the 2 * diff in log-likelihood for NB (modelB) compared to Poisson(modelA)
LRTstat <- 2 * (logLik(model9n) - logLik(model9))
df <- 1
p.val <- 1 - pchisq(LRTstat, df)
paste0(
    "LRT test statistic = ", round(LRTstat, 2), ", df = ", df, ", p-value = ", round(pval, 3)
)



#try quasipoisson
model9q <- glm(D ~ gender + factor(tbin) + agecat + diabetes + bpmeds + cursmoke,
    offset = log(Y),
    data = framdata,
    family = quasipoisson(link = "log")
)
summary(model9q)

