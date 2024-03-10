# problem set 4, term 3
# environment
library(tidyverse)

# read in dataset lymphoma
setwd("/Users/dehshini/code/R/problem_sets")

framdata <- read_csv("./datasets/FraminghamPS4bin.csv")

#explore the data
str(framdata)
head(framdata)
tail(framdata)
View(framdata)
summary(framdata)
unique(framdata$tbin)

#check for missing values
sum(is.na(framdata))

###
#641 rows, 13 columns
#568 missing values
# there are 5 time bins: 0, 1825, 3650, 5475, 7300


# convert variables to factor
factors <- c("gender", "cursmoke", "agecat", "diabetes", "bpmeds", "bmicat")

framdata <- framdata %>%
    mutate(
        across(all_of(factors), factor)
    )

str(framdata)



#build models
#poisson regression
model1 <- glm(D ~ gender,
    offset = log(Y),
    data = framdata,
    family = poisson(link = "log")
)
summary(model1)
exp(coefficients(model1))
AIC(model1)


# Pearson chi-square goodness-of-fit test (like poisgof in Stata)
X2 <- sum(residuals(model1, type = "pearson")^2)
X2
df <- model1$df.residual
df
pval <- 1 - pchisq(X2, df)
pval

# Negative binomial regression
library(MASS)

model2 <- glm.nb(D ~ gender + offset(log(Y)), data = framdata)
summary(model2)

