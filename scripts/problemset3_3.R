# problem set 3 term 3
# environment
library(tidyverse)
library(survival)
library(survminer)
library(ggfortify)
library(GGally)

# read in dataset lymphoma
setwd("/Users/dehshini/code/R/problem_sets")
lymphoma <- read.csv("./datasets/lymphoma.csv", header = TRUE)
lymphbin  <- read.csv("./datasets/binlymph.csv", header = TRUE)

str(lymphoma)
head(lymphoma)
tail(lymphoma)
View(lymphoma)
summary(lymphoma)
dim(lymphoma)

str(lymphbin)
head(lymphbin)
tail(lymphbin)
View(lymphbin)
summary(lymphbin)
dim(lymphbin)
print(lymphbin)

lymphoma %>%
    filter(days<=14 & days>7) %>%
    group_by(stage) %>%
    summarise(
        n = n()
    )

#plot the survival curve for stages
qplot(
    x = mid_days, y = Survival,
    col = factor(stage, labels = c("Stage 3", "Stage 4")),
    data = lymphbin
) + geom_line() + labs(col = "Cancer Stage")

#use ggplot
ggplot(data = lymphbin, aes(x = mid_days, y = Survival)) +
    geom_point(aes(color = factor(stage, labels = c("Stage 3", "Stage 4")))) +
    geom_line(aes(color = factor(stage, labels = c("Stage 3", "Stage 4")))) +
    labs(
        x = "Time (days)", 
        y = "Survival", 
        col = "Cancer Stage", 
    ) +
    ggtitle("Survival curves by cancer stage") +
    theme(
        plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 15)
    ) +
    theme_bw()

#rename variables
lymphbin <- lymphbin %>%
    mutate(t = mid_days) %>%
    mutate(N = P_Days)

##################
#fit poisson models
#################

#center t at 60,
#add spline to t at 60

lymphbin <- lymphbin %>%
    mutate(t60 = t - 60) %>%
    mutate(t60sp = ifelse(t > 60, t-60, 0))


#modelA, covariate = stage
modelA <- glm(
    D ~ factor(stage),
    offset = log(P_Days),
    data = lymphbin,
    family = poisson(link = "log")
)
summary(modelA)
exp(coefficients(modelA))
AIC(modelA) #56.91

#modelB, covariate = stage + time(centered)
#assumes a linear change in incidencerate with time
#forces a linear relationship between the time bins
modelB <- glm(
    D ~ factor(stage) + t60,
    offset = log(P_Days),
    data = lymphbin,
    family = poisson(link = "log")
)
summary(modelB)
exp(coefficients(modelB))
exp(confint(modelB))
AIC(modelB) #52.87

#modelC, covariate = stage + time(centered) + time_splined_at60
#assumes a linear change in incidencerate with time until 60
#then it allows the slope to vary at 60
modelC <- glm(
    D ~ factor(stage) + t60 + t60sp,
    offset = log(P_Days),
    data = lymphbin,
    family = poisson(link = "log")
)
summary(modelC)
AIC(modelC) #54.59

#modelD, covariate = stage + time(centered) + time_splined_at60 + time*stage + time_splined_at60*stage
#assumes a linear change in incidencerate with time until 60
#then it allows the slope to vary at 60
#then it allows the 
modelD <- glm(
    D ~ factor(stage) + t60 + t60sp + t60*factor(stage) + t60sp*factor(stage),
    offset = log(P_Days),
    data = lymphbin,
    family = poisson(link = "log")
)
summary(modelD)
AIC(modelD) #57.09

AIC(modelA, modelB, modelC, modelD)
#model B has the lowest AIC


###########
#now work with the lymphoma dataset
############

head(lymphoma)

#survival object
lymphoma$survobj <- with(lymphoma, Surv(days, died == 1))

#survival estimates by stage
km.stage <- survfit(
    survobj ~ factor(stage),
    data = lymphoma,
    conf.type = "log-log",
    type = "kaplan-meier",
    conf.int = 0.95
)
summary(km.stage)
km.stage

#plot survival curves
plot(
    km.stage,
    col = c("red", "blue"),
    lty = 1:2,
    lwd = 2,
    xlab = "Time (days)",
    ylab = "Survival probability S(t)",
    main = "Survival curves by cancer stage",
    legend(
        "topright",
        c("Stage 3", "Stage 4"),
        lty = 1:2,
        col = c("red", "blue")
    )
)

#use ggplot to plot survival curves
ggsurvplot(
    km.stage,
    data = lymphoma,
    conf.int = FALSE,
    pval = TRUE,
    risk.table = TRUE,
    risk.table.col = "strata",
    ggtheme = theme_bw(),
    legend = c(0.9, 0.9),
    legend.title = "Cancer Stage",
    legend.labs = c("Stage 3", "Stage 4"),
    xlab = "Time (days)",
    ylab = "Survival probability S(t)",
    title = "Kaplan-Meier survival estimates by cancer stage",
    surv.median.line = "hv"
)

#autoplot
autoplot(km.stage)

#log-rank test
survdiff(survobj ~ factor(stage), data = lymphoma)
#significant p=0.02

#cox model
model1 <- coxph(survobj ~ factor(stage), data = lymphoma, ties = "breslow")
summary(model1)
exp(coefficients(model1)) 
exp(confint(model1))

