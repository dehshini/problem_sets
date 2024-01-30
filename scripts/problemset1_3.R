#problem set 1 term 3
#environment
library(tidyverse)

#read in dataset nepal621
setwd("/Users/dehshini/code/R/problem_sets")
nepal621  <-  read.csv("./datasets/nepal621.csv", header=TRUE)
str(nepal621)
head(nepal621)
tail(nepal621)

#Suppose you are interested in choosing an appropriate sample size
# for estimating the 16- month mortality rate for children younger
# than 3 years of age in a developing country in
# which vitamin A supplementation is not currently available.

#subset children under 3 years
nepal_under3  <-  nepal621 %>%
    filter(age == "<1" | age == "1-2")

str(nepal_under3)

#crosstabulate
ct <- table(nepal_under3$trt, nepal_under3$status)
addmargins(ct)

# proportions of status for each treatment
ct_prop <- prop.table(ct, margin = 1)
addmargins(ct_prop)

#the proprtion of children who died in the placebo group
# is 0.0294 while the proportion of children who died in
# the vitamin A group is 0.0245

#note: 
#for the ct_prop table,
#row 1 is placebo, row 2 is vitamin A, col 2 is risk of death
#p1 is the risk of death in the placebo group
#p2 is the risk of death in the vitamin A group

#calculate sample size using the nepal proportions
#power 80%, alpha = 0.05
power.prop.test(
    n = NULL,
    p1 = round(ct_prop[[1,2]], 4), 
    p2 = round(ct_prop[[2,2]], 4), 
    sig.level = 0.05, 
    power = 0.8, 
    alternative = "two.sided"
)
#with proportion in placebo 0.5% lower
power.prop.test(
    p1 = round(ct_prop[[1, 2]], 4) - 0.005,
    p2 = round(ct_prop[[2, 2]], 4),
    sig.level = 0.05,
    power = 0.8,
    alternative = "two.sided"
)
# with proportion in placebo 0.5% higher
power.prop.test(
    p1 = round(ct_prop[[1, 2]], 4) + 0.005,
    p2 = round(ct_prop[[2, 2]], 4),
    sig.level = 0.05,
    power = 0.8,
    alternative = "two.sided"
)

# with risk in placebo at 1.2 times risk in vitamin A
power.prop.test(
    p1 = round(ct_prop[[2, 2]] * 1.2, 4),
    p2 = round(ct_prop[[2, 2]], 4),
    sig.level = 0.05,
    power = 0.8,
    alternative = "two.sided"
)

# with risk in placebo at 1.5 times risk in vitamin A
power.prop.test(
    p1 = round(ct_prop[[2, 2]] * 1.5, 4),
    p2 = round(ct_prop[[2, 2]], 4),
    sig.level = 0.05,
    power = 0.8,
    alternative = "two.sided"
)
# with risk in placebo at 1.75 times risk in vitamin A
power.prop.test(
    p1 = round(ct_prop[[2, 2]] * 1.75, 4),
    p2 = round(ct_prop[[2, 2]], 4),
    sig.level = 0.05,
    power = 0.8,
    alternative = "two.sided"
)

########################
# power 90%, alpha = 0.05
########################

# calculate sample size using the nepal proportions
power.prop.test(
    p1 = round(ct_prop[[1, 2]], 4),
    p2 = round(ct_prop[[2, 2]], 4),
    sig.level = 0.05,
    power = 0.9,
    alternative = "two.sided"
)
#with proportion in placebo 0.5% lower
power.prop.test(
    p1 = round(ct_prop[[1, 2]], 4) - 0.005,
    p2 = round(ct_prop[[2, 2]], 4),
    sig.level = 0.05,
    power = 0.9,
    alternative = "two.sided"
)
# with proportion in placebo 0.5% higher
power.prop.test(
    p1 = round(ct_prop[[1, 2]], 4) + 0.005,
    p2 = round(ct_prop[[2, 2]], 4),
    sig.level = 0.05,
    power = 0.9,
    alternative = "two.sided"
)

# with risk in placebo at 1.2 times risk in vitamin A
power.prop.test(
    p1 = round(ct_prop[[2, 2]] * 1.2, 4),
    p2 = round(ct_prop[[2, 2]], 4),
    sig.level = 0.05,
    power = 0.9,
    alternative = "two.sided"
)

# with risk in placebo at 1.5 times risk in vitamin A
power.prop.test(
    p1 = round(ct_prop[[2, 2]] * 1.5, 4),
    p2 = round(ct_prop[[2, 2]], 4),
    sig.level = 0.05,
    power = 0.9,
    alternative = "two.sided"
)
# with risk in placebo at 1.75 times risk in vitamin A
power.prop.test(
    p1 = round(ct_prop[[2, 2]] * 1.75, 4),
    p2 = round(ct_prop[[2, 2]], 4),
    sig.level = 0.05,
    power = 0.9,
    alternative = "two.sided"
)
