#problem set 1 term 3
#environment
library(tidyverse)

#read in dataset nepal621
setwd("/Users/dehshini/code/R/problem_sets")
nepal621  <-  read.csv("./datasets/nepal621.csv", header=TRUE)
str(nepal621)
head(nepal621)
tail(nepal621)
