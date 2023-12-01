#import dataset as nepal
#view
View(nepal)

#Create a table that displays the numbers of deaths and numbers of survivors for the
#vitamin A and control groups separately for the six age-by-sex strata.
nepal %>%
  group_by(trt, sex, age) %>%
  summarize(N_Alive = sum(status=="Alive"),
            Perc_Alive = round(N_Alive/n(),4)*100,
            N_Died = sum(status=="Died"),
            Perc_Died = round(N_Died/n(),4)*100,
            Total=n())

#proportion of children who died in the vitamin A group and in the control group
nepal %>%
  group_by(trt) %>%
  summarize(N_Alive = sum(status=="Alive"),
            Perc_Alive = round(N_Alive/n(),4)*100,
            N_Died = sum(status=="Died"),
            Perc_Died = round(N_Died/n(),4)*100,
            Total=n())

#Calculate a 95% confidence interval for each mortality rate
nepal %>%
  group_by(trt) %>%
  summarize(N_Alive = sum(status=="Alive"),
            p_Alive = N_Alive/n(),
            N_Died = sum(status=="Died"),
            p_Died = N_Died/n(),
            Total = n(),
            se_Died = sqrt(p_Died *(1-p_Died)/Total),
            CI_L = p_Died - 1.96*se_Died,
            CI_U = p_Died + 1.96*se_Died)

#C.I by hand
p.1 = 0.0217 # sample proportion of dead for placebo group
n.1 =  13389 # sample size for placebo group
p.2 = 0.0170 # sample proportion of dead for vitA group
n.2 = 13732 # sample size for vitA group

#diff between placebo and vitA
diff = p.1 - p.2

# standard error
se = sqrt(p.1*(1-p.1)/n.1 + p.2*(1-p.2)/n.2) 

# confidence interval
LL = diff - 1.96*se
UL = diff + 1.96*se 

#confidence interval by age, sex and trt
nepal %>%
  group_by(sex, age) %>%
  summarize(N_Plac = sum(trt=="Placebo"),
            p_Plac = sum(status=="Died" & trt=="Placebo")/N_Plac,
            N_VitA = sum(trt=="Vit A"),
            p_VitA = sum(status=="Died" & trt=="Vit A")/N_VitA,
            diff = p_Plac - p_VitA,
            se = sqrt(p_Plac*(1 - p_Plac)/N_Plac + p_VitA*(1 - p_VitA)/N_VitA),
            CI_L = diff - 1.96*se,
            CI_U = diff + 1.96*se)


dataForCIplot = nepal %>%
  group_by(sex, age) %>%
  summarize(N_Plac = sum(trt=="Placebo"),
            p_Plac = sum(status=="Died" & trt=="Placebo")/N_Plac,
            N_VitA = sum(trt=="Vit A"),
            p_VitA = sum(status=="Died" & trt=="Vit A")/N_VitA,
            diff = p_Plac - p_VitA,
            se = sqrt(p_Plac*(1 - p_Plac)/N_Plac + p_VitA*(1 - p_VitA)/N_VitA),
            CI_L = diff - 1.96*se,
            CI_U = diff + 1.96*se)
agestrata = c(1,2,3,4,5,6,7)
agestrata_labels = c("F < 1", "F 1-2", "F 3-4", "M < 1", "M 1-2", "M 3-4", "Overall")
diff = c(dataForCIplot$diff, 0.0047)
#more code to come.may use ggplot

#fit a model on the data
model1 = glm(as.factor(status) ~ trt, data=nepal, family=binomial(link="identity"))
summary(model1)
confint(model1)

#second part



