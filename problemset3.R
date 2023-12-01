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

#save the above as a new dataframe
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

#concatenate the overall values(diff, UL, LL, se) into new lists with the grouped data
diffg = c(dataForCIplot$diff, 0.0047)
LLg = c(dataForCIplot$CI_L, 0.00142)
ULg = c(dataForCIplot$CI_U, 0.00798)
se_g = c(dataForCIplot$se, se)

#create a new dataframe for the plot using the lists created
df = data_frame(sex=c(dataForCIplot$sex, "overall"),
                age=c(dataForCIplot$age, "overall"),
                diff=diffg, 
                se=se_g, 
                LL = LLg,
                UL = ULg)

#plot: create error plots
g2 = ggplot(df, aes(x=sex , y=diff, ymin=LL, ymax=UL))

g2 + geom_errorbar(aes(color=age), position = position_dodge(0.3), width = 0.3) +
  geom_point(aes(color=age, size=0.5, shape=age), position = position_dodge(0.3)) +
  geom_hline(aes(yintercept=0), color="black") +
  xlab("gender/age group") +
  ylab("difference in mortality rate (placebo - vitA)") +
  theme_minimal()+
  labs(title = "95% Confidence Intervals for Difference in Mortality Rates")

#fit a model on the data
model1 = glm(as.factor(status) ~ trt, data=nepal, family=binomial(link="identity"))
summary(model1)
confint(model1)
svycontrast(model1, c(1,1))
confint(svycontrast(model1, c(1,1)))

#second part

#Create two age groups (< 3 years, ≥ 3 years).
nepal = nepal %>%
  mutate(agegp = ifelse(age == "3 to 4", "3+ years", "<3 years"))

#calculate odds
nepal %>%
  group_by(agegp, trt) %>%
  summarize(N_Alive = sum(status=="Alive"),
            N_Died = sum(status=="Died"),
            Odds = N_Died/N_Alive)

#calculate OR of death for plac vs vitA
nepal %>%
  group_by(agegp) %>%
  summarize(N_Alive_P = sum(status=="Alive" & trt=="Placebo"),
            N_Died_P = sum(status=="Died" & trt=="Placebo"),
            N_Alive_V = sum(status=="Alive" & trt=="Vit A"),
            N_Died_V = sum(status=="Died" & trt=="Vit A"),
            OR = (N_Died_P/N_Alive_P)/(N_Died_V/N_Alive_V),
            se = sqrt(1/N_Alive_P + 1/N_Died_P + 1/N_Alive_V + 1/N_Died_V),
            CI_L = exp(log(OR)-1.96*se),
            CI_U = exp(log(OR)+1.96*se))

#create a dataframe for the low age group
nepal621.lowage = nepal %>% filter(agegp == "<3 years")

#fit a model
model2 = glm(as.factor(status) ~ trt, data=nepal621.lowage,
             family=binomial(link="logit"))
summary(model2) # This summary is on the logOR scale
exp(model2$coefficients) # We exponentiate to get on the OR scale
exp(confint(model2))

#create data frame for the high age group
nepal621.highage = nepal %>% filter(agegp == "3+ years")

model3 = glm(as.factor(status) ~ trt, data=nepal621.highage,
             family=binomial(link="logit"))
summary(model3)
exp(model3$coefficients)
exp(confint(model3))
