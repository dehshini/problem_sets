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

