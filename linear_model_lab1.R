mamdata = Lab.1.Data
head(mamdata)

#
ggplot(mamdata, aes(x=year)) +
  ylab("Percent with no mammagram") +
  geom_point(aes(y=Maryland, col="Maryland")) +
  geom_point(aes(y=US, col="US")) +
  scale_colour_manual("", breaks = c("Maryland", "US"),
                      values = c("red", "blue"))

qplot(x=year, y=US, data=mamdata)

#linear regression
model1 = lm(US ~ year, data=mamdata)
summary(model1)

model2 = lm(Maryland ~ year, data=mamdata)
summary(model2)

#centered Xs(year)
#create new Xs
mamdatanew = mamdata %>% mutate(yearcen = year - 1995)

model3 = lm(US ~ yearcen, data = mamdatanew)
summary(model3)

#another option that could have been used
model5 = glm(US ~ yearcen, data=mamdatanew, family=gaussian(link="identity"))
summary(model5)
confint(model5)

