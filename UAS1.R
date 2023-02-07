library(readxl)
library(ggplot2)

dt <- read_excel("C:/Users/Feelouis Elfredo/Downloads/20230113070740_STAT6157016_FIN_RCQuestion/STAT6157016 - Data Mining and Visualization - Copy/Insurance.xlsx")
view(dt)
head(dt)
summary(dt)

#cek missing data
sum(is.na(dt$age))
sum(is.na(dt$sex))
sum(is.na(dt$bmi))
sum(is.na(dt$children))
sum(is.na(dt$smoker))
sum(is.na(dt$region))
sum(is.na(dt$expenses))

#linear regression 
age_expenses <- ggplot(dt, aes(x = age,y = expenses)) +
  geom_point()
age_expenses

sex_expenses <- ggplot(dt, aes(x = sex,y = expenses)) +
  geom_point() 
sex_expenses

bmi_expenses <- ggplot(dt, aes(x = bmi,y = expenses)) +
  geom_point()
bmi_expenses

child_expenses <- ggplot(dt, aes(x = children,y = expenses)) +
  geom_point() 
child_expenses

smoker_expenses <- ggplot(dt, aes(x = smoker, y = expenses)) +
  geom_point() 
smoker_expenses

region_expenses <- ggplot(dt, aes(x = region, y = expenses)) +
  geom_point() 
region_expenses

#model regression
age_lm <- ggplot(dt, aes(x = age,y = expenses)) +
  geom_point() +
  geom_smooth(method = "lm")
age_lm

bmi_lm <- ggplot(dt, aes(x = bmi,y = expenses)) +
  geom_point()+
  geom_smooth(method = "lm")
bmi_lm

child_lm <- ggplot(dt, aes(x = children,y = expenses)) +
  geom_point() +
  geom_smooth(method = "lm")
child_lm

#significance test
test<- lm(expenses ~. , data=dt)
summary(test)