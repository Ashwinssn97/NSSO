# SCMA ASSIGNMENT A1 (A) ASHWIN SREEDHAR V00999566

data=read.csv("4NSSO68.csv")
names(data)

#SUBSETTING
arp = data[data$state_1 == 'ARP',]
dim(arp)
names(arp)
head(arp,5)

#DATA CLEANING
##NULL VALUE OMMISION
sum(is.na(arp))

library(dplyr)
View(arp)
colSums(is.na(arp))
arp<-subset(arp, select = -c(soyabean_q,soyabean_v,Land_Leased_in,Otherwise_possessed,Land_Leased_out, During_July_June_Cultivated,During_July_June_Irrigated,Days_Stayed_away,Meals_School,Meals_Employer,Meals_Others,Meals_Payment))
dim(arp)

arp$District
library(dplyr)
arp <- arp %>%
  mutate(Division = case_when(
    District == 1 ~ "Tawang",
    District == 2 ~ "West Kameng",
    District == 3 ~ "East Kameng",
    District == 4 ~ "Papum pare",
    District == 5 ~ "Lower Subansiri",
    District == 6 ~ "Upper subansiri",
    District == 7 ~ "West Siang",
    District == 8 ~ "East siang",
    District == 9 ~ "Upper siang",
    District == 10 ~ "Dibang valley",
    District == 11 ~ "Lohit",
    District == 12 ~ "Changlang",
    District == 13 ~ "Tirap",
    District == 14 ~ "Anjaw",
    District == 15 ~ "Kurungkumey",
    District == 16 ~ "Lower Dibang valley",
   ))


summary(arp$Age)
summary(arp$cerealtot_q)
summary(arp$cerealtot_v)
summary(arp$pulsestot_q)
summary(arp$pulsestot_v)
summary(arp$Milktotal_q)
summary(arp$Milktotal_v)
summary(arp$emftt_q)
summary(arp$emftt_v)
summary(arp$fruitt_total)
summary(arp$vegtt_q)
summary(arp$oilseeds_q)
summary(arp$oilseeds_v)

library(psych)
View(describe(arp))


hist(arp$Age)
hist(arp$cerealtot_q)
hist(arp$milk_q)
hist(arp$emftt_q)
hist(arp$pulsestot_q)
hist(arp$fruitt_total)
hist(arp$vegtt_q)

library(ggplot2)
# ASCERTAINING OUTLIERS
milk_q_outliers <- boxplot.stats(arp$milk_q)$out
milk_q_outliers_ind <- which(arp$milk_q %in% c(milk_q_outliers))
milk_q_outliers_ind

#Viewing the Outliers
View(arp[cerealtot_q_outliers_ind,])                                    
arp$milk_q





#SCMA ASSIGNMENT2 NSSO
rm(list = ls())
data<-read.csv("d:/Desktop/4NSSO68.csv")

## Arunachal Pradesh data
data_Aruna<-subset(data,state==12)

##  mean levels of consumptions in each division 
district_mean<-aggregate(data_Aruna$fv_tot, list(data_Aruna$District), mean)
colnames(district_mean)<-c("District","Mean")

district_mean1<-aggregate(data_Aruna$sugartotal_v, list(data_Aruna$District), mean)
colnames(district_mean)<-c("District","Mean")

district_mean2<-aggregate(data_Aruna$ricetotal_q, list(data_Aruna$District), mean)
colnames(district_mean)<-c("District","Mean")

district_mean3<-aggregate(data_Aruna$milk_q, list(data_Aruna$District), mean)
colnames(district_mean)<-c("District","Mean")

district_mean4<-aggregate(data_Aruna$pulsestot_q, list(data_Aruna$District), mean)
colnames(district_mean)<-c("District","Mean")



data_Aruna$District<-as.factor(data_Aruna$District)
library(ggplot2)
p<-ggplot(data_Aruna, aes(x=District,y=fv_tot, color=District)) +
  geom_boxplot()
p

q<-ggplot(data_Aruna, aes(x=District,y=sugartotal_v, color=District)) +
  geom_boxplot()
q

r<-ggplot(data_Aruna, aes(x=District,y=ricetotal_q, color=District)) +
  geom_boxplot()
r

s<-ggplot(data_Aruna, aes(x=District,y=milk_q, color=District)) +
  geom_boxplot()
s

t<-ggplot(data_Aruna, aes(x=District,y=pulsestot_q, color=District)) +
  geom_boxplot()
t
  

model<-lm(fv_tot~District,data = data_Aruna)
model1<-lm(sugartotal_v~District,data = data_Aruna)
model2<-lm(ricetotal_q~District,data = data_Aruna)
model3<-lm(milk_q~District,data = data_Aruna)
model4<-lm(pulsestot_q~District,data = data_Aruna)

summary(model)
summary(model1)
summary(model2)
summary(model3)
summary(model4)

confint(model)
confint(model1)
confint(model2)
confint(model3)
confint(model4)

# Compute the analysis of variance
res.aov <- aov(fv_tot ~ District, data = data_Aruna)
res.aov1 <- aov(sugartotal_v~ District, data = data_Aruna)
res.aov2 <- aov(ricetotal_q ~ District, data = data_Aruna)
res.aov3 <- aov(milk_q ~ District, data = data_Aruna)
res.aov4 <- aov(pulsestot_q ~ District, data = data_Aruna)

# Summary of the analysis
summary(res.aov)
summary(res.aov1)
summary(res.aov2)
summary(res.aov3)
summary(res.aov4)

TukeyHSD(res.aov)
TukeyHSD(res.aov1)
TukeyHSD(res.aov2)
TukeyHSD(res.aov3)
TukeyHSD(res.aov4)
