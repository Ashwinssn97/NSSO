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

