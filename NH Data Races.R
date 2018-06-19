setwd("C:/Users/Johnny/Documents/UNH Notes and Documents 2018/Data 801 R")

#import nh data 
nhdata <- read.csv("NH_cleaned.csv")

#import dplyr and ggplot
library(dplyr)
install.packages("ggplot2")
library(ggplot2)

#create tibble dataframe for nh stop data
nh <- tbl_df(nhdata)

#create dataframe of driver race, stop outcome, and in/out of state 
eth <- select(nh, driver_race, stop_outcome, out_of_state)

#filter for black/non black/white/asian/hispanic drivers who received a ticket or summons
summons <- eth %>%
  filter(!is.na(driver_race)) %>%
  filter(driver_race != "") %>%
  filter(stop_outcome == "Ticket" | stop_outcome =="Summons")

blacksummons <- eth %>%
  filter(driver_race == "Black") %>%
  filter(stop_outcome == "Ticket" | stop_outcome == "Summons")

othersummons <- eth %>%
  filter(!is.na(driver_race)) %>%
  filter(driver_race != "") %>%
  filter(driver_race != "Black") %>%
  filter(stop_outcome == "Ticket" | stop_outcome == "Summons")
  
whitesummons <- eth %>%
  filter(driver_race == "White") %>%
  filter(stop_outcome =="Ticket" | stop_outcome == "Summons")

asiansummons <- eth %>%
  filter(driver_race == "Asian") %>%
  filter(stop_outcome =="Ticket" | stop_outcome == "Summons")

hispanicsummons <- eth %>%
  filter(driver_race == "Hispanic") %>%
  filter(stop_outcome =="Ticket" | stop_outcome == "Summons")

#filter for total stops of black, white, hispanic, asian, and non black drivers
allstops <- eth %>%
  filter(!is.na(driver_race)) %>%
  filter(driver_race != "")
blackstops <- filter(nh, driver_race == "Black")
otherstops <- eth %>%
  filter(!is.na(driver_race)) %>%
  filter(driver_race != "") %>%
  filter(driver_race != "Black")
whitestops <- filter(nh, driver_race == "White")
asianstops <- filter(nh, driver_race == "Asian")
hispanicstops <- filter(nh, driver_race == "Hispanic")

#create variables for number of stops per race
totalstops <- nrow(allstops)
totalstopb <- nrow(blackstops) 
totalstopo <- nrow(otherstops)
totalstopw <- nrow(whitestops)
totalstopa <- nrow(asianstops)
totalstoph <- nrow(hispanicstops)

#create variable for number of tickets per race
totaltickets <- nrow(summons)
totalticketb <- nrow(blacksummons)
totalticketo <- nrow(othersummons)
totalticketw <- nrow(whitesummons)
totalticketa <- nrow(asiansummons)
totalticketh <- nrow(hispanicsummons)

#calculate and assign ratio of stops per race resulting in a ticket
ratioall <- totaltickets/totalstops
ratiob <- totalticketb/totalstopb
ratioo <- totalticketo/totalstopo
ratiow <- totalticketw/totalstopw
ratioa <- totalticketa/totalstopa
ratioh <- totalticketh/totalstoph

#print percentage of stops resulting in a ticket or summons per race
print(ratioall)
print(ratiow)
print(ratioa)
print(ratioh)
print(ratiob)
print(ratioo)

#create vectors listing races and respective ratios 
racelist <- c("White", "Black", "Hispanic", "Asian")
ratiolist <- c(ratiow, ratiob, ratioh, ratioa)

#create tibble dataframe from two vectors
ratmatrix <- matrix(c(racelist, ratiolist),nrow=length(racelist))
rattib <- tbl_df(ratmatrix)

#create barplot plotting race and ratio from new tibble df 
p<-ggplot(data=rattib, aes(x=V1, y=V2)) +
  geom_bar(stat="identity")
p <- p + xlab("Race")+ylab("Ratio")
p <- p + ggtitle("Ratio of Stops Resulting In Tickets or Summons by Race")
p

#filter fpr white drivers
white <- eth %>%
  filter(driver_race == "White")

#filter for black drivers
black <- eth %>%
  filter(driver_race == "Black") 

#filter for all non black drivers excluding na's and 
nonblack <- eth %>%
  filter(!is.na(driver_race)) %>%
  filter(driver_race != "") %>%
  filter(driver_race != "Black") 

#filter for asian drivers
asian <- eth %>%
  filter(driver_race == "Asian") 

#filter for hispanic drivers
hispanic <- eth %>%
  filter(driver_race == "Hispanic") 

#create barplots for stop outcomes of white, black, hispanic, and asian drivers 
whiteplot <- ggplot(white, aes(driver_race, fill = stop_outcome)) + geom_bar(position ="dodge")
whiteplot <- whiteplot + ggtitle("Outcomes of White Pullovers")
whiteplot <- whiteplot + theme_bw()
whiteplot <- whiteplot + xlab("Stop Outcomes")
whiteplot <- whiteplot + geom_text(stat='Count',aes(label=..count..),position = position_dodge(0.7), vjust=-0.5)
whiteplot

blackplot <- ggplot(black, aes(driver_race, fill = stop_outcome)) + geom_bar(position ="dodge")
blackplot <- blackplot + ggtitle("Outcomes of Black Pullovers")
blackplot <- blackplot + theme_bw()
blackplot <- blackplot + xlab("Stop Outcomes")
blackplot <- blackplot + geom_text(stat='Count',aes(label=..count..),position = position_dodge(0.7), vjust=-0.5)
blackplot

asianplot <- ggplot(asian, aes(driver_race, fill = stop_outcome)) + geom_bar(position ="dodge")
asianplot <- asianplot + ggtitle("Outcomes of Asian Pullovers")
asianplot <- asianplot + theme_bw()
asianplot <- asianplot + xlab("Stop Outcomes")
asianplot <- asianplot + geom_text(stat='Count',aes(label=..count..),position = position_dodge(0.7), vjust=-0.5)
asianplot

hispanicplot <- ggplot(hispanic, aes(driver_race, fill = stop_outcome)) + geom_bar(position ="dodge")
hispanicplot <- hispanicplot + ggtitle("Outcomes of Hispanic Pullovers")
hispanicplot <- hispanicplot + theme_bw()
hispanicplot <- hispanicplot + xlab("Stop Outcomes")
hispanicplot <- hispanicplot + geom_text(stat='Count',aes(label=..count..),position = position_dodge(0.7), vjust=-0.5)
hispanicplot


#filter out all na's and missing values in driver_age 
allstopsage <- nh %>%
  filter(!is.na(driver_age)) %>%
  filter(driver_age != "")

#filter for age ranges 15-24, 25-44, 45-64, 65+
stops1524 <- filter(allstopsage, driver_age >= 15 & driver_age <=24)
stops2544 <- filter(allstopsage, driver_age >= 25 & driver_age <=44)
stops4564 <- filter(allstopsage, driver_age >= 45 & driver_age <=64)
stops65 <- filter(allstopsage, driver_age >= 65)


#filter for age stops of each age range that resulted in a ticket or summons 
agesummons <- nh %>%
  filter(!is.na(driver_age)) %>%
  filter(driver_age != "") %>%
  filter(stop_outcome == "Ticket" | stop_outcome =="Summons")

summons1524 <- nh %>%
  filter(driver_age >= 15 & driver_age <=24) %>%
  filter(stop_outcome == "Ticket" | stop_outcome == "Summons")

summons2544 <- nh %>%
  filter(!is.na(driver_age)) %>%
  filter(driver_age != "") %>%
  filter(driver_age >= 25 & driver_age <=44) %>%
  filter(stop_outcome == "Ticket" | stop_outcome == "Summons")

summons4564 <- nh %>%
  filter(!is.na(driver_age)) %>%
  filter(driver_age != "") %>%
  filter(driver_age >= 45 & driver_age <=64) %>%
  filter(stop_outcome == "Ticket" | stop_outcome == "Summons")

summons65 <- nh %>%
  filter(!is.na(driver_age)) %>%
  filter(driver_age != "") %>%
  filter(driver_age >= 65)%>%
  filter(stop_outcome == "Ticket" | stop_outcome == "Summons")

#create variable for number of stops per age range
totalstopsage <- nrow(allstopsage)
totalstop1524 <- nrow(stops1524) 
totalstop2544 <- nrow(stops2544)
totalstop4564 <- nrow(stops4564)
totalstop65 <- nrow(stops65)

#create variable for number of tickets per age range 
totalticketsage <- nrow(agesummons)
totalticket1524 <- nrow(summons1524)
totalticket2544 <- nrow(summons2544)
totalticket4564 <- nrow(summons4564)
totalticket65 <- nrow(summons65)

#calculate and assign ratio of stops per age group resulting in a ticket
ratioall_age <- totalticketsage/totalstopsage
ratio1524 <- totalticket1524/totalstop1524
ratio2544 <- totalticket2544/totalstop2544
ratio4564 <- totalticket4564/totalstop4564
ratio65 <- totalticket65/totalstop65

#print percentage of stops resulting in a ticket or summons per age group
print(ratioall_age)
print(ratio1524)
print(ratio2544)
print(ratio4564)
print(ratio65)

#create 2 vectors listing age groups and respective ratios
agelist <- c("15-24", "25-44", "44-65", "65+")
ageratiolist <- c(ratio1524, ratio2544, ratio4564, ratio65)
agestopslist <- c(totalstop1524, totalstop2544, totalstop4564, totalstop65)

#create tibble dataframe using two vectors created above 
agestopsmatrix <- matrix(c(agelist, agestopslist),nrow=length(agelist))
ageratiomatrix <- matrix(c(agelist, ageratiolist),nrow=length(agelist))
ageratiotib <- tbl_df(ageratiomatrix)
agestopstib <- tbl_df(agestopsmatrix)

#create bar plot plotting age group and respective ratios 
ageplot<-ggplot(data=ageratiotib, aes(x=V1, y=V2)) +
  geom_bar(stat="identity")
ageplot <- ageplot + xlab("Age")+ylab("Ratio")
ageplot <- ageplot + ggtitle("Ratio of Stops Resulting In Tickets or Summons by Age Range")
ageplot


#print percentage of stops resulting in a ticket or summons per race
paste("The percentage of all stops resulting in a ticket or summons is", round(ratioall, 4))
paste("The percentage of stops of white individuals resulting in a ticket or summons is", round(ratiow, 4))
paste("The percentage of stops of asian individuals resulting in a ticket or summons is", round(ratioa, 4))
paste("The percentage of stops of hispanic individuals resulting in a ticket or summons is", round(ratioh, 4))
paste("The percentage of stops of black individuals resulting in a ticket or summons is", round(ratiob, 4))
paste("The percentage of stops of all non-black individuals resulting in a ticket or summons is", round(ratioo, 4))


#print percentage of stops resulting in a ticket or summons per age group
paste("The percentage of stops of all ages resulting in a ticket or summons is", round(ratioall_age, 4))
paste("The percentage of stops of individuals 15-24 resulting in a ticket or summons is", round(ratio1524, 4))
paste("The percentage of stops of individuals 25-44 resulting in a ticket or summons is", round(ratio2544, 4))
paste("The percentage of stops of individuals 45-64 resulting in a ticket or summons is", round(ratio4564, 4))
paste("The percentage of stops of individuals 65+ resulting in a ticket or summons is", round(ratio65, 4))


