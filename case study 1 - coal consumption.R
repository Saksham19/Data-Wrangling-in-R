#CASE STUDY 1
#Coal consumption
library(tidyverse)
#importing the dataset
coal<-read_csv("http://594442.youcanlearnit.net/coal.csv")
glimpse(coal)
#all data names are named as X
#1st row is blank
#2nd row is just the headers
#remove them
coal<-read_csv("http://594442.youcanlearnit.net/coal.csv",skip = 2)
glimpse(coal)
#X1 is just a placeholder name for col1 name
#rename it to region
colnames(coal)[1]<-"region"
summary(coal)
#data types are all wrong. Everything is char type while we expect a num type
#transform from wide to long dataset first (with only three colmns)
#the data has one row for every region and a col for each year
#pivot_longer function - wide to long
help(pivot_longer)
coal_long<-pivot_longer(coal,names_to = "year",values_to = "coal consumption",cols=-region)
glimpse(coal_long)
#still in char though
coal_long$year<-as.integer(coal_long$year)
coal_long$`coal consumption`<-as.numeric(coal_long$`coal consumption`)
#NAs introduced by coercion - is fine. We got NA values 
glimpse(coal_long)

#each tibble should contain info about only a single type of observational unit
#we have country and continent
unique(coal_long$region)
#we need to separate
noncountries<-c("North America","Central & South America","Antarctica","Europe","Eurasia","Middle East","Africa","Asia & Oceania","World")
matches<-which(!is.na(match(coal_long$region,noncountries)))
#create a tibble only containing data for countries (not continents)
coal_country<-coal_long[-matches,]
#create a tibble for only continents
coal_region<-coal_long[matches,]
coal_consumption<-(coal_region$`coal consumption`)
unique(coal_country$region)
unique(coal_region$region)
#can also separate the world observation into a separate tibble
wld<-c("World")
matches_2<-which(!is.na(match(coal_long$region,wld)))
coal_world<-coal_long[matches_2,]
unique(coal_world$region)

#visualizing this data using ggplot2

ggplot(data=coal_region,mapping=aes(x=year,y=coal_consumption))+
  geom_line(mapping=aes(color=region))
