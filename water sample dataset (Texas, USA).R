#Case study 2:
#Water QUality Sampling Data, Austin, Texas
library(tidyverse)
library(stringr)
library(lubridate)

water<-read_csv("http://594442.youcanlearnit.net/austinwater.csv")
glimpse(water)
#PH and Water temp - main focus
water<-tibble("siteName"=water$SITE_NAME,"siteType"=water$SITE_TYPE,"sampleTime"=water$SAMPLE_DATE,"parameterType"=water$PARAM_TYPE,"parameter"=water$PARAMETER,"result"=water$RESULT,"unit"=water$UNIT)
#down to 7 columns
#we only want info about ph and water temp. 
unique(water$parameter)
#more than 3000 parameters
#search for ph
unique(water[which(str_detect(water$parameter,"PH")),]$parameter)
#not that helpful
unique(water$parameterType)
filtered_water<-subset(water,(parameterType=="Alkalinity/Hardness/pH")| parameterType=="Conventionals")
glimpse(filtered_water)
unique(filtered_water$parameter)
#can subset this to just the rows that measure ph and rows that measure water temp

filtered_water<-subset(filtered_water,(parameter=="PH")| (parameter=="WATER TEMPERATURE"))
glimpse(filtered_water)
summary(filtered_water)
#too many char variables
#siteType might be better off as factor
filtered_water$siteType<-as.factor(filtered_water$siteType)
filtered_water$parameterType<-as.factor(filtered_water$parameterType)
filtered_water$unit<-as.factor(filtered_water$unit)
#using lubridate to fix sampletime
filtered_water$sampleTime<-mdy_hms(filtered_water$sampleTime)

#can check the units of measurement
summary(filtered_water)
#ph and water temp are measured in diff units
#But multiple units of measurement for same type of obs
subset(filtered_water,unit=="Feet")
#probably wrong labeling
convert<-which(filtered_water$unit=="Feet")
filtered_water$unit[convert]<-"Deg. Fahrenheit"
glimpse(subset(filtered_water,unit=="MG/L" & parameter=="PH"))

convert<-which(filtered_water$unit=="MG/L" & filtered_water$parameter=="PH")
filtered_water$unit[convert]<-"Standard units"

glimpse(subset(filtered_water,unit=="MG/L" & result>70))
convert<-which(filtered_water$unit=="MG/L" & filtered_water$result>70)
filtered_water$unit[convert]<-"Deg. Fahrenheit"

convert<-which(filtered_water$unit=="MG/L")
filtered_water$unit[convert]<-"Deg. Celsius"
summary(filtered_water)
#everything is in deg c or F

#check for outliers
ggplot(filtered_water,mapping=aes(x=sampleTime,y=result))+
  geom_point()
#seems to be one outlier
subset(filtered_water,result>1000000)
#remove this plus the na value 
remove<-which(filtered_water$result>1000000 | is.na(filtered_water$result))
filtered_water<-filtered_water[-remove,]
glimpse(subset(filtered_water,result>1000))
#remove all these (As too high)
remove<-which(filtered_water$result>1000)
filtered_water<-filtered_water[-remove,]
#can check box plots to see if we can see any further outliers
#boxplot 1 - unit and result
ggplot(data=filtered_water,mapping=aes(x=unit,y=result))+
  geom_boxplot()
convert<-which(filtered_water$result>60,filtered_water$unit=="Deg. Celsius")
filtered_water$unit[convert]<-"Deg. Fahrenheit"

#there are still temp recorded in both fahrenheit and Celsius
fahrenheit<-which(filtered_water$unit=="Deg. Fahrenheit")

filtered_water$result[fahrenheit]<-(filtered_water$result[fahrenheit]-32)*(5.0/9.0)

filtered_water$unit[fahrenheit]<-"Deg. Celsius"
summary(filtered_water)
#all in celsius
#can remove the empty levels now
filtered_water$unit<-droplevels(filtered_water$unit)
summary(filtered_water)

#fixing the length issue now
#can remove parameter type, units
filtered_water<-filtered_water[,-c(4,7)]
summary(filtered_water)
#can try and spread it now
filtered_water_wide<-spread(filtered_water,parameter,result)
#repetition
filtered_water[c(49274,49342,49219,49284),]
dupe_check<-filtered_water[,-5]
dupes<-which(duplicated(dupe_check))
filtered_water<-filtered_water[-dupes,]
filtered_water_wide<-spread(filtered_water,parameter,result)
filtered_water_wide
colnames(filtered_water_wide)[4]<-"pH"
colnames(filtered_water_wide)[5]<-"temperature"
glimpse(filtered_water_wide)
#this dataset is ready for further analysis
