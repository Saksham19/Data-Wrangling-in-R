#spotify dataset
#source: https://www.kaggle.com/iamsumat/spotify-top-2000s-mega-dataset
#loading in the relevant libraries
library(tidyverse)
library(ggplot2)
rm(list=ls())

#set wd and load in the dataset
spotify<- read.csv("~/R random stuff/Datasets/Spotify-2000.csv")
View(spotify)

#sanity checks
#no.of songs by year
spotify%>% group_by(Year)%>%
  count()%>%
  ggplot(aes(Year,n))+
  geom_col()+
  labs(x="Year",y="total songs",title="No. of songs by year")
#seems to show no trends or anything. Dataset only till halfway of 2019, so bear that in mind

#can try the dates on which songs by the beatles were released

spotify%>%filter(Artist=="The Beatles")%>%
  group_by(Year)%>%
  count()%>%
  ggplot(aes(Year,n))+
  geom_col()+
  labs(x="year",y="songs released",title = "songs released by the beatles(on spotify) by years")

#seems to be alright. newer songs (post 1980) seem to be probably remasters

#end of sanity checks - dataset seems to be aright

#Which genres have the most albumns/songs within them?
#first, makes sense to get an idea of what genres have the most songs within them:
spotify%>%
  count(Top.Genre)%>%
  arrange(desc(n))->genre_top

#Next, we plot the ones with atleast 50 albums/songs within them
genre_top%>%filter(n>=50)%>%
  ggplot(aes(Top.Genre,n))+
  geom_col(fill="red2")+
  theme_minimal()+
  ggtitle("Number of albumns within top 10 genres")+
  xlab("Genres")+ylab("Count")+
  theme(plot.background = element_rect(fill="gray20"),
        text=element_text(colour="darkcyan"),
        panel.grid = element_blank(),
        plot.title = element_text(colour="darkcyan",face="bold",size=12),
        axis.line = element_line(colour="red2"),
        axis.text = element_text(colour="darkcyan"))
#no idea why album rock is at the top but oh well


#Top 10 songs in terms of popularity?
#higher the value of populatity, more popular the song is
#first, get the songs as per their popularity score in descending order
spotify%>%group_by(Popularity)%>%
  arrange(desc(Popularity))->pop_desc

#plot for only top 10 values

pop_desc%>%
  head(10)%>%
  ggplot(aes(reorder(Title,Popularity),y=Popularity))+
  geom_col(fill="greenyellow")+
  theme_minimal()+
  coord_flip()+
  ggtitle("Top 10 most popular songs")+
  geom_text(aes(label=Popularity), position=position_dodge(width=0.9),vjust=-0.25,hjust=-0.25,colour="grey96")+
  xlab("Song Name")+ylab("Popularity")+
  theme(plot.background = element_rect(fill="gray20"),
        text=element_text(colour="grey96"),
        panel.grid = element_blank(),
        plot.title = element_text(colour="grey96",face="bold",size=12),
        axis.line = element_line(colour="grey96"),
        axis.text = element_text(colour="grey96"))


#top 10 songs in terms of valence(positivity) 

#first, arrange the songs as per the valence score(higher is better, hence descending order)
spotify%>%group_by(Valence)%>%
  arrange(desc(Valence))->val_desc

#plot 
val_desc%>%
  head(10)%>%
  ggplot(aes(reorder(Title,Valence),Valence))+
  geom_col(fill="greenyellow")+
  theme_minimal()+
  coord_flip()+
  ggtitle("Top 10 songs based on valence score (Spotify)")+
  geom_text(aes(label=Valence),position=position_dodge(width=0.9),vjust=-0.25,hjust=-0.25,colour="grey96")+
  xlab("Song Name")+ylab("Valence")+
  theme(plot.background = element_rect(fill="gray20"),
        text=element_text(colour="greenyellow"),
        panel.grid=element_blank(),
        axis.line=element_line(colour="grey96"),
        plot.title = element_text(colour="greenyellow",face="bold",size=12),
        axis.text = element_text(colour="greenyellow"))


#Top 10 songs in terms of valence and song length

class(spotify$Length..Duration.)
#factor

#need to convert length to numeric to perform any meaningful analysis


spotify%>%mutate_at("Length..Duration.",~as.numeric(.))%>%
      group_by(Valence,Length..Duration.)%>%
      arrange(desc(Valence))->val_length
  


val_length%>%
  head(10)%>%
  ggplot(aes(Valence,Length..Duration.))+
  geom_point(fill="greenyellow")+
  theme_minimal()+
  coord_flip()+
  geom_hline(yintercept = mean(val_length$Length..Duration.[10]),linetype="dashed",colour="grey96")+
  geom_vline(xintercept = mean(val_length$Valence[10]),linetype="dashed",colour="grey96")+
  ggtitle("Valence and song length")+
  geom_text(aes(label=Title),position=position_dodge(width=0.9),vjust=3,hjust=0.8,colour="grey96",size=2.5)+
  xlab("Valence")+ylab("Song Length")+
  theme(plot.background = element_rect(fill="gray20"),
        text=element_text(colour="greenyellow"),
        panel.grid=element_blank(),
        axis.line=element_line(colour="grey96"),
        legend.position = "none",
        plot.title = element_text(colour="greenyellow",face="bold",size=12),
        axis.text = element_text(colour="greenyellow"))

#can run a regression to see the significance of song length on valence


attach(val_length)
lmfit<-lm(Valence~Length..Duration.+Popularity+Danceability+Energy+Liveness+Acousticness+Loudness..dB.+Beats.Per.Minute..BPM.+Speechiness)
summary(lmfit)
#p value too small, length seems to be significant at 1% level. Hence, length of the song matters in terms of happiness but not in terms of popularity
#adjusted R squared is small though



