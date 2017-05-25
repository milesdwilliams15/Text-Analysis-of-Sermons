
## Download csv file
sermons<-read.csv("https://raw.githubusercontent.com/ryanburge/sermon_analysis/master/sermons.csv")

  # Fix an error in one of the dates
library(car)
sermons$Date<-Recode(sermons$Date,"'10/04/015'='10/04/2015'")

  # Format the dates correctly
sermons$Date<-as.Date(sermons$Date,format='%m/%d/%Y')

  # Get rid of the random vector of all NAs
sermons$X<-NULL

  # Determine the denomination of each church from which these sermons originate
sermons$Denom<-NA
sermons$Denom[sermons$Name=="Ryan D. Cochran"] <- "Christian Churches/Churches of Christ"
sermons$Denom[sermons$Name=="George A. Mason"] <- "Southern Baptist Convention"
sermons$Denom[sermons$Name=="Timothy Peoples"] <- "Southern Baptist Convention"
sermons$Denom[sermons$Name=="Kevin Sinclair"] <- "Southern Baptist Convention"
sermons$Denom[sermons$Name=="Jakob Topper"] <- "Southern Baptist Convention"
sermons$Denom[sermons$Name=="Erica Whitaker"] <- "Cooperative Baptist Fellowship"
sermons$Denom[sermons$Name=="Matthew Broyles"] <- "Southern Baptist Convention"
sermons$Denom[sermons$Name=="Robert Jeffress"] <- "Southern Baptist Convention"
sermons$Denom[sermons$Name=="Josh Thiering"] <- "Southern Baptist Convention"
sermons$Denom[sermons$Name=="Jerry Johnson"] <- "Evangelical Free Church of America"
sermons$Denom[sermons$Name=="Tom Sterneman"] <- "Evangelical Free Church of America"
sermons$Denom[sermons$Name=="John F. Williams"] <- "Evangelical Free Church of America"
sermons$Denom[sermons$Name=="Peter Kolb"] <- "Evangelical Free Church of America"
sermons$Denom[sermons$Name=="Frederic Martin"] <- "Evangelical Free Church of America"
sermons$Denom[sermons$Name=="Micah Carpenter"] <- "Evangelical Free Church of America"
sermons$Denom[sermons$Name=="Jack Hyles"] <- "Independent Baptist"
sermons$Denom[sermons$Name=="Jacob Sutton"] <- "Lutheran"
sermons$Denom[sermons$Name=="Bryan Stecker"] <- "Lutheran"
sermons$Denom[sermons$Name=="Michael Sullivan"] <- "Lutheran"
sermons$Denom[sermons$Name=="Colin Smith"] <- "Evangelical Free Church of America"
sermons$Denom[sermons$Name=="Jason Brannan"] <- "Non-denominational"
sermons$Denom[sermons$Name=="JR Moffatt"] <- "Christian Churches/Churches of Christ"
sermons$Denom[sermons$Name=="Troy White"] <- "Christian Churches/Churches of Christ"
sermons$Denom[sermons$Name=="Clint Flanders"] <- "Christian Churches/Churches of Christ"
sermons$Denom[sermons$Name=="Brent Avery"] <- "Christian Churches/Churches of Christ"
sermons$Denom[sermons$Name=="Peter Bynum"] <- "Presbyterian"
sermons$Denom[sermons$Name=="Rachel Vogado"] <- "Presbyterian"
sermons$Denom[sermons$Name=="Lynne Keel"] <- "Presbyterian"
sermons$Denom[sermons$Name=="Caroline Parkinson"] <- "Episcopal"
sermons$Denom[sermons$Name=="Randolph Marshall Hollerith"] <- "Episcopal"
sermons$Denom[sermons$Name=="Hilary Streever"] <- "Episcopal"
sermons$Denom1<-sermons$Denom
sermons$Denom2<-NA
sermons$Denom2[sermons$Denom1=="Christian Churches/Churches of Christ"]<-"Evangelical"
sermons$Denom2[sermons$Denom1=="Southern Baptist Convention"]<-"Evangelical"
sermons$Denom2[sermons$Denom1=="Cooperative Baptist Fellowship"]<-"Evangelical"
sermons$Denom2[sermons$Denom1=="Evangelical Free Church of America"]<-"Evangelical"
sermons$Denom2[sermons$Denom1=="Independent Baptist"]<-"Evangelical"
sermons$Denom2[sermons$Denom1=="Non-denominational"]<-"Evangelical"
sermons$Denom2[sermons$Denom1=="Lutheran"]<-"Mainline"
sermons$Denom2[sermons$Denom1=="Presbyterian"]<-"Mainline"
sermons$Denom2[sermons$Denom1=="Episcopal"]<-"Mainline"
sermons$Denom<-NULL
sermons$Denom1<-as.factor(sermons$Denom1)
sermons$Denom2<-as.factor(sermons$Denom2)


  # Add a variable for prechers' gender
sermons$sex<-"Male"
sermons$sex[sermons$Name=="Rachel Vogado"]<-"Female"
sermons$sex[sermons$Name=="Caroline Parkinson"]<-"Female"
sermons$sex[sermons$Name=="Hilary Streever"]<-"Female"
sermons$sex[sermons$Name=="Erica Whitaker"]<-"Female"
sermons$sex<-as.factor(sermons$sex)

###################################
## Sentiment Analysis of Sermons ##
library(syuzhet)
sermons$Sermon<-as.character(sermons$Sermon)
sent<-get_nrc_sentiment(sermons$Sermon)
sent$valence<-sent$positive-sent$negative
sermons<-cbind(sermons,sent)

library(ggplot2)
library(plyr)
library(dplyr)
meanVar1<-ddply(sermons,~Denom1,summarise,Mean=mean(valence))
meanVar1$Denom2<-NA
meanVar1$Denom2[meanVar1$Denom1=="Christian Churches/Churches of Christ"]<-"Evangelical"
meanVar1$Denom2[meanVar1$Denom1=="Southern Baptist Convention"]<-"Evangelical"
meanVar1$Denom2[meanVar1$Denom1=="Cooperative Baptist Fellowship"]<-"Evangelical"
meanVar1$Denom2[meanVar1$Denom1=="Evangelical Free Church of America"]<-"Evangelical"
meanVar1$Denom2[meanVar1$Denom1=="Independent Baptist"]<-"Evangelical"
meanVar1$Denom2[meanVar1$Denom1=="Non-denominational"]<-"Evangelical"
meanVar1$Denom2[meanVar1$Denom1=="Lutheran"]<-"Mainline"
meanVar1$Denom2[meanVar1$Denom1=="Presbyterian"]<-"Mainline"
meanVar1$Denom2[meanVar1$Denom1=="Episcopal"]<-"Mainline"
meanVar1$Denom2<-as.factor(meanVar1$Denom2)
meanVar2<-ddply(sermons,~Denom2,summarise,Mean=mean(valence))
meanVar3<-ddply(sermons,~sex,summarise,Mean=mean(valence))

  ## Sentiment Valence
windows()
ggplot(meanVar1,aes(reorder(Denom1,Mean),Mean,fill=Denom2)) +
  geom_col() + 
  theme_classic() +
  scale_fill_grey() +
  ggtitle("Mean Sentiment Valence in Protestant Sermons",
          subtitle="Values Obtained via the NRC Emotion Lexicon") +
  theme(plot.subtitle=element_text(face="italic",size=14)) +
  theme(plot.title=element_text(face="bold",size=16)) +
  theme(plot.subtitle=element_text(size=12, hjust=0, face="italic", color="black")) +
  theme(axis.text.x=element_text(angle=45,color="black",vjust=1,hjust=1)) +
  ylab("Mean Sentiment Valence") + xlab("") +
  theme(text=element_text(family="serif")) +
  theme(legend.position=c(.15,.9)) +
  theme(legend.title=element_blank()) +
  theme(legend.background = element_rect(color="black", size=.5)) +
  theme(panel.grid.major.y=element_line(colour="lightgrey",linetype=1))

windows()
ggplot(meanVar2,aes(Denom2,Mean)) +
  geom_col(width=.5) + 
  theme_classic() +
  scale_fill_grey() +
  ggtitle("Mean Sentiment Valence in Protestant Sermons",
          subtitle="Values Obtained via the NRC Emotion Lexicon") +
  theme(plot.subtitle=element_text(face="italic",size=14)) +
  theme(plot.title=element_text(face="bold",size=16)) +
  theme(plot.subtitle=element_text(size=12, hjust=0, face="italic", color="black")) +
  theme(axis.text.x=element_text(color="black",size=12)) +
  ylab("Mean Sentiment Valence") + xlab("") +
  theme(text=element_text(family="serif")) +
  theme(panel.grid.major.y=element_line(colour="lightgrey",linetype=1))

windows()
ggplot(meanVar3,aes(sex,Mean)) +
  geom_col(width=.5) + 
  theme_classic() +
  scale_fill_grey() +
  ggtitle("Mean Sentiment Valence in Protestant Sermons",
          subtitle="Values Obtained via the NRC Emotion Lexicon") +
  theme(plot.subtitle=element_text(face="italic",size=14)) +
  theme(plot.title=element_text(face="bold",size=16)) +
  theme(plot.subtitle=element_text(size=12, hjust=0, face="italic", color="black")) +
  theme(axis.text.x=element_text(color="black",size=12)) +
  ylab("Mean Sentiment Valence") + xlab("") +
  theme(text=element_text(family="serif")) +
  theme(panel.grid.major.y=element_line(colour="lightgrey",linetype=1))

  ## Emotions
meanEmo1<-ddply(sermons,~Denom1,summarise,Anger=mean(anger),
                Anticipation=mean(anticipation),
                Disgust=mean(disgust),
                Fear=mean(fear),
                Joy=mean(joy),
                Sadness=mean(sadness),
                Surprise=mean(surprise),
                Trust=mean(trust))
meanEmo1$Denom2<-NA
meanEmo1$Denom2[meanEmo1$Denom1=="Christian Churches/Churches of Christ"]<-"Evangelical"
meanEmo1$Denom2[meanEmo1$Denom1=="Southern Baptist Convention"]<-"Evangelical"
meanEmo1$Denom2[meanEmo1$Denom1=="Cooperative Baptist Fellowship"]<-"Evangelical"
meanEmo1$Denom2[meanEmo1$Denom1=="Evangelical Free Church of America"]<-"Evangelical"
meanEmo1$Denom2[meanEmo1$Denom1=="Independent Baptist"]<-"Evangelical"
meanEmo1$Denom2[meanEmo1$Denom1=="Non-denominational"]<-"Evangelical"
meanEmo1$Denom2[meanEmo1$Denom1=="Lutheran"]<-"Mainline"
meanEmo1$Denom2[meanEmo1$Denom1=="Presbyterian"]<-"Mainline"
meanEmo1$Denom2[meanEmo1$Denom1=="Episcopal"]<-"Mainline"
meanEmo1$Denom2<-as.factor(meanEmo1$Denom2)
meanEmo2<-ddply(sermons,~Denom2,summarise,Anger=mean(anger),
                Anticipation=mean(anticipation),
                Disgust=mean(disgust),
                Fear=mean(fear),
                Joy=mean(joy),
                Sadness=mean(sadness),
                Surprise=mean(surprise),
                Trust=mean(trust))
meanEmo3<-ddply(sermons,~sex,summarise,Anger=mean(anger),
                Anticipation=mean(anticipation),
                Disgust=mean(disgust),
                Fear=mean(fear),
                Joy=mean(joy),
                Sadness=mean(sadness),
                Surprise=mean(surprise),
                Trust=mean(trust))

# Anger
windows()
ggplot(meanEmo1,aes(reorder(Denom1,Anger),Anger,fill=Denom2)) +
  geom_col() + 
  theme_classic() +
  scale_fill_grey() +
  ggtitle("Mean Anger in Protestant Sermons",
          subtitle="Values Obtained via the NRC Emotion Lexicon") +
  theme(plot.subtitle=element_text(face="italic",size=14)) +
  theme(plot.title=element_text(face="bold",size=16)) +
  theme(plot.subtitle=element_text(size=12, hjust=0, face="italic", color="black")) +
  theme(axis.text.x=element_text(angle=45,color="black",vjust=1,hjust=1)) +
  ylab("Mean Anger") + xlab("") +
  theme(text=element_text(family="serif")) +
  theme(legend.position=c(.15,.9)) +
  theme(legend.title=element_blank()) +
  theme(legend.background = element_rect(color="black", size=.5)) +
  theme(panel.grid.major.y=element_line(colour="lightgrey",linetype=1))

windows()
ggplot(meanEmo2,aes(Denom2,Anger)) +
  geom_col(width=.5) + 
  theme_classic() +
  scale_fill_grey() +
  ggtitle("Mean Anger in Protestant Sermons",
          subtitle="Values Obtained via the NRC Emotion Lexicon") +
  theme(plot.subtitle=element_text(face="italic",size=14)) +
  theme(plot.title=element_text(face="bold",size=16)) +
  theme(plot.subtitle=element_text(size=12, hjust=0, face="italic", color="black")) +
  theme(axis.text.x=element_text(color="black",size=12)) +
  ylab("Mean Anger") + xlab("") +
  theme(text=element_text(family="serif")) +
  theme(panel.grid.major.y=element_line(colour="lightgrey",linetype=1))

windows()
ggplot(meanEmo3,aes(sex,Anger)) +
  geom_col(width=.5) + 
  theme_classic() +
  scale_fill_grey() +
  ggtitle("Mean Anger in Protestant Sermons",
          subtitle="Values Obtained via the NRC Emotion Lexicon") +
  theme(plot.subtitle=element_text(face="italic",size=14)) +
  theme(plot.title=element_text(face="bold",size=16)) +
  theme(plot.subtitle=element_text(size=12, hjust=0, face="italic", color="black")) +
  theme(axis.text.x=element_text(color="black",size=12)) +
  ylab("Mean Anger") + xlab("") +
  theme(text=element_text(family="serif")) +
  theme(panel.grid.major.y=element_line(colour="lightgrey",linetype=1))


# Anticipation
windows()
ggplot(meanEmo1,aes(reorder(Denom1,Anticipation),Anticipation,fill=Denom2)) +
  geom_col() + 
  theme_classic() +
  scale_fill_grey() +
  ggtitle("Mean Anticipation in Protestant Sermons",
          subtitle="Values Obtained via the NRC Emotion Lexicon") +
  theme(plot.subtitle=element_text(face="italic",size=14)) +
  theme(plot.title=element_text(face="bold",size=16)) +
  theme(plot.subtitle=element_text(size=12, hjust=0, face="italic", color="black")) +
  theme(axis.text.x=element_text(angle=45,color="black",vjust=1,hjust=1)) +
  ylab("Mean Anticipation") + xlab("") +
  theme(text=element_text(family="serif")) +
  theme(legend.position=c(.15,.9)) +
  theme(legend.title=element_blank()) +
  theme(legend.background = element_rect(color="black", size=.5)) +
  theme(panel.grid.major.y=element_line(colour="lightgrey",linetype=1))

windows()
ggplot(meanEmo2,aes(Denom2,Anticipation)) +
  geom_col(width=.5) + 
  theme_classic() +
  scale_fill_grey() +
  ggtitle("Mean Anticipation in Protestant Sermons",
          subtitle="Values Obtained via the NRC Emotion Lexicon") +
  theme(plot.subtitle=element_text(face="italic",size=14)) +
  theme(plot.title=element_text(face="bold",size=16)) +
  theme(plot.subtitle=element_text(size=12, hjust=0, face="italic", color="black")) +
  theme(axis.text.x=element_text(color="black",size=12)) +
  ylab("Mean Anticipation") + xlab("") +
  theme(text=element_text(family="serif")) +
  theme(panel.grid.major.y=element_line(colour="lightgrey",linetype=1))

windows()
ggplot(meanEmo3,aes(sex,Anticipation)) +
  geom_col(width=.5) + 
  theme_classic() +
  scale_fill_grey() +
  ggtitle("Mean Anticipation in Protestant Sermons",
          subtitle="Values Obtained via the NRC Emotion Lexicon") +
  theme(plot.subtitle=element_text(face="italic",size=14)) +
  theme(plot.title=element_text(face="bold",size=16)) +
  theme(plot.subtitle=element_text(size=12, hjust=0, face="italic", color="black")) +
  theme(axis.text.x=element_text(color="black",size=12)) +
  ylab("Mean Anticipation") + xlab("") +
  theme(text=element_text(family="serif")) +
  theme(panel.grid.major.y=element_line(colour="lightgrey",linetype=1))


# Disgust
windows()
ggplot(meanEmo1,aes(reorder(Denom1,Disgust),Disgust,fill=Denom2)) +
  geom_col() + 
  theme_classic() +
  scale_fill_grey() +
  ggtitle("Mean Disgust in Protestant Sermons",
          subtitle="Values Obtained via the NRC Emotion Lexicon") +
  theme(plot.subtitle=element_text(face="italic",size=14)) +
  theme(plot.title=element_text(face="bold",size=16)) +
  theme(plot.subtitle=element_text(size=12, hjust=0, face="italic", color="black")) +
  theme(axis.text.x=element_text(angle=45,color="black",vjust=1,hjust=1)) +
  ylab("Mean Disgust") + xlab("") +
  theme(text=element_text(family="serif")) +
  theme(legend.position=c(.15,.9)) +
  theme(legend.title=element_blank()) +
  theme(legend.background = element_rect(color="black", size=.5)) +
  theme(panel.grid.major.y=element_line(colour="lightgrey",linetype=1))

windows()
ggplot(meanEmo2,aes(Denom2,Disgust)) +
  geom_col(width=.5) + 
  theme_classic() +
  scale_fill_grey() +
  ggtitle("Mean Disgust in Protestant Sermons",
          subtitle="Values Obtained via the NRC Emotion Lexicon") +
  theme(plot.subtitle=element_text(face="italic",size=14)) +
  theme(plot.title=element_text(face="bold",size=16)) +
  theme(plot.subtitle=element_text(size=12, hjust=0, face="italic", color="black")) +
  theme(axis.text.x=element_text(color="black",size=12)) +
  ylab("Mean Disgust") + xlab("") +
  theme(text=element_text(family="serif")) +
  theme(panel.grid.major.y=element_line(colour="lightgrey",linetype=1))

windows()
ggplot(meanEmo3,aes(sex,Disgust)) +
  geom_col(width=.5) + 
  theme_classic() +
  scale_fill_grey() +
  ggtitle("Mean Disgust in Protestant Sermons",
          subtitle="Values Obtained via the NRC Emotion Lexicon") +
  theme(plot.subtitle=element_text(face="italic",size=14)) +
  theme(plot.title=element_text(face="bold",size=16)) +
  theme(plot.subtitle=element_text(size=12, hjust=0, face="italic", color="black")) +
  theme(axis.text.x=element_text(color="black",size=12)) +
  ylab("Mean Disgust") + xlab("") +
  theme(text=element_text(family="serif")) +
  theme(panel.grid.major.y=element_line(colour="lightgrey",linetype=1))


# Fear
windows()
ggplot(meanEmo1,aes(reorder(Denom1,Fear),Fear,fill=Denom2)) +
  geom_col() + 
  theme_classic() +
  scale_fill_grey() +
  ggtitle("Mean Fear in Protestant Sermons",
          subtitle="Values Obtained via the NRC Emotion Lexicon") +
  theme(plot.subtitle=element_text(face="italic",size=14)) +
  theme(plot.title=element_text(face="bold",size=16)) +
  theme(plot.subtitle=element_text(size=12, hjust=0, face="italic", color="black")) +
  theme(axis.text.x=element_text(angle=45,color="black",vjust=1,hjust=1)) +
  ylab("Mean Fear") + xlab("") +
  theme(text=element_text(family="serif")) +
  theme(legend.position=c(.15,.9)) +
  theme(legend.title=element_blank()) +
  theme(legend.background = element_rect(color="black", size=.5)) +
  theme(panel.grid.major.y=element_line(colour="lightgrey",linetype=1))

windows()
ggplot(meanEmo2,aes(Denom2,Fear)) +
  geom_col(width=.5) + 
  theme_classic() +
  scale_fill_grey() +
  ggtitle("Mean Fear in Protestant Sermons",
          subtitle="Values Obtained via the NRC Emotion Lexicon") +
  theme(plot.subtitle=element_text(face="italic",size=14)) +
  theme(plot.title=element_text(face="bold",size=16)) +
  theme(plot.subtitle=element_text(size=12, hjust=0, face="italic", color="black")) +
  theme(axis.text.x=element_text(color="black",size=12)) +
  ylab("Mean Fear") + xlab("") +
  theme(text=element_text(family="serif")) +
  theme(panel.grid.major.y=element_line(colour="lightgrey",linetype=1))

windows()
ggplot(meanEmo3,aes(sex,Fear)) +
  geom_col(width=.5) + 
  theme_classic() +
  scale_fill_grey() +
  ggtitle("Mean Fear in Protestant Sermons",
          subtitle="Values Obtained via the NRC Emotion Lexicon") +
  theme(plot.subtitle=element_text(face="italic",size=14)) +
  theme(plot.title=element_text(face="bold",size=16)) +
  theme(plot.subtitle=element_text(size=12, hjust=0, face="italic", color="black")) +
  theme(axis.text.x=element_text(color="black",size=12)) +
  ylab("Mean Fear") + xlab("") +
  theme(text=element_text(family="serif")) +
  theme(panel.grid.major.y=element_line(colour="lightgrey",linetype=1))


# Joy
windows()
ggplot(meanEmo1,aes(reorder(Denom1,Joy),Joy,fill=Denom2)) +
  geom_col() + 
  theme_classic() +
  scale_fill_grey() +
  ggtitle("Mean Joy in Protestant Sermons",
          subtitle="Values Obtained via the NRC Emotion Lexicon") +
  theme(plot.subtitle=element_text(face="italic",size=14)) +
  theme(plot.title=element_text(face="bold",size=16)) +
  theme(plot.subtitle=element_text(size=12, hjust=0, face="italic", color="black")) +
  theme(axis.text.x=element_text(angle=45,color="black",vjust=1,hjust=1)) +
  ylab("Mean Joy") + xlab("") +
  theme(text=element_text(family="serif")) +
  theme(legend.position=c(.15,.9)) +
  theme(legend.title=element_blank()) +
  theme(legend.background = element_rect(color="black", size=.5)) +
  theme(panel.grid.major.y=element_line(colour="lightgrey",linetype=1))

windows()
ggplot(meanEmo2,aes(Denom2,Joy)) +
  geom_col(width=.5) + 
  theme_classic() +
  scale_fill_grey() +
  ggtitle("Mean Joy in Protestant Sermons",
          subtitle="Values Obtained via the NRC Emotion Lexicon") +
  theme(plot.subtitle=element_text(face="italic",size=14)) +
  theme(plot.title=element_text(face="bold",size=16)) +
  theme(plot.subtitle=element_text(size=12, hjust=0, face="italic", color="black")) +
  theme(axis.text.x=element_text(color="black",size=12)) +
  ylab("Mean Joy") + xlab("") +
  theme(text=element_text(family="serif")) +
  theme(panel.grid.major.y=element_line(colour="lightgrey",linetype=1))

windows()
ggplot(meanEmo3,aes(sex,Joy)) +
  geom_col(width=.5) + 
  theme_classic() +
  scale_fill_grey() +
  ggtitle("Mean Joy in Protestant Sermons",
          subtitle="Values Obtained via the NRC Emotion Lexicon") +
  theme(plot.subtitle=element_text(face="italic",size=14)) +
  theme(plot.title=element_text(face="bold",size=16)) +
  theme(plot.subtitle=element_text(size=12, hjust=0, face="italic", color="black")) +
  theme(axis.text.x=element_text(color="black",size=12)) +
  ylab("Mean Joy") + xlab("") +
  theme(text=element_text(family="serif")) +
  theme(panel.grid.major.y=element_line(colour="lightgrey",linetype=1))


# Sadness
windows()
ggplot(meanEmo1,aes(reorder(Denom1,Sadness),Sadness,fill=Denom2)) +
  geom_col() + 
  theme_classic() +
  scale_fill_grey() +
  ggtitle("Mean Sadness in Protestant Sermons",
          subtitle="Values Obtained via the NRC Emotion Lexicon") +
  theme(plot.subtitle=element_text(face="italic",size=14)) +
  theme(plot.title=element_text(face="bold",size=16)) +
  theme(plot.subtitle=element_text(size=12, hjust=0, face="italic", color="black")) +
  theme(axis.text.x=element_text(angle=45,color="black",vjust=1,hjust=1)) +
  ylab("Mean Sadness") + xlab("") +
  theme(text=element_text(family="serif")) +
  theme(legend.position=c(.15,.9)) +
  theme(legend.title=element_blank()) +
  theme(legend.background = element_rect(color="black", size=.5)) +
  theme(panel.grid.major.y=element_line(colour="lightgrey",linetype=1))

windows()
ggplot(meanEmo2,aes(Denom2,Sadness)) +
  geom_col(width=.5) + 
  theme_classic() +
  scale_fill_grey() +
  ggtitle("Mean Sadness in Protestant Sermons",
          subtitle="Values Obtained via the NRC Emotion Lexicon") +
  theme(plot.subtitle=element_text(face="italic",size=14)) +
  theme(plot.title=element_text(face="bold",size=16)) +
  theme(plot.subtitle=element_text(size=12, hjust=0, face="italic", color="black")) +
  theme(axis.text.x=element_text(color="black",size=12)) +
  ylab("Mean Sadness") + xlab("") +
  theme(text=element_text(family="serif")) +
  theme(panel.grid.major.y=element_line(colour="lightgrey",linetype=1))

windows()
ggplot(meanEmo3,aes(sex,Sadness)) +
  geom_col(width=.5) + 
  theme_classic() +
  scale_fill_grey() +
  ggtitle("Mean Sadness in Protestant Sermons",
          subtitle="Values Obtained via the NRC Emotion Lexicon") +
  theme(plot.subtitle=element_text(face="italic",size=14)) +
  theme(plot.title=element_text(face="bold",size=16)) +
  theme(plot.subtitle=element_text(size=12, hjust=0, face="italic", color="black")) +
  theme(axis.text.x=element_text(color="black",size=12)) +
  ylab("Mean Sadness") + xlab("") +
  theme(text=element_text(family="serif")) +
  theme(panel.grid.major.y=element_line(colour="lightgrey",linetype=1))


# Surprise
windows()
ggplot(meanEmo1,aes(reorder(Denom1,Surprise),Surprise,fill=Denom2)) +
  geom_col() + 
  theme_classic() +
  scale_fill_grey() +
  ggtitle("Mean Surprise in Protestant Sermons",
          subtitle="Values Obtained via the NRC Emotion Lexicon") +
  theme(plot.subtitle=element_text(face="italic",size=14)) +
  theme(plot.title=element_text(face="bold",size=16)) +
  theme(plot.subtitle=element_text(size=12, hjust=0, face="italic", color="black")) +
  theme(axis.text.x=element_text(angle=45,color="black",vjust=1,hjust=1)) +
  ylab("Mean Surprise") + xlab("") +
  theme(text=element_text(family="serif")) +
  theme(legend.position=c(.15,.9)) +
  theme(legend.title=element_blank()) +
  theme(legend.background = element_rect(color="black", size=.5)) +
  theme(panel.grid.major.y=element_line(colour="lightgrey",linetype=1))

windows()
ggplot(meanEmo2,aes(Denom2,Surprise)) +
  geom_col(width=.5) + 
  theme_classic() +
  scale_fill_grey() +
  ggtitle("Mean Surprise in Protestant Sermons",
          subtitle="Values Obtained via the NRC Emotion Lexicon") +
  theme(plot.subtitle=element_text(face="italic",size=14)) +
  theme(plot.title=element_text(face="bold",size=16)) +
  theme(plot.subtitle=element_text(size=12, hjust=0, face="italic", color="black")) +
  theme(axis.text.x=element_text(color="black",size=12)) +
  ylab("Mean Surprise") + xlab("") +
  theme(text=element_text(family="serif")) +
  theme(panel.grid.major.y=element_line(colour="lightgrey",linetype=1))

windows()
ggplot(meanEmo3,aes(sex,Surprise)) +
  geom_col(width=.5) + 
  theme_classic() +
  scale_fill_grey() +
  ggtitle("Mean Surprise in Protestant Sermons",
          subtitle="Values Obtained via the NRC Emotion Lexicon") +
  theme(plot.subtitle=element_text(face="italic",size=14)) +
  theme(plot.title=element_text(face="bold",size=16)) +
  theme(plot.subtitle=element_text(size=12, hjust=0, face="italic", color="black")) +
  theme(axis.text.x=element_text(color="black",size=12)) +
  ylab("Mean Surprise") + xlab("") +
  theme(text=element_text(family="serif")) +
  theme(panel.grid.major.y=element_line(colour="lightgrey",linetype=1))


# Trust
windows()
ggplot(meanEmo1,aes(reorder(Denom1,Trust),Trust,fill=Denom2)) +
  geom_col() + 
  theme_classic() +
  scale_fill_grey() +
  ggtitle("Mean Trust in Protestant Sermons",
          subtitle="Values Obtained via the NRC Emotion Lexicon") +
  theme(plot.subtitle=element_text(face="italic",size=14)) +
  theme(plot.title=element_text(face="bold",size=16)) +
  theme(plot.subtitle=element_text(size=12, hjust=0, face="italic", color="black")) +
  theme(axis.text.x=element_text(angle=45,color="black",vjust=1,hjust=1)) +
  ylab("Mean Trust") + xlab("") +
  theme(text=element_text(family="serif")) +
  theme(legend.position=c(.15,.9)) +
  theme(legend.title=element_blank()) +
  theme(legend.background = element_rect(color="black", size=.5)) +
  theme(panel.grid.major.y=element_line(colour="lightgrey",linetype=1))

windows()
ggplot(meanEmo2,aes(Denom2,Trust)) +
  geom_col(width=.5) + 
  theme_classic() +
  scale_fill_grey() +
  ggtitle("Mean Trust in Protestant Sermons",
          subtitle="Values Obtained via the NRC Emotion Lexicon") +
  theme(plot.subtitle=element_text(face="italic",size=14)) +
  theme(plot.title=element_text(face="bold",size=16)) +
  theme(plot.subtitle=element_text(size=12, hjust=0, face="italic", color="black")) +
  theme(axis.text.x=element_text(color="black",size=12)) +
  ylab("Mean Trust") + xlab("") +
  theme(text=element_text(family="serif")) +
  theme(panel.grid.major.y=element_line(colour="lightgrey",linetype=1))

windows()
ggplot(meanEmo3,aes(sex,Trust)) +
  geom_col(width=.5) + 
  theme_classic() +
  scale_fill_grey() +
  ggtitle("Mean Trust in Protestant Sermons",
          subtitle="Values Obtained via the NRC Emotion Lexicon") +
  theme(plot.subtitle=element_text(face="italic",size=14)) +
  theme(plot.title=element_text(face="bold",size=16)) +
  theme(plot.subtitle=element_text(size=12, hjust=0, face="italic", color="black")) +
  theme(axis.text.x=element_text(color="black",size=12)) +
  ylab("Mean Trust") + xlab("") +
  theme(text=element_text(family="serif")) +
  theme(panel.grid.major.y=element_line(colour="lightgrey",linetype=1))


###########################################
## tf-idf Analysis Based on Denomination ##
library(tm)
sermons2<-sermons
sermons2$Sermon <- tolower(sermons2$Sermon) #make it lower case
sermons2$Sermon <- gsub('[[:punct:]]', '', sermons2$Sermon) #remove punctuation
sermons2$Sermon <- gsub('[[:digit:]]+', '', sermons2$Sermon) #remove numbers
sermons2$Sermon <- Corpus(VectorSource(sermons2$Sermon))
sermons2$Sermon <- tm_map(sermons2$Sermon, removeWords, stopwords('english')) #remove stopwords
sermons2$Sermon <- lapply(sermons2$Sermon[1:305], as.character)
sermons2$Sermon <- unlist(sermons2$Sermon)

library(tidytext)
sermon_words <- sermons2 %>% unnest_tokens(word, Sermon) %>%
  count(Denom2, word, sort = TRUE) %>%
  ungroup()

sermon_words <- sermon_words  %>% bind_tf_idf(word,Denom2, n) %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word))))

library(ggstance)
library(ggthemes)

## By Denominaiton
sermon_words2 <- sermon_words %>% filter(Denom2 == "Evangelical")
windows()
ggplot(sermon_words2[1:20,], aes(tf_idf, reorder(word,tf_idf), alpha=tf_idf)) +
  theme_classic() + geom_barh(stat = "identity", show.legend = FALSE, fill="red") + 
  scale_alpha(range=c(.5,1)) +
  ggtitle("Top tf-idf Words in Evangelical Protestant Sermons") +
  ylab("") + xlab("tf-idf") +
  scale_x_continuous(expand=c(0,0)) +
  theme(strip.text=element_text(hjust=0)) +
  theme(text=element_text(family="serif")) +
  theme(plot.title=element_text(size=16, hjust=0, face="bold", color="black")) +
  theme(legend.position = "none") +
  theme(panel.grid.major.x=element_line(colour="lightgrey",linetype=1))

sermon_words3 <- sermon_words %>% filter(Denom2 == "Mainline")
windows()
ggplot(sermon_words3[1:20,], aes(tf_idf, reorder(word,tf_idf), alpha=tf_idf)) +
  theme_classic() + geom_barh(stat = "identity", show.legend = FALSE, fill="blue") +
  scale_alpha(range=c(.5,1)) +
  ggtitle("Top tf-idf Words in Mainline Protestant Sermons") +
  ylab("") + xlab("tf-idf") +
  scale_x_continuous(expand=c(0,0)) +
  theme(strip.text=element_text(hjust=0)) +
  theme(text=element_text(family="serif")) +
  theme(plot.title=element_text(size=16, hjust=0, face="bold", color="black")) +
  theme(legend.position = "none") +
  theme(panel.grid.major.x=element_line(colour="lightgrey",linetype=1))


## By Sex
sermon_wordsSex <- sermons2 %>% unnest_tokens(word, Sermon) %>%
  count(sex, word, sort = TRUE) %>%
  ungroup()

sermon_wordsSex <- sermon_wordsSex  %>% bind_tf_idf(word,sex, n) %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word))))

sermon_words4 <- sermon_wordsSex %>% filter(sex == "Male")
windows()
ggplot(sermon_words4[1:20,], aes(tf_idf, reorder(word,tf_idf), alpha=tf_idf)) +
  theme_classic() + geom_barh(stat = "identity", show.legend = FALSE, fill="red") + 
  scale_alpha(range=c(.5,1)) +
  ggtitle("Top tf-idf Words in Sermons Given by Men") +
  ylab("") + xlab("tf-idf") +
  scale_x_continuous(expand=c(0,0)) +
  theme(strip.text=element_text(hjust=0)) +
  theme(text=element_text(family="serif")) +
  theme(plot.title=element_text(size=16, hjust=0, face="bold", color="black")) +
  theme(legend.position = "none") +
  theme(panel.grid.major.x=element_line(colour="lightgrey",linetype=1))

sermon_words5 <- sermon_wordsSex %>% filter(sex == "Female")
windows()
ggplot(sermon_words5[1:20,], aes(tf_idf, reorder(word,tf_idf), alpha=tf_idf)) +
  theme_classic() + geom_barh(stat = "identity", show.legend = FALSE, fill="blue") +
  scale_alpha(range=c(.5,1)) +
  ggtitle("Top tf-idf Words in Sermons Given by Women") +
  ylab("") + xlab("tf-idf") +
  scale_x_continuous(expand=c(0,0)) +
  theme(strip.text=element_text(hjust=0)) +
  theme(text=element_text(family="serif")) +
  theme(plot.title=element_text(size=16, hjust=0, face="bold", color="black")) +
  theme(legend.position = "none") +
  theme(panel.grid.major.x=element_line(colour="lightgrey",linetype=1))


#############################
## STM Analysis of Sermons ##
library(stm)
metadata<-sermons[,5:18]

## Process the sermon text
textPros<-textProcessor(documents=sermons$Sermon,
                        metadata=metadata)

# Prepare documents for analysis
out   <- prepDocuments(textPros$documents, textPros$vocab, textPros$meta)
docs  <- out$documents
vocab <- out$vocab
meta  <- out$meta
meta$Denom2 <- as.factor(meta$Denom2)
meta$sex <- as.factor(meta$sex)

## Determine the ideal number of topics (go with between 10 and 20)
n_topics <- seq(10,20,1)

  # Use sentiment valence as a covariate
storage1 <- searchK(out$documents,out$vocab,K=n_topics,
                   prevalence=~Denom2+sex+valence,data=meta)
  # Use emotion scores as covariates
storage2 <- searchK(out$documents,out$vocab,K=n_topics,
                    prevalence=~Denom2+sex+anger+anticipation+
                      disgust+fear+joy+sadness+surprise+trust,
                    data=meta)
windows()
par(bty="l",family="serif")
plot(storage1)
windows()
par(bty="l",family="serif")
plot(storage2)
print(storage1)
print(storage2)

# Select the best number of topics that maximuzes exclusivity and 
# semantic coherence
windows()
par(mfcol=c(1,2),bty="l",family="serif")
plot(storage1$results$exclus,storage1$results$semcoh,
     main="Sentiment Valence Model",
     xlab="Exclusivity",ylab="Semantic Coherence")
text(storage1$results$exclus,storage1$results$semcoh,
     labels=storage1$results$K,cex=0.7,pos=2)
plot(storage2$results$exclus,storage2$results$semcoh,
     main="Emotional Score Model",
     xlab="Exclusivity",ylab="Semantic Coherence")
text(storage2$results$exclus,storage2$results$semcoh,
     labels=storage2$results$K,cex=0.7,pos=2)

# Model Selection
n_topics <- 12
  # Sentiment Model
modelSelect1<-selectModel(out$documents,out$vocab,K=n_topics,
                         prevalence=~Denom2+sex+valence,
                         data=meta,runs=20,seed=15)
  # Emotion Model
modelSelect2<-selectModel(out$documents,out$vocab,K=n_topics,
                          prevalence=~Denom2+sex+anger+anticipation+
                            disgust+fear+joy+sadness+surprise+trust,
                          data=meta,runs=20,seed=20)

windows()
par(mfcol=c(1,2),bty="l",family="serif")
plotModels(modelSelect1,main="Sentiment Valence Model")
plotModels(modelSelect2,main="Emotional Score Model")
  # Choose the model that maximizes exclusivity and semantic coherence
modelFit1<-modelSelect1$runout[[3]]
modelFit2<-modelSelect2$runout[[1]]

  # Model Exploration and Validation
windows()
par(mfcol=c(1,2),bty="l",family="serif")
plot(modelFit1,labeltype=c("frex"),
     main="Top Topics\nSentiment Valence Model")
plot(modelFit2,labeltype=c("frex"),
     main="Top Topics\nEmotional Valence Model")

windows()
par(mfcol=c(1,2),bty="l",family="serif")
topicQuality(model=modelFit1,n=10,documents=out$documents,
             main="Sentiment Valence Model")
topicQuality(model=modelFit2,n=10,documents=out$documents,
             main="Emotional Score Model")

    # List of words associated with the topic
sageLabels(modelFit1,n=10)
sageLabels(modelFit2,n=10)
labelTopics(modelFit1,n=8)
labelTopics(modelFit2,n=8)

  # I found the following topics
topics1<-c("Torah","Relationship with Jesus","Different Stories","Apostle Peter",
          "Letters of Paul","Christmas","Fulfillment","Easter","Repentance",
          "Salvation","Tithing","Doubt")
topics2<-c("Obedience to Parents","Christmas","Doubting Thomas","Torah",
           "Employment Struggles","Settling Debts","Denominational Divisions",
           "Repentance","Letters of Paul","Israel's Exodus from Egypt",
           "Praying for Contentment and Security","Jesus and Marriage")

## Influence of Denomination, Gender, and Sentiment on Sermon Topics
  # Sentiment Valence Model
prep1<-estimateEffect(1:n_topics~Denom2+sex+valence,
                     modelFit1,meta=meta,uncertainty="Global")
  # Emotional Score Model
prep2<-estimateEffect(1:n_topics~Denom2+sex+anger+anticipation+
                        disgust+fear+joy+sadness+surprise+trust,
                      modelFit2,meta=meta,uncertainty="Global")
# ---------------------------------------------------------------------
# Sentiment Valence Model
# ---------------------------------------------------------------------
  # Visualize Topic 1
    # by denomination
windows()
par(mfcol=c(1,2),bty="l",lwd=2,col="black",family="serif")
plot.estimateEffect(prep1,                    
                    covariate="valence",
                    model=modelFit1,
                    topics=prep1$topics[1],
                    method="continuous",
                    xlab="Sentiment Valence",
                    ylab="Expected Topic Proportions",
                    main="",
                    ylim=c(-.1,.3),
                    moderator="Denom2",
                    moderator.value="Evangelical",
                    linecol="grey35",
                    printlegend=F)
plot.estimateEffect(prep1,                    
                    covariate="valence",
                    model=modelFit1,
                    topics=prep1$topics[1],
                    method="continuous",
                    moderator="Denom2",
                    moderator.value="Mainline",
                    linecol="grey65",
                    printlegend=F,add=T)
abline(v=0,lty=1,lwd=2,col="black")
abline(h=0,lty=2,lwd=1,col="black")
legend("top",legend=c("Evangelical","Mainline"),col=c("grey35","grey65"),
       lty=1,bty="n",bg="white")
mtext("Evangelical vs. Mainline Protestant",line=.5)
# by gender
plot.estimateEffect(prep1,                    
                    covariate="valence",
                    model=modelFit1,
                    topics=prep1$topics[1],
                    method="continuous",
                    xlab="Sentiment Valence",
                    ylab="Expected Topic Proportions",
                    main="",
                    ylim=c(-.1,.3),
                    moderator="sex",
                    moderator.value="Male",
                    linecol="grey35",
                    printlegend=F)
plot.estimateEffect(prep1,                    
                    covariate="valence",
                    model=modelFit1,
                    topics=prep1$topics[1],
                    method="continuous",
                    moderator="sex",
                    moderator.value="Female",
                    linecol="grey65",
                    printlegend=F,add=T)
abline(v=0,lty=1,lwd=2,col="black")
abline(h=0,lty=2,lwd=1,col="black")
legend("top",legend=c("Male","Female"),col=c("grey35","grey65"),
       lty=1,bty="n",bg="white")
mtext("Male vs. Female Clergy",line=.5)
title(topics1[1],outer=TRUE,line=-1)
mtext("Sentiment Valence Model",outer=TRUE,font=2,line=-2.25)


# Visualize Topic 2
# by denomination
windows()
par(mfcol=c(1,2),bty="l",lwd=2,col="black",family="serif")
plot.estimateEffect(prep1,                    
                    covariate="valence",
                    model=modelFit1,
                    topics=prep1$topics[2],
                    method="continuous",
                    xlab="Sentiment Valence",
                    ylab="Expected Topic Proportions",
                    main="",
                    ylim=c(-.1,.3),
                    moderator="Denom2",
                    moderator.value="Evangelical",
                    linecol="grey35",
                    printlegend=F)
plot.estimateEffect(prep1,                    
                    covariate="valence",
                    model=modelFit1,
                    topics=prep1$topics[2],
                    method="continuous",
                    moderator="Denom2",
                    moderator.value="Mainline",
                    linecol="grey65",
                    printlegend=F,add=T)
abline(v=0,lty=1,lwd=2,col="black")
abline(h=0,lty=2,lwd=1,col="black")
legend("top",legend=c("Evangelical","Mainline"),col=c("grey35","grey65"),
       lty=1,bty="n",bg="white")
mtext("Evangelical vs. Mainline Protestant",line=.5)
# by gender
plot.estimateEffect(prep1,                    
                    covariate="valence",
                    model=modelFit1,
                    topics=prep1$topics[2],
                    method="continuous",
                    xlab="Sentiment Valence",
                    ylab="Expected Topic Proportions",
                    main="",
                    ylim=c(-.1,.3),
                    moderator="sex",
                    moderator.value="Male",
                    linecol="grey35",
                    printlegend=F)
plot.estimateEffect(prep1,                    
                    covariate="valence",
                    model=modelFit1,
                    topics=prep1$topics[2],
                    method="continuous",
                    moderator="sex",
                    moderator.value="Female",
                    linecol="grey65",
                    printlegend=F,add=T)
abline(v=0,lty=1,lwd=2,col="black")
abline(h=0,lty=2,lwd=1,col="black")
legend("top",legend=c("Male","Female"),col=c("grey35","grey65"),
       lty=1,bty="n",bg="white")
mtext("Male vs. Female Clergy",line=.5)
title(topics1[2],outer=TRUE,line=-1)
mtext("Sentiment Valence Model",outer=TRUE,font=2,line=-2.25)


# Visualize Topic 3
# by denomination
windows()
par(mfcol=c(1,2),bty="l",lwd=2,col="black",family="serif")
plot.estimateEffect(prep1,                    
                    covariate="valence",
                    model=modelFit1,
                    topics=prep1$topics[3],
                    method="continuous",
                    xlab="Sentiment Valence",
                    ylab="Expected Topic Proportions",
                    main="",
                    ylim=c(-.1,.3),
                    moderator="Denom2",
                    moderator.value="Evangelical",
                    linecol="grey35",
                    printlegend=F)
plot.estimateEffect(prep1,                    
                    covariate="valence",
                    model=modelFit1,
                    topics=prep1$topics[3],
                    method="continuous",
                    moderator="Denom2",
                    moderator.value="Mainline",
                    linecol="grey65",
                    printlegend=F,add=T)
abline(v=0,lty=1,lwd=2,col="black")
abline(h=0,lty=2,lwd=1,col="black")
legend("top",legend=c("Evangelical","Mainline"),col=c("grey35","grey65"),
       lty=1,bty="n",bg="white")
mtext("Evangelical vs. Mainline Protestant",line=.5)
# by gender
plot.estimateEffect(prep1,                    
                    covariate="valence",
                    model=modelFit1,
                    topics=prep1$topics[3],
                    method="continuous",
                    xlab="Sentiment Valence",
                    ylab="Expected Topic Proportions",
                    main="",
                    ylim=c(-.1,.3),
                    moderator="sex",
                    moderator.value="Male",
                    linecol="grey35",
                    printlegend=F)
plot.estimateEffect(prep1,                    
                    covariate="valence",
                    model=modelFit1,
                    topics=prep1$topics[3],
                    method="continuous",
                    moderator="sex",
                    moderator.value="Female",
                    linecol="grey65",
                    printlegend=F,add=T)
abline(v=0,lty=1,lwd=2,col="black")
abline(h=0,lty=2,lwd=1,col="black")
legend("top",legend=c("Male","Female"),col=c("grey35","grey65"),
       lty=1,bty="n",bg="white")
mtext("Male vs. Female Clergy",line=.5)
title(topics1[3],outer=TRUE,line=-1)
mtext("Sentiment Valence Model",outer=TRUE,font=2,line=-2.25)


# Visualize Topic 4
# by denomination
windows()
par(mfcol=c(1,2),bty="l",lwd=2,col="black",family="serif")
plot.estimateEffect(prep1,                    
                    covariate="valence",
                    model=modelFit1,
                    topics=prep1$topics[4],
                    method="continuous",
                    xlab="Sentiment Valence",
                    ylab="Expected Topic Proportions",
                    main="",
                    ylim=c(-.1,.3),
                    moderator="Denom2",
                    moderator.value="Evangelical",
                    linecol="grey35",
                    printlegend=F)
plot.estimateEffect(prep1,                    
                    covariate="valence",
                    model=modelFit1,
                    topics=prep1$topics[4],
                    method="continuous",
                    moderator="Denom2",
                    moderator.value="Mainline",
                    linecol="grey65",
                    printlegend=F,add=T)
abline(v=0,lty=1,lwd=2,col="black")
abline(h=0,lty=2,lwd=1,col="black")
legend("top",legend=c("Evangelical","Mainline"),col=c("grey35","grey65"),
       lty=1,bty="n",bg="white")
mtext("Evangelical vs. Mainline Protestant",line=.5)
# by gender
plot.estimateEffect(prep1,                    
                    covariate="valence",
                    model=modelFit1,
                    topics=prep1$topics[4],
                    method="continuous",
                    xlab="Sentiment Valence",
                    ylab="Expected Topic Proportions",
                    main="",
                    ylim=c(-.1,.3),
                    moderator="sex",
                    moderator.value="Male",
                    linecol="grey35",
                    printlegend=F)
plot.estimateEffect(prep1,                    
                    covariate="valence",
                    model=modelFit1,
                    topics=prep1$topics[4],
                    method="continuous",
                    moderator="sex",
                    moderator.value="Female",
                    linecol="grey65",
                    printlegend=F,add=T)
abline(v=0,lty=1,lwd=2,col="black")
abline(h=0,lty=2,lwd=1,col="black")
legend("top",legend=c("Male","Female"),col=c("grey35","grey65"),
       lty=1,bty="n",bg="white")
mtext("Male vs. Female Clergy",line=.5)
title(topics1[4],outer=TRUE,line=-1)
mtext("Sentiment Valence Model",outer=TRUE,font=2,line=-2.25)


# Visualize Topic 5
# by denomination
windows()
par(mfcol=c(1,2),bty="l",lwd=2,col="black",family="serif")
plot.estimateEffect(prep1,                    
                    covariate="valence",
                    model=modelFit1,
                    topics=prep1$topics[5],
                    method="continuous",
                    xlab="Sentiment Valence",
                    ylab="Expected Topic Proportions",
                    main="",
                    ylim=c(-.1,.3),
                    moderator="Denom2",
                    moderator.value="Evangelical",
                    linecol="grey35",
                    printlegend=F)
plot.estimateEffect(prep1,                    
                    covariate="valence",
                    model=modelFit1,
                    topics=prep1$topics[5],
                    method="continuous",
                    moderator="Denom2",
                    moderator.value="Mainline",
                    linecol="grey65",
                    printlegend=F,add=T)
abline(v=0,lty=1,lwd=2,col="black")
abline(h=0,lty=2,lwd=1,col="black")
legend("top",legend=c("Evangelical","Mainline"),col=c("grey35","grey65"),
       lty=1,bty="n",bg="white")
mtext("Evangelical vs. Mainline Protestant",line=.5)
# by gender
plot.estimateEffect(prep1,                    
                    covariate="valence",
                    model=modelFit1,
                    topics=prep1$topics[5],
                    method="continuous",
                    xlab="Sentiment Valence",
                    ylab="Expected Topic Proportions",
                    main="",
                    ylim=c(-.1,.3),
                    moderator="sex",
                    moderator.value="Male",
                    linecol="grey35",
                    printlegend=F)
plot.estimateEffect(prep1,                    
                    covariate="valence",
                    model=modelFit1,
                    topics=prep1$topics[5],
                    method="continuous",
                    moderator="sex",
                    moderator.value="Female",
                    linecol="grey65",
                    printlegend=F,add=T)
abline(v=0,lty=1,lwd=2,col="black")
abline(h=0,lty=2,lwd=1,col="black")
legend("top",legend=c("Male","Female"),col=c("grey35","grey65"),
       lty=1,bty="n",bg="white")
mtext("Male vs. Female Clergy",line=.5)
title(topics1[5],outer=TRUE,line=-1)
mtext("Sentiment Valence Model",outer=TRUE,font=2,line=-2.25)


# Visualize Topic 6
# by denomination
windows()
par(mfcol=c(1,2),bty="l",lwd=2,col="black",family="serif")
plot.estimateEffect(prep1,                    
                    covariate="valence",
                    model=modelFit1,
                    topics=prep1$topics[6],
                    method="continuous",
                    xlab="Sentiment Valence",
                    ylab="Expected Topic Proportions",
                    main="",
                    ylim=c(-.1,.3),
                    moderator="Denom2",
                    moderator.value="Evangelical",
                    linecol="grey35",
                    printlegend=F)
plot.estimateEffect(prep1,                    
                    covariate="valence",
                    model=modelFit1,
                    topics=prep1$topics[6],
                    method="continuous",
                    moderator="Denom2",
                    moderator.value="Mainline",
                    linecol="grey65",
                    printlegend=F,add=T)
abline(v=0,lty=1,lwd=2,col="black")
abline(h=0,lty=2,lwd=1,col="black")
legend("top",legend=c("Evangelical","Mainline"),col=c("grey35","grey65"),
       lty=1,bty="n",bg="white")
mtext("Evangelical vs. Mainline Protestant",line=.5)
# by gender
plot.estimateEffect(prep1,                    
                    covariate="valence",
                    model=modelFit1,
                    topics=prep1$topics[6],
                    method="continuous",
                    xlab="Sentiment Valence",
                    ylab="Expected Topic Proportions",
                    main="",
                    ylim=c(-.1,.3),
                    moderator="sex",
                    moderator.value="Male",
                    linecol="grey35",
                    printlegend=F)
plot.estimateEffect(prep1,                    
                    covariate="valence",
                    model=modelFit1,
                    topics=prep1$topics[6],
                    method="continuous",
                    moderator="sex",
                    moderator.value="Female",
                    linecol="grey65",
                    printlegend=F,add=T)
abline(v=0,lty=1,lwd=2,col="black")
abline(h=0,lty=2,lwd=1,col="black")
legend("top",legend=c("Male","Female"),col=c("grey35","grey65"),
       lty=1,bty="n",bg="white")
mtext("Male vs. Female Clergy",line=.5)
title(topics1[6],outer=TRUE,line=-1)
mtext("Sentiment Valence Model",outer=TRUE,font=2,line=-2.25)


# Visualize Topic 7
# by denomination
windows()
par(mfcol=c(1,2),bty="l",lwd=2,col="black",family="serif")
plot.estimateEffect(prep1,                    
                    covariate="valence",
                    model=modelFit1,
                    topics=prep1$topics[7],
                    method="continuous",
                    xlab="Sentiment Valence",
                    ylab="Expected Topic Proportions",
                    main="",
                    ylim=c(-.1,.3),
                    moderator="Denom2",
                    moderator.value="Evangelical",
                    linecol="grey35",
                    printlegend=F)
plot.estimateEffect(prep1,                    
                    covariate="valence",
                    model=modelFit1,
                    topics=prep1$topics[7],
                    method="continuous",
                    moderator="Denom2",
                    moderator.value="Mainline",
                    linecol="grey65",
                    printlegend=F,add=T)
abline(v=0,lty=1,lwd=2,col="black")
abline(h=0,lty=2,lwd=1,col="black")
legend("top",legend=c("Evangelical","Mainline"),col=c("grey35","grey65"),
       lty=1,bty="n",bg="white")
mtext("Evangelical vs. Mainline Protestant",line=.5)
# by gender
plot.estimateEffect(prep1,                    
                    covariate="valence",
                    model=modelFit1,
                    topics=prep1$topics[7],
                    method="continuous",
                    xlab="Sentiment Valence",
                    ylab="Expected Topic Proportions",
                    main="",
                    ylim=c(-.1,.3),
                    moderator="sex",
                    moderator.value="Male",
                    linecol="grey35",
                    printlegend=F)
plot.estimateEffect(prep1,                    
                    covariate="valence",
                    model=modelFit1,
                    topics=prep1$topics[7],
                    method="continuous",
                    moderator="sex",
                    moderator.value="Female",
                    linecol="grey65",
                    printlegend=F,add=T)
abline(v=0,lty=1,lwd=2,col="black")
abline(h=0,lty=2,lwd=1,col="black")
legend("top",legend=c("Male","Female"),col=c("grey35","grey65"),
       lty=1,bty="n",bg="white")
mtext("Male vs. Female Clergy",line=.5)
title(topics1[7],outer=TRUE,line=-1)
mtext("Sentiment Valence Model",outer=TRUE,font=2,line=-2.25)


# Visualize Topic 8
# by denomination
windows()
par(mfcol=c(1,2),bty="l",lwd=2,col="black",family="serif")
plot.estimateEffect(prep1,                    
                    covariate="valence",
                    model=modelFit1,
                    topics=prep1$topics[8],
                    method="continuous",
                    xlab="Sentiment Valence",
                    ylab="Expected Topic Proportions",
                    main="",
                    ylim=c(-.1,.3),
                    moderator="Denom2",
                    moderator.value="Evangelical",
                    linecol="grey35",
                    printlegend=F)
plot.estimateEffect(prep1,                    
                    covariate="valence",
                    model=modelFit1,
                    topics=prep1$topics[8],
                    method="continuous",
                    moderator="Denom2",
                    moderator.value="Mainline",
                    linecol="grey65",
                    printlegend=F,add=T)
abline(v=0,lty=1,lwd=2,col="black")
abline(h=0,lty=2,lwd=1,col="black")
legend("top",legend=c("Evangelical","Mainline"),col=c("grey35","grey65"),
       lty=1,bty="n",bg="white")
mtext("Evangelical vs. Mainline Protestant",line=.5)
# by gender
plot.estimateEffect(prep1,                    
                    covariate="valence",
                    model=modelFit1,
                    topics=prep1$topics[8],
                    method="continuous",
                    xlab="Sentiment Valence",
                    ylab="Expected Topic Proportions",
                    main="",
                    ylim=c(-.1,.7),
                    moderator="sex",
                    moderator.value="Male",
                    linecol="grey35",
                    printlegend=F)
plot.estimateEffect(prep1,                    
                    covariate="valence",
                    model=modelFit1,
                    topics=prep1$topics[8],
                    method="continuous",
                    moderator="sex",
                    moderator.value="Female",
                    linecol="grey65",
                    printlegend=F,add=T)
abline(v=0,lty=1,lwd=2,col="black")
abline(h=0,lty=2,lwd=1,col="black")
legend("top",legend=c("Male","Female"),col=c("grey35","grey65"),
       lty=1,bty="n",bg="white")
mtext("Male vs. Female Clergy",line=.5)
title(topics1[8],outer=TRUE,line=-1)
mtext("Sentiment Valence Model",outer=TRUE,font=2,line=-2.25)


# Visualize Topic 9
# by denomination
windows()
par(mfcol=c(1,2),bty="l",lwd=2,col="black",family="serif")
plot.estimateEffect(prep1,                    
                    covariate="valence",
                    model=modelFit1,
                    topics=prep1$topics[9],
                    method="continuous",
                    xlab="Sentiment Valence",
                    ylab="Expected Topic Proportions",
                    main="",
                    ylim=c(-.1,.4),
                    moderator="Denom2",
                    moderator.value="Evangelical",
                    linecol="grey35",
                    printlegend=F)
plot.estimateEffect(prep1,                    
                    covariate="valence",
                    model=modelFit1,
                    topics=prep1$topics[9],
                    method="continuous",
                    moderator="Denom2",
                    moderator.value="Mainline",
                    linecol="grey65",
                    printlegend=F,add=T)
abline(v=0,lty=1,lwd=2,col="black")
abline(h=0,lty=2,lwd=1,col="black")
legend("top",legend=c("Evangelical","Mainline"),col=c("grey35","grey65"),
       lty=1,bty="n",bg="white")
mtext("Evangelical vs. Mainline Protestant",line=.5)
# by gender
plot.estimateEffect(prep1,                    
                    covariate="valence",
                    model=modelFit1,
                    topics=prep1$topics[9],
                    method="continuous",
                    xlab="Sentiment Valence",
                    ylab="Expected Topic Proportions",
                    main="",
                    ylim=c(-.2,.3),
                    moderator="sex",
                    moderator.value="Male",
                    linecol="grey35",
                    printlegend=F)
plot.estimateEffect(prep1,                    
                    covariate="valence",
                    model=modelFit1,
                    topics=prep1$topics[9],
                    method="continuous",
                    moderator="sex",
                    moderator.value="Female",
                    linecol="grey65",
                    printlegend=F,add=T)
abline(v=0,lty=1,lwd=2,col="black")
abline(h=0,lty=2,lwd=1,col="black")
legend("top",legend=c("Male","Female"),col=c("grey35","grey65"),
       lty=1,bty="n",bg="white")
mtext("Male vs. Female Clergy",line=.5)
title(topics1[9],outer=TRUE,line=-1)
mtext("Sentiment Valence Model",outer=TRUE,font=2,line=-2.25)


# Visualize Topic 10
# by denomination
windows()
par(mfcol=c(1,2),bty="l",lwd=2,col="black",family="serif")
plot.estimateEffect(prep1,                    
                    covariate="valence",
                    model=modelFit1,
                    topics=prep1$topics[10],
                    method="continuous",
                    xlab="Sentiment Valence",
                    ylab="Expected Topic Proportions",
                    main="",
                    ylim=c(-.1,.4),
                    moderator="Denom2",
                    moderator.value="Evangelical",
                    linecol="grey35",
                    printlegend=F)
plot.estimateEffect(prep1,                    
                    covariate="valence",
                    model=modelFit1,
                    topics=prep1$topics[10],
                    method="continuous",
                    moderator="Denom2",
                    moderator.value="Mainline",
                    linecol="grey65",
                    printlegend=F,add=T)
abline(v=0,lty=1,lwd=2,col="black")
abline(h=0,lty=2,lwd=1,col="black")
legend("top",legend=c("Evangelical","Mainline"),col=c("grey35","grey65"),
       lty=1,bty="n",bg="white")
mtext("Evangelical vs. Mainline Protestant",line=.5)
# by gender
plot.estimateEffect(prep1,                    
                    covariate="valence",
                    model=modelFit1,
                    topics=prep1$topics[10],
                    method="continuous",
                    xlab="Sentiment Valence",
                    ylab="Expected Topic Proportions",
                    main="",
                    ylim=c(-.2,.3),
                    moderator="sex",
                    moderator.value="Male",
                    linecol="grey35",
                    printlegend=F)
plot.estimateEffect(prep1,                    
                    covariate="valence",
                    model=modelFit1,
                    topics=prep1$topics[10],
                    method="continuous",
                    moderator="sex",
                    moderator.value="Female",
                    linecol="grey65",
                    printlegend=F,add=T)
abline(v=0,lty=1,lwd=2,col="black")
abline(h=0,lty=2,lwd=1,col="black")
legend("top",legend=c("Male","Female"),col=c("grey35","grey65"),
       lty=1,bty="n",bg="white")
mtext("Male vs. Female Clergy",line=.5)
title(topics1[10],outer=TRUE,line=-1)
mtext("Sentiment Valence Model",outer=TRUE,font=2,line=-2.25)


# Visualize Topic 11
# by denomination
windows()
par(mfcol=c(1,2),bty="l",lwd=2,col="black",family="serif")
plot.estimateEffect(prep1,                    
                    covariate="valence",
                    model=modelFit1,
                    topics=prep1$topics[11],
                    method="continuous",
                    xlab="Sentiment Valence",
                    ylab="Expected Topic Proportions",
                    main="",
                    ylim=c(-.1,.3),
                    moderator="Denom2",
                    moderator.value="Evangelical",
                    linecol="grey35",
                    printlegend=F)
plot.estimateEffect(prep1,                    
                    covariate="valence",
                    model=modelFit1,
                    topics=prep1$topics[11],
                    method="continuous",
                    moderator="Denom2",
                    moderator.value="Mainline",
                    linecol="grey65",
                    printlegend=F,add=T)
abline(v=0,lty=1,lwd=2,col="black")
abline(h=0,lty=2,lwd=1,col="black")
legend("top",legend=c("Evangelical","Mainline"),col=c("grey35","grey65"),
       lty=1,bty="n",bg="white")
mtext("Evangelical vs. Mainline Protestant",line=.5)
# by gender
plot.estimateEffect(prep1,                    
                    covariate="valence",
                    model=modelFit1,
                    topics=prep1$topics[11],
                    method="continuous",
                    xlab="Sentiment Valence",
                    ylab="Expected Topic Proportions",
                    main="",
                    ylim=c(-.1,.55),
                    moderator="sex",
                    moderator.value="Male",
                    linecol="grey35",
                    printlegend=F)
plot.estimateEffect(prep1,                    
                    covariate="valence",
                    model=modelFit1,
                    topics=prep1$topics[11],
                    method="continuous",
                    moderator="sex",
                    moderator.value="Female",
                    linecol="grey65",
                    printlegend=F,add=T)
abline(v=0,lty=1,lwd=2,col="black")
abline(h=0,lty=2,lwd=1,col="black")
legend("top",legend=c("Male","Female"),col=c("grey35","grey65"),
       lty=1,bty="n",bg="white")
mtext("Male vs. Female Clergy",line=.5)
title(topics1[11],outer=TRUE,line=-1)
mtext("Sentiment Valence Model",outer=TRUE,font=2,line=-2.25)


# Visualize Topic 12
# by denomination
windows()
par(mfcol=c(1,2),bty="l",lwd=2,col="black",family="serif")
plot.estimateEffect(prep1,                    
                    covariate="valence",
                    model=modelFit1,
                    topics=prep1$topics[12],
                    method="continuous",
                    xlab="Sentiment Valence",
                    ylab="Expected Topic Proportions",
                    main="",
                    ylim=c(-.1,.3),
                    moderator="Denom2",
                    moderator.value="Evangelical",
                    linecol="grey35",
                    printlegend=F)
plot.estimateEffect(prep1,                    
                    covariate="valence",
                    model=modelFit1,
                    topics=prep1$topics[12],
                    method="continuous",
                    moderator="Denom2",
                    moderator.value="Mainline",
                    linecol="grey65",
                    printlegend=F,add=T)
abline(v=0,lty=1,lwd=2,col="black")
abline(h=0,lty=2,lwd=1,col="black")
legend("top",legend=c("Evangelical","Mainline"),col=c("grey35","grey65"),
       lty=1,bty="n",bg="white")
mtext("Evangelical vs. Mainline Protestant",line=.5)
# by gender
plot.estimateEffect(prep1,                    
                    covariate="valence",
                    model=modelFit1,
                    topics=prep1$topics[12],
                    method="continuous",
                    xlab="Sentiment Valence",
                    ylab="Expected Topic Proportions",
                    main="",
                    ylim=c(-.1,.3),
                    moderator="sex",
                    moderator.value="Male",
                    linecol="grey35",
                    printlegend=F)
plot.estimateEffect(prep1,                    
                    covariate="valence",
                    model=modelFit1,
                    topics=prep1$topics[12],
                    method="continuous",
                    moderator="sex",
                    moderator.value="Female",
                    linecol="grey65",
                    printlegend=F,add=T)
abline(v=0,lty=1,lwd=2,col="black")
abline(h=0,lty=2,lwd=1,col="black")
legend("top",legend=c("Male","Female"),col=c("grey35","grey65"),
       lty=1,bty="n",bg="white")
mtext("Male vs. Female Clergy",line=.5)
title(topics1[12],outer=TRUE,line=-1)
mtext("Sentiment Valence Model",outer=TRUE,font=2,line=-2.25)



# ---------------------------------------------------------------------
# Emotional Score Model
# ---------------------------------------------------------------------

# Visualize Topic 1
# by denomination
windows()
par(mfcol=c(1,2),bty="l",lwd=2,col="black",family="serif")
plot.estimateEffect(prep2,                    
                    covariate="fear",
                    model=modelFit2,
                    topics=prep2$topics[1],
                    method="continuous",
                    xlab="Fear",
                    ylab="Expected Topic Proportions",
                    main="",
                    ylim=c(-.1,.3),
                    moderator="Denom2",
                    moderator.value="Evangelical",
                    linecol="grey35",
                    printlegend=F)
plot.estimateEffect(prep2,                    
                    covariate="fear",
                    model=modelFit2,
                    topics=prep2$topics[1],
                    method="continuous",
                    moderator="Denom2",
                    moderator.value="Mainline",
                    linecol="grey65",
                    printlegend=F,add=T)
abline(v=0,lty=1,lwd=2,col="black")
abline(h=0,lty=2,lwd=1,col="black")
legend("top",legend=c("Evangelical","Mainline"),col=c("grey35","grey65"),
       lty=1,bty="n",bg="white")
mtext("Evangelical vs. Mainline Protestant",line=.5)
# by gender
plot.estimateEffect(prep2,                    
                    covariate="fear",
                    model=modelFit2,
                    topics=prep2$topics[1],
                    method="continuous",
                    xlab="Fear",
                    ylab="Expected Topic Proportions",
                    main="",
                    ylim=c(-.1,.3),
                    moderator="sex",
                    moderator.value="Male",
                    linecol="grey35",
                    printlegend=F)
plot.estimateEffect(prep2,                    
                    covariate="fear",
                    model=modelFit2,
                    topics=prep2$topics[1],
                    method="continuous",
                    moderator="sex",
                    moderator.value="Female",
                    linecol="grey65",
                    printlegend=F,add=T)
abline(v=0,lty=1,lwd=2,col="black")
abline(h=0,lty=2,lwd=1,col="black")
legend("top",legend=c("Male","Female"),col=c("grey35","grey65"),
       lty=1,bty="n",bg="white")
mtext("Male vs. Female Clergy",line=.5)
title(topics2[1],outer=TRUE,line=-1)
mtext("Emotional Score Model (Fear)",outer=TRUE,font=2,line=-2.25)


# Visualize Topic 2
# by denomination
windows()
par(mfcol=c(1,2),bty="l",lwd=2,col="black",family="serif")
plot.estimateEffect(prep2,                    
                    covariate="fear",
                    model=modelFit2,
                    topics=prep2$topics[2],
                    method="continuous",
                    xlab="Fear",
                    ylab="Expected Topic Proportions",
                    main="",
                    ylim=c(-.1,.3),
                    moderator="Denom2",
                    moderator.value="Evangelical",
                    linecol="grey35",
                    printlegend=F)
plot.estimateEffect(prep2,                    
                    covariate="fear",
                    model=modelFit2,
                    topics=prep2$topics[2],
                    method="continuous",
                    moderator="Denom2",
                    moderator.value="Mainline",
                    linecol="grey65",
                    printlegend=F,add=T)
abline(v=0,lty=1,lwd=2,col="black")
abline(h=0,lty=2,lwd=1,col="black")
legend("top",legend=c("Evangelical","Mainline"),col=c("grey35","grey65"),
       lty=1,bty="n",bg="white")
mtext("Evangelical vs. Mainline Protestant",line=.5)
# by gender
plot.estimateEffect(prep2,                    
                    covariate="fear",
                    model=modelFit2,
                    topics=prep2$topics[2],
                    method="continuous",
                    xlab="Fear",
                    ylab="Expected Topic Proportions",
                    main="",
                    ylim=c(-.1,.3),
                    moderator="sex",
                    moderator.value="Male",
                    linecol="grey35",
                    printlegend=F)
plot.estimateEffect(prep2,                    
                    covariate="fear",
                    model=modelFit2,
                    topics=prep2$topics[2],
                    method="continuous",
                    moderator="sex",
                    moderator.value="Female",
                    linecol="grey65",
                    printlegend=F,add=T)
abline(v=0,lty=1,lwd=2,col="black")
abline(h=0,lty=2,lwd=1,col="black")
legend("top",legend=c("Male","Female"),col=c("grey35","grey65"),
       lty=1,bty="n",bg="white")
mtext("Male vs. Female Clergy",line=.5)
title(topics2[2],outer=TRUE,line=-1)
mtext("Emotional Score Model (Fear)",outer=TRUE,font=2,line=-2.25)


# Visualize Topic 3
# by denomination
windows()
par(mfcol=c(1,2),bty="l",lwd=2,col="black",family="serif")
plot.estimateEffect(prep2,                    
                    covariate="fear",
                    model=modelFit2,
                    topics=prep2$topics[3],
                    method="continuous",
                    xlab="Fear",
                    ylab="Expected Topic Proportions",
                    main="",
                    ylim=c(-.1,.3),
                    moderator="Denom2",
                    moderator.value="Evangelical",
                    linecol="grey35",
                    printlegend=F)
plot.estimateEffect(prep2,                    
                    covariate="fear",
                    model=modelFit2,
                    topics=prep2$topics[3],
                    method="continuous",
                    moderator="Denom2",
                    moderator.value="Mainline",
                    linecol="grey65",
                    printlegend=F,add=T)
abline(v=0,lty=1,lwd=2,col="black")
abline(h=0,lty=2,lwd=1,col="black")
legend("top",legend=c("Evangelical","Mainline"),col=c("grey35","grey65"),
       lty=1,bty="n",bg="white")
mtext("Evangelical vs. Mainline Protestant",line=.5)
# by gender
plot.estimateEffect(prep2,                    
                    covariate="fear",
                    model=modelFit2,
                    topics=prep2$topics[3],
                    method="continuous",
                    xlab="Fear",
                    ylab="Expected Topic Proportions",
                    main="",
                    ylim=c(-.1,1),
                    moderator="sex",
                    moderator.value="Male",
                    linecol="grey35",
                    printlegend=F)
plot.estimateEffect(prep2,                    
                    covariate="fear",
                    model=modelFit2,
                    topics=prep2$topics[3],
                    method="continuous",
                    moderator="sex",
                    moderator.value="Female",
                    linecol="grey65",
                    printlegend=F,add=T)
abline(v=0,lty=1,lwd=2,col="black")
abline(h=0,lty=2,lwd=1,col="black")
legend("top",legend=c("Male","Female"),col=c("grey35","grey65"),
       lty=1,bty="n",bg="white")
mtext("Male vs. Female Clergy",line=.5)
title(topics2[3],outer=TRUE,line=-1)
mtext("Emotional Score Model (Fear)",outer=TRUE,font=2,line=-2.25)


# Visualize Topic 4
# by denomination
windows()
par(mfcol=c(1,2),bty="l",lwd=2,col="black",family="serif")
plot.estimateEffect(prep2,                    
                    covariate="fear",
                    model=modelFit2,
                    topics=prep2$topics[4],
                    method="continuous",
                    xlab="Fear",
                    ylab="Expected Topic Proportions",
                    main="",
                    ylim=c(-.1,.3),
                    moderator="Denom2",
                    moderator.value="Evangelical",
                    linecol="grey35",
                    printlegend=F)
plot.estimateEffect(prep2,                    
                    covariate="fear",
                    model=modelFit2,
                    topics=prep2$topics[4],
                    method="continuous",
                    moderator="Denom2",
                    moderator.value="Mainline",
                    linecol="grey65",
                    printlegend=F,add=T)
abline(v=0,lty=1,lwd=2,col="black")
abline(h=0,lty=2,lwd=1,col="black")
legend("top",legend=c("Evangelical","Mainline"),col=c("grey35","grey65"),
       lty=1,bty="n",bg="white")
mtext("Evangelical vs. Mainline Protestant",line=.5)
# by gender
plot.estimateEffect(prep2,                    
                    covariate="fear",
                    model=modelFit2,
                    topics=prep2$topics[4],
                    method="continuous",
                    xlab="Fear",
                    ylab="Expected Topic Proportions",
                    main="",
                    ylim=c(-.1,.3),
                    moderator="sex",
                    moderator.value="Male",
                    linecol="grey35",
                    printlegend=F)
plot.estimateEffect(prep2,                    
                    covariate="fear",
                    model=modelFit2,
                    topics=prep2$topics[4],
                    method="continuous",
                    moderator="sex",
                    moderator.value="Female",
                    linecol="grey65",
                    printlegend=F,add=T)
abline(v=0,lty=1,lwd=2,col="black")
abline(h=0,lty=2,lwd=1,col="black")
legend("top",legend=c("Male","Female"),col=c("grey35","grey65"),
       lty=1,bty="n",bg="white")
mtext("Male vs. Female Clergy",line=.5)
title(topics2[4],outer=TRUE,line=-1)
mtext("Emotional Score Model (Fear)",outer=TRUE,font=2,line=-2.25)


# Visualize Topic 5
# by denomination
windows()
par(mfcol=c(1,2),bty="l",lwd=2,col="black",family="serif")
plot.estimateEffect(prep2,                    
                    covariate="fear",
                    model=modelFit2,
                    topics=prep2$topics[5],
                    method="continuous",
                    xlab="Fear",
                    ylab="Expected Topic Proportions",
                    main="",
                    ylim=c(-.1,.3),
                    moderator="Denom2",
                    moderator.value="Evangelical",
                    linecol="grey35",
                    printlegend=F)
plot.estimateEffect(prep2,                    
                    covariate="fear",
                    model=modelFit2,
                    topics=prep2$topics[5],
                    method="continuous",
                    moderator="Denom2",
                    moderator.value="Mainline",
                    linecol="grey65",
                    printlegend=F,add=T)
abline(v=0,lty=1,lwd=2,col="black")
abline(h=0,lty=2,lwd=1,col="black")
legend("top",legend=c("Evangelical","Mainline"),col=c("grey35","grey65"),
       lty=1,bty="n",bg="white")
mtext("Evangelical vs. Mainline Protestant",line=.5)
# by gender
plot.estimateEffect(prep2,                    
                    covariate="fear",
                    model=modelFit2,
                    topics=prep2$topics[5],
                    method="continuous",
                    xlab="Fear",
                    ylab="Expected Topic Proportions",
                    main="",
                    ylim=c(-.1,.3),
                    moderator="sex",
                    moderator.value="Male",
                    linecol="grey35",
                    printlegend=F)
plot.estimateEffect(prep2,                    
                    covariate="fear",
                    model=modelFit2,
                    topics=prep2$topics[5],
                    method="continuous",
                    moderator="sex",
                    moderator.value="Female",
                    linecol="grey65",
                    printlegend=F,add=T)
abline(v=0,lty=1,lwd=2,col="black")
abline(h=0,lty=2,lwd=1,col="black")
legend("top",legend=c("Male","Female"),col=c("grey35","grey65"),
       lty=1,bty="n",bg="white")
mtext("Male vs. Female Clergy",line=.5)
title(topics2[5],outer=TRUE,line=-1)
mtext("Emotional Score Model (Fear)",outer=TRUE,font=2,line=-2.25)


# Visualize Topic 6
# by denomination
windows()
par(mfcol=c(1,2),bty="l",lwd=2,col="black",family="serif")
plot.estimateEffect(prep2,                    
                    covariate="fear",
                    model=modelFit2,
                    topics=prep2$topics[6],
                    method="continuous",
                    xlab="Fear",
                    ylab="Expected Topic Proportions",
                    main="",
                    ylim=c(-.1,.4),
                    moderator="Denom2",
                    moderator.value="Evangelical",
                    linecol="grey35",
                    printlegend=F)
plot.estimateEffect(prep2,                    
                    covariate="fear",
                    model=modelFit2,
                    topics=prep2$topics[6],
                    method="continuous",
                    moderator="Denom2",
                    moderator.value="Mainline",
                    linecol="grey65",
                    printlegend=F,add=T)
abline(v=0,lty=1,lwd=2,col="black")
abline(h=0,lty=2,lwd=1,col="black")
legend("top",legend=c("Evangelical","Mainline"),col=c("grey35","grey65"),
       lty=1,bty="n",bg="white")
mtext("Evangelical vs. Mainline Protestant",line=.5)
# by gender
plot.estimateEffect(prep2,                    
                    covariate="fear",
                    model=modelFit2,
                    topics=prep2$topics[6],
                    method="continuous",
                    xlab="Fear",
                    ylab="Expected Topic Proportions",
                    main="",
                    ylim=c(-.3,.3),
                    moderator="sex",
                    moderator.value="Male",
                    linecol="grey35",
                    printlegend=F)
plot.estimateEffect(prep2,                    
                    covariate="fear",
                    model=modelFit2,
                    topics=prep2$topics[6],
                    method="continuous",
                    moderator="sex",
                    moderator.value="Female",
                    linecol="grey65",
                    printlegend=F,add=T)
abline(v=0,lty=1,lwd=2,col="black")
abline(h=0,lty=2,lwd=1,col="black")
legend("top",legend=c("Male","Female"),col=c("grey35","grey65"),
       lty=1,bty="n",bg="white")
mtext("Male vs. Female Clergy",line=.5)
title(topics2[6],outer=TRUE,line=-1)
mtext("Emotional Score Model (Fear)",outer=TRUE,font=2,line=-2.25)


# Visualize Topic 7
# by denomination
windows()
par(mfcol=c(1,2),bty="l",lwd=2,col="black",family="serif")
plot.estimateEffect(prep2,                    
                    covariate="fear",
                    model=modelFit2,
                    topics=prep2$topics[7],
                    method="continuous",
                    xlab="Fear",
                    ylab="Expected Topic Proportions",
                    main="",
                    ylim=c(-.1,.3),
                    moderator="Denom2",
                    moderator.value="Evangelical",
                    linecol="grey35",
                    printlegend=F)
plot.estimateEffect(prep2,                    
                    covariate="fear",
                    model=modelFit2,
                    topics=prep2$topics[7],
                    method="continuous",
                    moderator="Denom2",
                    moderator.value="Mainline",
                    linecol="grey65",
                    printlegend=F,add=T)
abline(v=0,lty=1,lwd=2,col="black")
abline(h=0,lty=2,lwd=1,col="black")
legend("top",legend=c("Evangelical","Mainline"),col=c("grey35","grey65"),
       lty=1,bty="n",bg="white")
mtext("Evangelical vs. Mainline Protestant",line=.5)
# by gender
plot.estimateEffect(prep2,                    
                    covariate="fear",
                    model=modelFit2,
                    topics=prep2$topics[7],
                    method="continuous",
                    xlab="Fear",
                    ylab="Expected Topic Proportions",
                    main="",
                    ylim=c(-.1,.4),
                    moderator="sex",
                    moderator.value="Male",
                    linecol="grey35",
                    printlegend=F)
plot.estimateEffect(prep2,                    
                    covariate="fear",
                    model=modelFit2,
                    topics=prep2$topics[7],
                    method="continuous",
                    moderator="sex",
                    moderator.value="Female",
                    linecol="grey65",
                    printlegend=F,add=T)
abline(v=0,lty=1,lwd=2,col="black")
abline(h=0,lty=2,lwd=1,col="black")
legend("top",legend=c("Male","Female"),col=c("grey35","grey65"),
       lty=1,bty="n",bg="white")
mtext("Male vs. Female Clergy",line=.5)
title(topics2[7],outer=TRUE,line=-1)
mtext("Emotional Score Model (Fear)",outer=TRUE,font=2,line=-2.25)


# Visualize Topic 8
# by denomination
windows()
par(mfcol=c(1,2),bty="l",lwd=2,col="black",family="serif")
plot.estimateEffect(prep2,                    
                    covariate="fear",
                    model=modelFit2,
                    topics=prep2$topics[8],
                    method="continuous",
                    xlab="Fear",
                    ylab="Expected Topic Proportions",
                    main="",
                    ylim=c(-.1,.5),
                    moderator="Denom2",
                    moderator.value="Evangelical",
                    linecol="grey35",
                    printlegend=F)
plot.estimateEffect(prep2,                    
                    covariate="fear",
                    model=modelFit2,
                    topics=prep2$topics[8],
                    method="continuous",
                    moderator="Denom2",
                    moderator.value="Mainline",
                    linecol="grey65",
                    printlegend=F,add=T)
abline(v=0,lty=1,lwd=2,col="black")
abline(h=0,lty=2,lwd=1,col="black")
legend("top",legend=c("Evangelical","Mainline"),col=c("grey35","grey65"),
       lty=1,bty="n",bg="white")
mtext("Evangelical vs. Mainline Protestant",line=.5)
# by gender
plot.estimateEffect(prep2,                    
                    covariate="fear",
                    model=modelFit2,
                    topics=prep2$topics[8],
                    method="continuous",
                    xlab="Fear",
                    ylab="Expected Topic Proportions",
                    main="",
                    ylim=c(-.3,.3),
                    moderator="sex",
                    moderator.value="Male",
                    linecol="grey35",
                    printlegend=F)
plot.estimateEffect(prep2,                    
                    covariate="fear",
                    model=modelFit2,
                    topics=prep2$topics[8],
                    method="continuous",
                    moderator="sex",
                    moderator.value="Female",
                    linecol="grey65",
                    printlegend=F,add=T)
abline(v=0,lty=1,lwd=2,col="black")
abline(h=0,lty=2,lwd=1,col="black")
legend("top",legend=c("Male","Female"),col=c("grey35","grey65"),
       lty=1,bty="n",bg="white")
mtext("Male vs. Female Clergy",line=.5)
title(topics2[8],outer=TRUE,line=-1)
mtext("Emotional Score Model (Fear)",outer=TRUE,font=2,line=-2.25)


# Visualize Topic 9
# by denomination
windows()
par(mfcol=c(1,2),bty="l",lwd=2,col="black",family="serif")
plot.estimateEffect(prep2,                    
                    covariate="fear",
                    model=modelFit2,
                    topics=prep2$topics[9],
                    method="continuous",
                    xlab="Fear",
                    ylab="Expected Topic Proportions",
                    main="",
                    ylim=c(-.2,.4),
                    moderator="Denom2",
                    moderator.value="Evangelical",
                    linecol="grey35",
                    printlegend=F)
plot.estimateEffect(prep2,                    
                    covariate="fear",
                    model=modelFit2,
                    topics=prep2$topics[9],
                    method="continuous",
                    moderator="Denom2",
                    moderator.value="Mainline",
                    linecol="grey65",
                    printlegend=F,add=T)
abline(v=0,lty=1,lwd=2,col="black")
abline(h=0,lty=2,lwd=1,col="black")
legend("top",legend=c("Evangelical","Mainline"),col=c("grey35","grey65"),
       lty=1,bty="n",bg="white")
mtext("Evangelical vs. Mainline Protestant",line=.5)
# by gender
plot.estimateEffect(prep2,                    
                    covariate="fear",
                    model=modelFit2,
                    topics=prep2$topics[9],
                    method="continuous",
                    xlab="Fear",
                    ylab="Expected Topic Proportions",
                    main="",
                    ylim=c(-.2,.4),
                    moderator="sex",
                    moderator.value="Male",
                    linecol="grey35",
                    printlegend=F)
plot.estimateEffect(prep2,                    
                    covariate="fear",
                    model=modelFit2,
                    topics=prep2$topics[9],
                    method="continuous",
                    moderator="sex",
                    moderator.value="Female",
                    linecol="grey65",
                    printlegend=F,add=T)
abline(v=0,lty=1,lwd=2,col="black")
abline(h=0,lty=2,lwd=1,col="black")
legend("top",legend=c("Male","Female"),col=c("grey35","grey65"),
       lty=1,bty="n",bg="white")
mtext("Male vs. Female Clergy",line=.5)
title(topics2[9],outer=TRUE,line=-1)
mtext("Emotional Score Model (Fear)",outer=TRUE,font=2,line=-2.25)


# Visualize Topic 10
# by denomination
windows()
par(mfcol=c(1,2),bty="l",lwd=2,col="black",family="serif")
plot.estimateEffect(prep2,                    
                    covariate="fear",
                    model=modelFit2,
                    topics=prep2$topics[10],
                    method="continuous",
                    xlab="Fear",
                    ylab="Expected Topic Proportions",
                    main="",
                    ylim=c(-.1,.3),
                    moderator="Denom2",
                    moderator.value="Evangelical",
                    linecol="grey35",
                    printlegend=F)
plot.estimateEffect(prep2,                    
                    covariate="fear",
                    model=modelFit2,
                    topics=prep2$topics[10],
                    method="continuous",
                    moderator="Denom2",
                    moderator.value="Mainline",
                    linecol="grey65",
                    printlegend=F,add=T)
abline(v=0,lty=1,lwd=2,col="black")
abline(h=0,lty=2,lwd=1,col="black")
legend("top",legend=c("Evangelical","Mainline"),col=c("grey35","grey65"),
       lty=1,bty="n",bg="white")
mtext("Evangelical vs. Mainline Protestant",line=.5)
# by gender
plot.estimateEffect(prep2,                    
                    covariate="fear",
                    model=modelFit2,
                    topics=prep2$topics[10],
                    method="continuous",
                    xlab="Fear",
                    ylab="Expected Topic Proportions",
                    main="",
                    ylim=c(-.1,.3),
                    moderator="sex",
                    moderator.value="Male",
                    linecol="grey35",
                    printlegend=F)
plot.estimateEffect(prep2,                    
                    covariate="fear",
                    model=modelFit2,
                    topics=prep2$topics[10],
                    method="continuous",
                    moderator="sex",
                    moderator.value="Female",
                    linecol="grey65",
                    printlegend=F,add=T)
abline(v=0,lty=1,lwd=2,col="black")
abline(h=0,lty=2,lwd=1,col="black")
legend("top",legend=c("Male","Female"),col=c("grey35","grey65"),
       lty=1,bty="n",bg="white")
mtext("Male vs. Female Clergy",line=.5)
title(topics2[10],outer=TRUE,line=-1)
mtext("Emotional Score Model (Fear)",outer=TRUE,font=2,line=-2.25)


# Visualize Topic 11
# by denomination
windows()
par(mfcol=c(1,2),bty="l",lwd=2,col="black",family="serif")
plot.estimateEffect(prep2,                    
                    covariate="fear",
                    model=modelFit2,
                    topics=prep2$topics[11],
                    method="continuous",
                    xlab="Fear",
                    ylab="Expected Topic Proportions",
                    main="",
                    ylim=c(-.1,.3),
                    moderator="Denom2",
                    moderator.value="Evangelical",
                    linecol="grey35",
                    printlegend=F)
plot.estimateEffect(prep2,                    
                    covariate="fear",
                    model=modelFit2,
                    topics=prep2$topics[11],
                    method="continuous",
                    moderator="Denom2",
                    moderator.value="Mainline",
                    linecol="grey65",
                    printlegend=F,add=T)
abline(v=0,lty=1,lwd=2,col="black")
abline(h=0,lty=2,lwd=1,col="black")
legend("top",legend=c("Evangelical","Mainline"),col=c("grey35","grey65"),
       lty=1,bty="n",bg="white")
mtext("Evangelical vs. Mainline Protestant",line=.5)
# by gender
plot.estimateEffect(prep2,                    
                    covariate="fear",
                    model=modelFit2,
                    topics=prep2$topics[11],
                    method="continuous",
                    xlab="Fear",
                    ylab="Expected Topic Proportions",
                    main="",
                    ylim=c(-.1,.3),
                    moderator="sex",
                    moderator.value="Male",
                    linecol="grey35",
                    printlegend=F)
plot.estimateEffect(prep2,                    
                    covariate="fear",
                    model=modelFit2,
                    topics=prep2$topics[11],
                    method="continuous",
                    moderator="sex",
                    moderator.value="Female",
                    linecol="grey65",
                    printlegend=F,add=T)
abline(v=0,lty=1,lwd=2,col="black")
abline(h=0,lty=2,lwd=1,col="black")
legend("top",legend=c("Male","Female"),col=c("grey35","grey65"),
       lty=1,bty="n",bg="white")
mtext("Male vs. Female Clergy",line=.5)
title(topics2[11],outer=TRUE,line=-1)
mtext("Emotional Score Model (Fear)",outer=TRUE,font=2,line=-2.25)


# Visualize Topic 12
# by denomination
windows()
par(mfcol=c(1,2),bty="l",lwd=2,col="black",family="serif")
plot.estimateEffect(prep2,                    
                    covariate="fear",
                    model=modelFit2,
                    topics=prep2$topics[12],
                    method="continuous",
                    xlab="Fear",
                    ylab="Expected Topic Proportions",
                    main="",
                    ylim=c(-.1,.3),
                    moderator="Denom2",
                    moderator.value="Evangelical",
                    linecol="grey35",
                    printlegend=F)
plot.estimateEffect(prep2,                    
                    covariate="fear",
                    model=modelFit2,
                    topics=prep2$topics[12],
                    method="continuous",
                    moderator="Denom2",
                    moderator.value="Mainline",
                    linecol="grey65",
                    printlegend=F,add=T)
abline(v=0,lty=1,lwd=2,col="black")
abline(h=0,lty=2,lwd=1,col="black")
legend("top",legend=c("Evangelical","Mainline"),col=c("grey35","grey65"),
       lty=1,bty="n",bg="white")
mtext("Evangelical vs. Mainline Protestant",line=.5)
# by gender
plot.estimateEffect(prep2,                    
                    covariate="fear",
                    model=modelFit2,
                    topics=prep2$topics[12],
                    method="continuous",
                    xlab="Fear",
                    ylab="Expected Topic Proportions",
                    main="",
                    ylim=c(-.1,.3),
                    moderator="sex",
                    moderator.value="Male",
                    linecol="grey35",
                    printlegend=F)
plot.estimateEffect(prep2,                    
                    covariate="fear",
                    model=modelFit2,
                    topics=prep2$topics[12],
                    method="continuous",
                    moderator="sex",
                    moderator.value="Female",
                    linecol="grey65",
                    printlegend=F,add=T)
abline(v=0,lty=1,lwd=2,col="black")
abline(h=0,lty=2,lwd=1,col="black")
legend("top",legend=c("Male","Female"),col=c("grey35","grey65"),
       lty=1,bty="n",bg="white")
mtext("Male vs. Female Clergy",line=.5)
title(topics2[12],outer=TRUE,line=-1)
mtext("Emotional Score Model (Fear)",outer=TRUE,font=2,line=-2.25)


