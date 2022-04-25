library(dplyr)
Data=read.csv('WaterlooTweetsResult.csv',header = T)

#Date Entity
Date=Data%>%filter(Data$Type=="Date")
DateResult=Date[!grepl("://", Date$Entity),]
DateResult%>%count(DateResult$Entity)%>% arrange(desc(n))
#Location
Location=Data%>%filter(Data$Type=="Location")
Location%>%count(Location$Entity)%>% arrange(desc(n))
#Money
Money=Data%>%filter(Data$Type=="Money")
Money%>%count(Money$Entity)%>% arrange(desc(n))
#Organization
Organization=Data%>%filter(Data$Type=="Organization")
Organization%>%count(Organization$Entity)%>% arrange(desc(n))
#Person
Person=Data%>%filter(Data$Type=="Person")
Person%>%count(Person$Entity)%>% arrange(desc(n))
