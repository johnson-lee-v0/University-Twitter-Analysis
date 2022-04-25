install.packages("openNLPmodels.en",repos = 'http://datacube.wu.ac.at/',type='source')
library(openNLP)
library(openNLPmodels.en)
require('NLP')
WT=read.csv('WesternTweets.csv',header=T)
org_annotator=Maxent_Entity_Annotator(kind='organization')
person_annotator=Maxent_Entity_Annotator(kind='person')
date_annotator=Maxent_Entity_Annotator(kind='date')
percent_annotator=Maxent_Entity_Annotator(kind='percentage')
loc_annotator=Maxent_Entity_Annotator(kind='location')
money_annotator=Maxent_Entity_Annotator(kind='money')
WTEntities=data.frame(Post=numeric(), Entity=character(),Position=numeric())

for (post in 1:nrow(WT))
{
  WTText=as.String(WT$message[post])
  WTTokens=annotate(WTText, list(Maxent_Sent_Token_Annotator(), Maxent_Word_Token_Annotator()))
  WTOrgPersTokens=annotate(WTText, list(org_annotator,person_annotator,date_annotator,percent_annotator,loc_annotator,money_annotator), WTTokens)
  WTOrg=subset(WTOrgPersTokens,WTOrgPersTokens$features=='list(kind = "organization")')
  WTPerson=subset(WTOrgPersTokens,WTOrgPersTokens$features=='list(kind = "person")')
  WTDate=subset(WTOrgPersTokens,WTOrgPersTokens$features=='list(kind = "date")')
  WTLocation=subset(WTOrgPersTokens,WTOrgPersTokens$features=='list(kind = "location")')
  WTPercent=subset(WTOrgPersTokens,WTOrgPersTokens$features=='list(kind = "percent")')
  WTMoney=subset(WTOrgPersTokens,WTOrgPersTokens$features=='list(kind = "money")')
  for (i in 1:nrow(as.data.frame(WTOrg)))
  {
    if (nrow(as.data.frame(WTOrg))>0) {
      WTEntities=rbind(WTEntities, cbind(post, substr(paste(WTText, collapse=' '),
                                                      WTOrg$start[i],WTOrg$end[i]),WTOrg$start[i], 'Organization'))
    }
  }
  for (i in 1:nrow(as.data.frame(WTPerson)))
  {
    if (nrow(as.data.frame(WTPerson))>0) {
      WTEntities=rbind(WTEntities, cbind(post, substr(paste(WTText, collapse=' '),
                                                      WTPerson$start[i],WTPerson$end[i]),WTPerson$start[i], 'Person'))
    }
  }
  for (i in 1:nrow(as.data.frame(WTDate)))
  {
    if (nrow(as.data.frame(WTDate))>0) {
      WTEntities=rbind(WTEntities, cbind(post, substr(paste(WTText, collapse=' '),
                                                      WTDate$start[i],WTDate$end[i]),WTDate$start[i], 'Date'))
    }
  }
  for (i in 1:nrow(as.data.frame(WTLocation)))
  {
    if (nrow(as.data.frame(WTLocation))>0) {
      WTEntities=rbind(WTEntities, cbind(post, substr(paste(WTText, collapse=' '),
                                                      WTLocation$start[i],WTLocation$end[i]),WTLocation$start[i], 'Location'))
    }
  }
  for (i in 1:nrow(as.data.frame(WTPercent)))
  {
    if (nrow(as.data.frame(WTPercent))>0) {
      WTEntities=rbind(WTEntities, cbind(post, substr(paste(WTText, collapse=' '),
                                                      WTPercent$start[i],WTPercent$end[i]),WTPercent$start[i], 'Percent'))
    }
  }
  for (i in 1:nrow(as.data.frame(WTMoney)))
  {
    if (nrow(as.data.frame(WTMoney))>0) {
      WTEntities=rbind(WTEntities, cbind(post, substr(paste(WTText, collapse=' '),
                                                      WTMoney$start[i],WTMoney$end[i]),WTMoney$start[i], 'Money'))
    }
  }
}

colnames(WTEntities)=c('Post','Entity','Position','Type')
WTEntities
#WTEntities=subset(WTEntities,WTEntities$Entity!='@CryptoYoda1338')
#WTEntities
WTResult=merge(WTEntities,WT,by.x='Post',by.y='Post')
write.csv(WTResult,'WesternTweetsResult.csv',row.names=T)
