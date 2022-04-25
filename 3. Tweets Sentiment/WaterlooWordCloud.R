library(dplyr)
library(tm)
library(SnowballC)
library(ggplot2)
library(reshape2)
library(syuzhet)
library(wordcloud)

DivPosts = read.csv("WaterlooEdgeSheet.csv")
DivCorp = Corpus(VectorSource(DivPosts$X.11))

StripString=content_transformer(function(x, pattern) gsub(pattern, '', x))
latin2ascii=content_transformer(function(x) iconv(x, 'latin1', 'ASCII', sub=''))

DivCorp=tm_map(DivCorp, content_transformer(tolower))
DivCorp=tm_map(DivCorp, removeWords,stopwords('english'))
DivCorp=tm_map(DivCorp, removePunctuation)
DivCorp=tm_map(DivCorp, removeNumbers)
DivCorp=tm_map(DivCorp, StripString,'[\r\n]')
DivCorp=tm_map(DivCorp, StripString,'[\t]')
DivCorp=tm_map(DivCorp, stemDocument)
DivCorp=tm_map(DivCorp,latin2ascii)
DivCorp=tm_map(DivCorp, StripString,'gtgt')
DivCorp=tm_map(DivCorp, StripString,'https[[:alnum:]]*')

DTM=DocumentTermMatrix(DivCorp)

DTM=removeSparseTerms(DTM, 0.99)

inspect(DTM)

#1
DTM$dimnames$Terms
#TDM$dimnames$Terms
TermFreq=colSums(as.matrix(DTM))
TermFreq=sort(TermFreq, decreasing=T)
FreqWord=as.data.frame(TermFreq)
FreqWord<-tibble::rownames_to_column(FreqWord, "Term")
FreqWord100=head(FreqWord,100)
FreqWord100
FreqWord25=head(FreqWord,25)

ggplot(FreqWord25, aes(x=reorder(Term, TermFreq),y=TermFreq))+
  geom_bar(stat='identity',fill='darkred')+
  labs(y='Frequencies',X='Term')+
  theme(text=element_text(size=12))+
  coord_flip()

#2
Senti <- get_nrc_sentiment(DivPosts$X.11)
DivSenti<-mutate(Senti, Sentiment=ifelse(Senti$positive-Senti$negative > 0, 'positive', 
                                            ifelse(Senti$positive-Senti$negative == 0, 'neutral', 'negative')))
DivSenti<-mutate(DivSenti, SentimentScore=DivSenti$positive-DivSenti$negative)
CombinedSenti=cbind(DivPosts$X,DivPosts$X.11,DivSenti)
CombinedSenti
table(CombinedSenti$Sentiment)
write.csv(CombinedSenti,'WaterlooSentiResult.csv',row.names=T)

#3
NegativeDivSenti=CombinedSenti[order(CombinedSenti$SentimentScore),]
NegativeDivSenti

PositiveDivSenti=CombinedSenti[order(CombinedSenti$SentimentScore,decreasing = TRUE),]
PositiveDivSenti

#4
wordcloud(DivCorp,min.freq = 10)

wordcloud(FreqWord100$Term,FreqWord100$TermFreq,rot.per =.25,vfont=c('serif','bold'),col=brewer.pal(7,"Blues"))

Emotion=cbind(DivPosts$X.11,Senti)

Anger=filter(Emotion,anger>0)
wordcloud(Anger$`DivPosts$X.11`,min.freq = 20,font=4,col=brewer.pal(5,"Oranges"))

Disgust=filter(Emotion,disgust>0)
wordcloud(Disgust$`DivPosts$X.11`,min.freq = 15,font=3,col=brewer.pal(5,"Reds"))

Surprise=filter(Emotion,surprise>0)
wordcloud(Surprise$`DivPosts$X.11`,min.freq = 5,col=rainbow(10))
