###############WhatsappAnalysis.R

#Load required packages
library(ggplot2)
library(lubridate)
library(Scale)
library(reshape2)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(stringr)
library(syuzhet) 
library(dplyr ) 

setwd("C:/Users/Maddy/Desktop/Whatsapp_sentiment_analysis")

#Read the data from whatsapp chat 
content <- readLines("Karthi.txt")

#Corpus creation
corp <- Corpus(VectorSource(content))

#Pre-processing the chat file
tran <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
corp <- tm_map(corp, trans, "/")
corp <- tm_map(corp, tran, "@")
corp <- tm_map(corp, tran, "\\|")
corp <- tm_map(corp, content_transformer(tolower))
corp <- tm_map(corp, removeNumbers)
corp <- tm_map(corp, removeWords, stopwords("english"))
corp <- tm_map(corp, removeWords, c("karthi","ddi"))
corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, stripWhitespace)
corp <- tm_map(corp, stemDocument)

#Document Term Matrix creation
#-matrix that describes the frequency of terms that occur in a collection of documents-rows:documents,column:terms
DTM <- TermDocumentMatrix(corp)
mat <- as.matrix(DTM)
a <- sort(rowSums(mat),decreasing=TRUE)

#Table creation usinf dataframe
data <- data.frame(word = names(a),freq=a)
head(data, 10)


#World cloud graph creation
set.seed(1056)
wordcloud(words = data$word, freq = data$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))


#Sentiment analysis from file
Sentiment <- get_nrc_sentiment(content)
head(Sentiment)
text <- cbind(content,Sentiment)

#Counting the words in dataframe
TotalSentiment <- data.frame(colSums(content[,c(2:11)]))
names(TotalSentiment) <- "count"
TotalSentiment <- cbind("sentiment" = rownames(TotalSentiment), TotalSentiment)
rownames(TotalSentiment) <- NULL

#Sentiment score graph with ggplot2
ggplot(data = TotalSentiment, aes(x = sentiment, y = count)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Total Count") + ggtitle("Total Sentiment Score")
