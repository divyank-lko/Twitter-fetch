# Step 0 - Importing libraries

library('devtools')
library('twitteR')
library('ROAuth')
library('plyr')
library('dplyr')
library('stringr')
library('ggplot2')
library('httr')
library('wordcloud')
library('tm')
library('RCurl')
library('syuzhet')

api_key <- "7iWrZxxxxxxxxxxxxxxxxxxxx"
api_secret <- "xxxxxxxxxkxkepPy7moWxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
access_token <- "xxxxxxxxx-xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
access_token_secret <- "J4zGrgxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"

# Step 1 
# Authenticate

setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

# Step 2
# Get data

some_tweets_vodafone_idea = searchTwitter("#vodafone OR #vodafoneidea",
                                     n = 1000, 
                                     lang = "en")

some_tweets_airtel = searchTwitter("#Airtel OR #airtel",
                                     n=1000,
                                     lang = "en")

some_tweets_bsnl = searchTwitter("#bsnl OR #BSNL",
                                 n = 1000,
                                 lang = "en")

some_tweets_jio = searchTwitter("#reliancejio OR #RelianceJio OR #jio",
                                n = 1000,
                                lang = "en")


some_tweets.df.vodafone.idea <- ldply(some_tweets_vodafone_idea, function(t) t$toDataFrame())

some_tweets.df.airtel = ldply(some_tweets_airtel, function(t) t$toDataFrame())

some_tweets.df.bsnl = ldply(some_tweets_bsnl, function(t) t$toDataFrame())

some_tweets.df.jio = ldply(some_tweets_jio, function(t) t$toDataFrame())

# Step 3
# Data cleaning

# VodafoneIdea

vodafone_idea_txt = sapply(some_tweets_vodafone_idea, function(x) x$getText())

vodafone_idea_txt1 = gsub("(RT|via)((?:\\b\\W*@\\w+)+)","",vodafone_idea_txt)

vodafone_idea_txt2 = gsub("http[^[:blank:]]+", "", vodafone_idea_txt1)

vodafone_idea_txt3 = gsub("@\\w+", "", vodafone_idea_txt2)

vodafone_idea_txt4 = gsub("[^[:alnum:]]", " ", vodafone_idea_txt3)

# converting into corpus

vodafone_idea_txt5 <- Corpus(VectorSource(vodafone_idea_txt4))

vodafone_idea_txt6 <- tm_map(vodafone_idea_txt5, removePunctuation)
vodafone_idea_txt6 <- tm_map(vodafone_idea_txt5, content_transformer(tolower))
vodafone_idea_txt6 <- tm_map(vodafone_idea_txt5, removeWords, stopwords("english"))
vodafone_idea_txt6 <- tm_map(vodafone_idea_txt5, stripWhitespace)

# airtel

airtel_txt = sapply(some_tweets_airtel, function(x) x$getText())

airtel_txt1 = gsub("(RT|via)((?:\\b\\W*@\\w+)+)","",airtel_txt)

airtel_txt2 = gsub("http[^[:blank:]]+", "", airtel_txt1)

airtel_txt3 = gsub("@\\w+", "", airtel_txt2)

airtel_txt4 = gsub("[^[:alnum:]]", " ", airtel_txt3)

# converting into corpus

airtel_txt5 <- Corpus(VectorSource(airtel_txt4))

airtel_txt6 <- tm_map(airtel_txt5, removePunctuation)
airtel_txt6 <- tm_map(airtel_txt5, content_transformer(tolower))
airtel_txt6 <- tm_map(airtel_txt5, removeWords, stopwords("english"))
airtel_txt6 <- tm_map(airtel_txt5, stripWhitespace)

#bsnl

bsnl_txt = sapply(some_tweets_bsnl, function(x) x$getText())

bsnl_txt1 = gsub("(RT|via)((?:\\b\\W*@\\w+)+)","",bsnl_txt)

bsnl_txt2 = gsub("http[^[:blank:]]+", "", bsnl_txt1)

bsnl_txt3 = gsub("@\\w+", "", bsnl_txt2)

bsnl_txt4 = gsub("[^[:alnum:]]", " ", bsnl_txt3)

# converting into corpus

bsnl_txt5 <- Corpus(VectorSource(bsnl_txt4))

bsnl_txt6 <- tm_map(bsnl_txt5, removePunctuation)
bsnl_txt6 <- tm_map(bsnl_txt5, content_transformer(tolower))
bsnl_txt6 <- tm_map(bsnl_txt5, removeWords, stopwords("english"))
bsnl_txt6 <- tm_map(bsnl_txt5, stripWhitespace)


# jio

jio_txt = sapply(some_tweets_jio, function(x) x$getText())

jio_txt1 = gsub("(RT|via)((?:\\b\\W*@\\w+)+)","",jio_txt)

jio_txt2 = gsub("http[^[:blank:]]+", "", jio_txt1)

jio_txt3 = gsub("@\\w+", "", jio_txt2)

jio_txt4 = gsub("[^[:alnum:]]", " ", jio_txt3)

# converting into corpus

jio_txt5 <- Corpus(VectorSource(jio_txt4))

jio_txt6 <- tm_map(jio_txt5, removePunctuation)
jio_txt6 <- tm_map(jio_txt5, content_transformer(tolower))
jio_txt6 <- tm_map(jio_txt5, removeWords, stopwords("english"))
jio_txt6 <- tm_map(jio_txt5, stripWhitespace)


pal <- brewer.pal(8,"Dark2")


# Plotting wordclouds

wordcloud_vodafone_idea = wordcloud(vodafone_idea_txt6, min.freq = 5,  max.words = Inf, width=1000, height =1000,  
          random.order = FALSE, color=pal)

wordcloud_airtel = wordcloud(airtel_txt6, min.freq = 5,  max.words = Inf, width=1000, height =1000,  
                                    random.order = FALSE, color=pal)

wordcloud_bsnl = wordcloud(bsnl_txt6, min.freq = 5,  max.words = Inf, width=1000, height =1000,  
                                    random.order = FALSE, color=pal)

wordcloud_jio = wordcloud(jio_txt6, min.freq = 5,  max.words = Inf, width=1000, height =1000,  
                                    random.order = FALSE, color=pal)


length(some_tweets_vodafone_idea) #1000
length(some_tweets_airtel)        #995 
length(some_tweets_bsnl)          #1000
length(some_tweets_jio)           #952


# Sentiment Analysis for each of the operators

# Vodafone Idea

vodafone_idea_sentiment <- get_nrc_sentiment(vodafone_idea_txt4) # returns a dataframe

vodafone_idea_sentiment_scores = data.frame(colSums(vodafone_idea_sentiment[,]))

vodafone_idea_sentiment_scores

names(vodafone_idea_sentiment_scores) <- "Vodafone_Idea_Score"

vodafone_idea_sentiment_scores <- cbind("sentiment" = rownames(vodafone_idea_sentiment_scores),
                                        vodafone_idea_sentiment_scores)

vodafone_idea_sentiment_scores

ggplot(data = vodafone_idea_sentiment_scores, aes(x = sentiment, y = Vodafone_Idea_Score)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Vodafone_Idea_Score") + 
  ggtitle("Total Sentiment Score Based on Tweets")


# Airtel

airtel_sentiment <- get_nrc_sentiment(airtel_txt4) # returns a dataframe

airtel_sentiment_scores = data.frame(colSums(airtel_sentiment[,]))

airtel_sentiment_scores

names(airtel_sentiment_scores) <- "airtel_Score"

airtel_sentiment_scores <- cbind("sentiment" = rownames(airtel_sentiment_scores),
                                 airtel_sentiment_scores)

airtel_sentiment_scores

ggplot(data = airtel_sentiment_scores, aes(x = sentiment, y = airtel_Score)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("airtel_Score") + 
  ggtitle("Total Sentiment Score Based on Tweets")


# BSNL

bsnl_sentiment <- get_nrc_sentiment(bsnl_txt4) # returns a dataframe

bsnl_sentiment_scores = data.frame(colSums(bsnl_sentiment[,]))

bsnl_sentiment_scores

names(bsnl_sentiment_scores) <- "bsnl_Score"

bsnl_sentiment_scores <- cbind("sentiment" = rownames(bsnl_sentiment_scores),
                               bsnl_sentiment_scores)

bsnl_sentiment_scores

ggplot(data = bsnl_sentiment_scores, aes(x = sentiment, y = bsnl_Score)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("bsnl_Score") + 
  ggtitle("Total Sentiment Score Based on Tweets")


# Jio

jio_sentiment <- get_nrc_sentiment(jio_txt4) # returns a dataframe

jio_sentiment_scores = data.frame(colSums(jio_sentiment[,]))

jio_sentiment_scores

names(jio_sentiment_scores) <- "jio_Score"

jio_sentiment_scores <- cbind("sentiment" = rownames(jio_sentiment_scores),
                              jio_sentiment_scores)

jio_sentiment_scores

ggplot(data = jio_sentiment_scores, aes(x = sentiment, y = jio_Score)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("jio_Score") + 
  ggtitle("Total Sentiment Score Based on Tweets")






















