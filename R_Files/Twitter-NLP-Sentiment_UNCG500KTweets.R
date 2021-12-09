library(ndjson)
library(tidyverse)
library(tidytext)
library(widyr)
library(tm)

# convert json to data.frame/tibble and then reduce to only needed variables
tweets.data <- stream_in('UNCG500KTweets_Project.ndjson') %>%
  as_tibble() %>%
  select(created_at, text, user.description, user.favourites_count, user.followers_count, user.friends_count, user.location, user.name, user.screen_name, user.statuses_count, entities.user_mentions.0.screen_name, retweeted_status.user.screen_name, entities.urls.0.expanded_url, retweeted_status.user.entities.url.urls.0.expanded_url, retweet_count) %>%
  rename(tweet.datetime = created_at, total.favorites = user.favourites_count, total.followers = user.followers_count, total.friends = user.friends_count, screen.name = user.screen_name, total.statuses = user.statuses_count, tweet.mentions = entities.user_mentions.0.screen_name, retweet.mentions = retweeted_status.user.screen_name, url = entities.urls.0.expanded_url, retweet.url = retweeted_status.user.entities.url.urls.0.expanded_url, total.retweets = retweet_count)

tweets.data$text<- gsub("[^a-zA-Z0-9]", " ", tweets.data$text)
tweets.data$user.description<- gsub("[^a-zA-Z0-9]", " ", tweets.data$user.description)
tweets.data$user.name<- gsub("[^a-zA-Z0-9]", " ", tweets.data$user.name)

names(tweets.data)
# common artifacts that remain after cleaning
other.words <- c("Advertisement","Supported", "by", "SUBSCRIBER", "NEWSLETTER", "Wirecutter", "reader-supported",
                 "Times", "games", "day", "You", "he", "his", "Are", "they",
                 "has", "person", "jacob", "person", "thoma", "minute", "noun")

# remove all urls
tweets.data$text <- gsub("(s?)(f|ht)tp(s?)://\\S+\\b", "", tweets.data$text)

# clean data
tweets.data$text <- tweets.data$text %>%
  removePunctuation() %>%
  removeNumbers() %>%
  tolower() %>%
  removeWords(stopwords("SMART")) %>%
  removeWords(other.words) %>%
  stemDocument() %>%
  stripWhitespace()

# transform table into one-word-per-line tidytext format
tidy.data <- tweets.data %>%
  unnest_tokens(word, text)

# most frequent words
tidy.data %>%
  count(word, sort = TRUE)

# bigrams
bigram.data <- tweets.data %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

# explore bigrams 
bigram.data %>%
  count(bigram, sort = TRUE)

# pairwise word correlations
word.cors <- tidy.data %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, url, sort = TRUE)

# explore top word correlations
word.cors

# most frequent words
tidy.data %>%
  count(word, sort = TRUE)

# explore specific word correlations
# try a couple different words for fun
word.cors %>%
  filter(item1 == "ncat")

# produce graph comparing 4 words of importance to their most correlated words
word.cors %>%
  filter(item1 %in% c("asu", "item", "ncat","uncg","pitch", "auction","scienc","essay","ssu","rt")) %>%
  group_by(item1) %>%
  top_n(6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation,fill=correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()

############### sentiment ###################
bing.negative <- get_sentiments("bing") %>%
  filter(sentiment == "negative")

# explore
bing.negative

# scores word sentiment according to bing lexicon
bing.word.counts <- tidy.data %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

# visualizes to positive and negative words according to bing
bing.word.counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

#############################Sentiment Analysis Using R and Twitter ###################################
# Install packages
# install.packages("twitteR", repos = "http://cran.us.r-project.org")
# install.packages("RCurl", repos = "http://cran.us.r-project.org")
# install.packages("httr", repos = "http://cran.us.r-project.org")
# install.packages("syuzhet", repos = "http://cran.us.r-project.org")

# Load the required Packages
library(twitteR)
library(RCurl)
library(httr)
library(tm)
library(wordcloud)
library(syuzhet)
# authorisation keys
consumer_key = "ikYcvoNX8woXzyi1FZWhc0koY" #Consumer key from twitter app
consumer_secret = "AS7nalextbcrs5CSUED09frbu54nIIWhiDE5XYTJDb0VBMUtn6" #Consumer secret from twitter app
access_token = "1426026335529586690-zFcXMpXJ1NZ0sH5pB0QGZORbSIJ7y8" #access token from twitter app
access_secret ="Q9MbWeFNRR3M2YRpC0Lx4BN2gKAnfdRIGpTZqpwVjZa3b" #access secret from twitter app

# set up
setup_twitter_oauth(consumer_key,consumer_secret,access_token, access_secret)
## [1] "Using direct authentication"
# search for tweets in english language
tweets = searchTwitter("#UNCG", n = 10000, lang = "en")
# store the tweets into dataframe
tweets.df = twListToDF(tweets)
# Warning message:
#   In doRppAPICall("search/tweets", n, params = params, retryOnRateLimit = retryOnRateLimit,  :
#                     10000 tweets were requested but the API can only return 652

# CLEANING TWEETS

tweets.df$text=gsub("&amp", "", tweets.df$text)
tweets.df$text = gsub("&amp", "", tweets.df$text)
tweets.df$text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweets.df$text)
tweets.df$text = gsub("@\\w+", "", tweets.df$text)
tweets.df$text = gsub("[[:punct:]]", "", tweets.df$text)
tweets.df$text = gsub("[[:digit:]]", "", tweets.df$text)
tweets.df$text = gsub("http\\w+", "", tweets.df$text)
tweets.df$text = gsub("[ \t]{2,}", "", tweets.df$text)
tweets.df$text = gsub("^\\s+|\\s+$", "", tweets.df$text)

tweets.df$text <- iconv(tweets.df$text, "UTF-8", "ASCII", sub="")

# Emotions for each tweet using NRC dictionary
emotions <- get_nrc_sentiment(tweets.df$text)
emo_bar = colSums(emotions)
emo_sum = data.frame(count=emo_bar, emotion=names(emo_bar))
emo_sum$emotion = factor(emo_sum$emotion, levels=emo_sum$emotion[order(emo_sum$count, decreasing = TRUE)])

# Visualize the emotions from NRC sentiments
library(plotly)
p <- plot_ly(emo_sum, x=~emotion, y=~count, type="bar", color=~emotion) %>%
  layout(xaxis=list(title=""), showlegend=FALSE,
         title="Emotion Type for hashtag: #UNCG")
p

api_create(p,filename="Sentimentanalysis")


# Create comparison word cloud data

wordcloud_tweet = c(
  paste(tweets.df$text[emotions$anger > 0], collapse=" "),
  paste(tweets.df$text[emotions$anticipation > 0], collapse=" "),
  paste(tweets.df$text[emotions$disgust > 0], collapse=" "),
  paste(tweets.df$text[emotions$fear > 0], collapse=" "),
  paste(tweets.df$text[emotions$joy > 0], collapse=" "),
  paste(tweets.df$text[emotions$sadness > 0], collapse=" "),
  paste(tweets.df$text[emotions$surprise > 0], collapse=" "),
  paste(tweets.df$text[emotions$trust > 0], collapse=" ")
)

# create corpus
corpus = Corpus(VectorSource(wordcloud_tweet))

# remove punctuation, convert every word in lower case and remove stop words

corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, c(stopwords("english")))
corpus = tm_map(corpus, stemDocument)

# create document term matrix

tdm = TermDocumentMatrix(corpus)

# convert as matrix
tdm = as.matrix(tdm)
tdmnew <- tdm[nchar(rownames(tdm)) < 11,]

# column name binding
colnames(tdm) = c('anger', 'anticipation', 'disgust', 'fear', 'joy', 'sadness', 'surprise', 'trust')
colnames(tdmnew) <- colnames(tdm)
comparison.cloud(tdmnew, random.order=FALSE,
                 colors = c("#00B2FF", "red", "#FF0099", "#6600CC", "green", "orange", "blue", "brown"),
                 title.size=1.5, max.words=300, scale=c(3, 0.5),rot.per=0.4)

