library(ndjson)
library(tidyverse)
library(tm)

##################################################################################
# basic cleaning and wranlging for massmine json in R
##################################################################################

# use strem_in() function from ndjson to create data.frame/tibble
tweets.data <- stream_in('UNCG100Tweets.ndjson') %>%
  as_tibble() %>%
  # reduce dataset to the following variables
  select(created_at, text, user.description, user.favourites_count, user.followers_count, user.friends_count, user.location, user.name, user.screen_name, user.statuses_count, entities.user_mentions.0.screen_name, retweeted_status.user.screen_name, entities.urls.0.expanded_url, retweeted_status.user.entities.url.urls.0.expanded_url, retweet_count) %>%
  # rename variables
  rename(tweet.datetime = created_at, total.favorites = user.favourites_count, total.followers = user.followers_count, total.friends = user.friends_count, screen.name = user.screen_name, total.statuses = user.statuses_count, tweet.mentions = entities.user_mentions.0.screen_name, retweet.mentions = retweeted_status.user.screen_name, url = entities.urls.0.expanded_url, retweet.url = retweeted_status.user.entities.url.urls.0.expanded_url, total.retweets = retweet_count)

dim(tweets.data)

# common artifacts that remain after cleaning
other.words <- c("rt", "amp","htt")

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

# converts tweets.data to csv file
write.csv(tweets.data, file="UNCG100Tweets.csv")