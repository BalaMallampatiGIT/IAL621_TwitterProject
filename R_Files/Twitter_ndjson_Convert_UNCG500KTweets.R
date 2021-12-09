library(ndjson)
library(tidyverse)
library(tm)
# install.packages("remotes")
# remotes::install_github("Displayr/flipTime")
library(flipTime)
library(lubridate)
library(stringr)

# doInstall <- TRUE  # Change to FALSE if you don't want packages installed.
# toInstall <- c("ROAuth", "igraph", "ggplot2", "wordcloud", "devtools", "tm",
#                "R2WinBUGS", "rmongodb", "scales")
# if(doInstall){
#   install.packages(toInstall, repos = "http://cran.r-project.org")
#   library(devtools)
#   # R packages to get twitter and Facebook data
#   install_github("streamR", "pablobarbera", subdir="streamR")
#   install_github("Rfacebook", "pablobarbera", subdir="Rfacebook")
#   # smapp R package
#   install_github("smappR", "SMAPPNYU")
# }
# install.packages("smappR")
# library(smappR)

##################################################################################
# basic cleaning and wranlging for massmine json in R
##################################################################################

# use strem_in() function from ndjson to create data.frame/tibble
tweets.data <- stream_in('UNCG500KTweets_Project.ndjson') %>%
  as_tibble() %>%
  # reduce dataset to the following variables
  select(created_at, text, user.description, user.favourites_count, user.followers_count, user.friends_count, user.location, user.name, user.screen_name, user.statuses_count, entities.user_mentions.0.screen_name, retweeted_status.user.screen_name, entities.urls.0.expanded_url, retweeted_status.user.entities.url.urls.0.expanded_url, retweet_count) %>%
  # rename variables
  rename(tweet.datetime = created_at, total.favorites = user.favourites_count, total.followers = user.followers_count, total.friends = user.friends_count, screen.name = user.screen_name, total.statuses = user.statuses_count, tweet.mentions = entities.user_mentions.0.screen_name, retweet.mentions = retweeted_status.user.screen_name, url = entities.urls.0.expanded_url, retweet.url = retweeted_status.user.entities.url.urls.0.expanded_url, total.retweets = retweet_count)

#str_replace_all( "arena monthsðÿz® hub locat iâ???¦",  "[[:punct:]]", "")
#iconv("arena monthsðÿz® hub locat iâ???¦",, from = 'UTF-8', to = 'ASCII//TRANSLIT')
#gsub("[^0-9A-Za-z///' ]","" , "arena monthsðÿz® hub locat iâ???¦" ,ignore.case = TRUE)
names(tweets.data)
#View(tweets.data)
typeof(tweets.data$tweet.datetime)
#tweets.data <- as.data.frame(gsub("[^[:alnum:]]", " ", as.matrix(tweets.data)))  
#tweets.data <- as.data.frame(gsub("[[:punct:]]", " ", as.matrix(tweets.data)))
#tweets.data <- as.data.frame(gsub("[^a-zA-Z0-9]", " ", as.matrix(tweets.data)))
tweets.data$text<- gsub("(s?)(f|ht)tp(s?)://\\S+\\b", "", tweets.data$text)
tweets.data$text<- gsub("[^a-zA-Z0-9]", " ", tweets.data$text)
tweets.data$user.description<- gsub("[^a-zA-Z0-9]", " ", tweets.data$user.description)
tweets.data$user.name<- gsub("[^a-zA-Z0-9]", " ", tweets.data$user.name)
tweets.data$tweet.datetime <- AsDateTime(paste(substr(tweets.data$tweet.datetime, 5, 10),"2021",substr(tweets.data$tweet.datetime, 12, 19) ))


#format.twitter.date(tweets.data$tweet.datetime, format="date")
#as_date(tweets.data$tweet.datetime, tz = NULL)
#(str_trim(substr(str_trim(scrape.data$Date), 1, 13)))

#View(tweets.data)

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
write.csv(tweets.data, file="UNCG500KTweets.csv")

#Use R script example to create a list of users for collecting histories of user statuses
unique(tweets.data$user.name)
write.csv(unique(tweets.data$user.name), file="UNCG500KTweets_UsersList.csv")