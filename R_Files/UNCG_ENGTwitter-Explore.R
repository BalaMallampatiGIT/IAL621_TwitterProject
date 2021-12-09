library(ndjson)
library(tidyverse)
library(tm)
library(ggmap)
library(maps)

# converts json to data.frame
tweets.data.bulk <- stream_in('UNCG500KTweets_Project.ndjson') %>%
  as_tibble()

# explore number of columns/variables
length(names(tweets.data.bulk))

# convert json to data.frame/tibble and then reduce to only needed variables
tweets.data <- stream_in('UNCG_ENGLast100Tweets.ndjson') %>%
  as_tibble() %>%
  select(created_at, text, user.description, user.favourites_count, user.followers_count, user.friends_count, user.location, user.name, user.screen_name, user.statuses_count, entities.user_mentions.0.screen_name, retweeted_status.user.screen_name, entities.urls.0.expanded_url, retweeted_status.user.entities.url.urls.0.expanded_url, retweet_count) %>%
  rename(tweet.datetime = created_at, total.favorites = user.favourites_count, total.followers = user.followers_count, total.friends = user.friends_count, screen.name = user.screen_name, total.statuses = user.statuses_count, tweet.mentions = entities.user_mentions.0.screen_name, retweet.mentions = retweeted_status.user.screen_name, url = entities.urls.0.expanded_url, retweet.url = retweeted_status.user.entities.url.urls.0.expanded_url, total.retweets = retweet_count)

tweets.data$text<- gsub("[^a-zA-Z0-9]", " ", tweets.data$text)
tweets.data$user.description<- gsub("[^a-zA-Z0-9]", " ", tweets.data$user.description)
tweets.data$user.name<- gsub("[^a-zA-Z0-9]", " ", tweets.data$user.name)

# compare differences in two datasets after reduction
object.size(tweets.data.bulk)
object.size(tweets.data)
length(names(tweets.data.bulk))
length(names(tweets.data))

# remove bulk dataset from memory
rm(tweets.data.bulk)

# view result of reduced dataset and renaming of variables
names(tweets.data)

# top retweeted users
tweets.data %>%
  select(total.retweets, retweet.mentions) %>%
  distinct(retweet.mentions, .keep_all=TRUE) %>%
  arrange(desc(total.retweets)) %>%
  print(n=25)

# top retweeted URLs
tweets.data %>%
  select(total.retweets, retweet.url) %>%
  drop_na(retweet.url) %>%
  distinct(retweet.url, .keep_all=TRUE) %>%
  arrange(desc(total.retweets)) %>%
  print(n=25)

# users with most followers
tweets.data %>%
  select(total.followers, screen.name) %>%
  distinct(screen.name, .keep_all=TRUE) %>%
  arrange(desc(total.followers)) %>%
  print(n=25)

##############################################
# total overall "tweeting" activity in dataset
##############################################
# convert twitter datetimes to posix
tweets.data$tweet.datetime <- as.POSIXct(tweets.data$tweet.datetime, format = "%a %b %d %H:%M:%S", tz = "UTC")

# explore total tweets per day
tweets.data %>%
  mutate(date = as.Date(tweet.datetime, format = "%m/%d/%y")) %>%
  group_by(date) %>%
  count()

# visualize total tweets per day in line chart
tweets.data %>%
  mutate(date = as.Date(tweet.datetime, format = "%m/%d/%y")) %>%
  group_by(date) %>%
  count() %>%
  ggplot(aes(x = date, y = n))+
  geom_line(color = "blue", size = 0.5)

##################################
# Example ONLY
# not a required part of tutorial
##################################

# location scrubbing example
tweets.location <- word(tweets.data$user.location, 1) %>%
  tolower() %>%
  removePunctuation() %>%
  discard(`==`, "")

# reduce to first 200 for testing purposes
tweets.location <- head(tweets.location, 200)

# yt_auth(new_user=TRUE, no_auto=FALSE)
# you must provide ggmap with your google API key
register_google(key = "AIzaSyDpIBq6yegcVgzhV0qdkThEt4ITBj1SzY8")

# Source : https://maps.googleapis.com/maps/api/geocode/json?address=greensboro&key=xxx
# Warning: Geocoding "greensboro" failed with error:
  # You must enable Billing on the Google Cloud Project at https://console.cloud.google.com/project/_/billing/enable Learn more at https://developers.google.com/maps/gmp-get-started

geocode("houston texas")
# get geocode data for city names
tweets.geocode <- geocode(tweets.location)

# explore
tweets.geocode %>%
  drop_na() %>%
  print(n=25)

#simple map of tweeting locations
map("world", fill=TRUE, col="white", bg="lightblue", ylim=c(-60, 90), mar=c(0,0,0,0))
points(tweets.geocode$lon,tweets.geocode$lat, col="red", pch=16)