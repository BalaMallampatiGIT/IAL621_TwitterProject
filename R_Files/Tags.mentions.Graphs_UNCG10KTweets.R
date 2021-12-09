library(quanteda)
library(readtext)
library(ndjson)
library(tidyverse)
# install.packages("quanteda.textplots")
library(quanteda.textplots)

# read in data
tweets.data <- readtext('UNCG500KTweets.csv', text_field='text')
# View(tweets.data)
# write.csv(file="UNCG10KTweets_R.csv",tweets.data)

# convert to corpus data structure
tweets.corpus <- corpus(tweets.data)

# convert to document feature matrix
tweet.dfm <- dfm(tweets.corpus, remove_punct = TRUE)

#'dfm.corpus()' is deprecated. Use 'tokens()' first. So used tokens
toks <- tweets.corpus %>%  tokens()
tweet.dfm <- dfm(toks)

# explore
head(tweet.dfm)


# extract the most frequently used hashtags
tag.dfm <- dfm_select(tweet.dfm, pattern = c("uncg"))
toptag <- names(topfeatures(tag.dfm, 50))

# explore top hashtags
head(toptag)

# construct feature co-occurance matrix of hashtags
tag.fcm <- fcm(tag.dfm)
head(tag.fcm)

# create graph of co-occurance relationships for tags
topgat.fcm <- fcm_select(tag.fcm, pattern = toptag)
textplot_network(topgat.fcm, min_freq = 0.1, edge_alpha = 0.8, edge_size = 5)

# extract the most frequently mentioned usernames
user.dfm <- dfm_select(tweet.dfm, pattern = c("#*","ð","T","<","¡","-","®","¤",">"))
topuser <- names(topfeatures(user.dfm, 100))

# explore top usernames
head(topuser)

# construct feature co-occurance matrix of usernames
user.fcm <- fcm(user.dfm)
head(user.fcm)

# create a graph of co-occurance relationships for mentioned usernames
user.fcm <- fcm_select(user.fcm, pattern = topuser)
# install.packages("quanteda.textplots")
library(quanteda.textplots)
textplot_network(user.fcm, min_freq = 0.1, edge_color = "orange", edge_alpha = 0.8, edge_size = 5)

