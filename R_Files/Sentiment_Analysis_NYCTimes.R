library(tidyverse)
library(tidytext)
# install.packages("textdata")
library(textdata)
# install.packages("remotes")
# remotes::install_github("Displayr/flipTime")
library(flipTime)
library(lubridate)
library(stringr)
###############################################################################################################
# Additional tidy sentiment tutorials of interest:
# https://datascienceplus.com/parsing-text-for-emotion-terms-analysis-visualization-using-r-upDated-analysis/
# https://www.kaggle.com/rtatman/tutorial-sentiment-analysis-in-r
# TM package sentiment analysis:
# https://www.red-gate.com/simple-talk/sql/bi/text-mining-and-sentiment-analysis-with-r/
###############################################################################################################


##############################################################
# continues to use the data restulging from rvest_example2.R
##############################################################

# read in csv file as tibble/data frame
scrape.data <- read.csv(file='NYCTimes.csv', stringsAsFactors=FALSE) %>%
  as_tibble()

scrape.data$Date <- str_replace_all(scrape.data$Date,"Updated","")
scrape.data$Date <- str_replace_all(scrape.data$Date,"FYI","")
scrape.data$Date <- str_replace_all(scrape.data$Date,"Published","")
scrape.data$Date <- (str_trim(substr(str_trim(scrape.data$Date), 1, 13)))
scrape.data$Date[scrape.data$Date==""] <- NA
unique(scrape.data$Date)


# uses FlipTime package to convert Datetime format
scrape.data$Date <- AsDate(scrape.data$Date)


# filter to just 2020 and add month and week
tidy.data <- scrape.data %>%
  filter(year(Date) %in% c(2021)) %>%
  group_by(month=floor_date(Date, "month"), week = week(Date))

# transform month to month name abbreviation
tidy.data$month <- tidy.data$month %>%
  month(label = TRUE)

# explore
unique(tidy.data$month)
unique(tidy.data$week)

# transform to one word per line tidytext format
# this translates their book, line, chapter wrangling for months, weeks, and URLs
# chapter 2.2 Text Mining with R
tidy.data <- tidy.data %>%
  group_by(month) %>%
  ungroup() %>%
  mutate(linenumber = row_number()) %>%
  unnest_tokens(word, Text)

# capture negative sentiments for bing lexicon
# chapter 2.2 Text Mining with R
bing.negative <- get_sentiments("bing") %>%
  filter(sentiment == "negative")

# count the top bing lexicon negatives in a given month
# chapter 2.2 Text Mining with R
month.negative <- tidy.data %>%
  filter(month == "Oct") %>%
  inner_join(bing.negative) %>%
  count(word, sort = TRUE)

# reproduces figure 2.2 from Texting with R
url.sentiment <- tidy.data %>%
  inner_join(get_sentiments("bing")) %>%
  count(month, index = URL, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

# reproduces figure 2.2 from Texting with R
ggplot(url.sentiment, aes(index, sentiment, fill = month)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~month, ncol = 2, scales = "free_x")

# find out how much each word contributed to sentiment
# chapter 2.4 Text Minging with R
bing.word.counts <- tidy.data %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

# visualize top words contributing to sentiment
# figure 2.4 Text Minging with R
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

##########################################
# bi-gram contenxt for sentiment analysis
# chapter 4.1.3 Text Mining with R
##########################################

# tokenizing bigrams
# chapter 4.1.3 Text Mining with R
url.bigrams <- scrape.data %>%
  unnest_tokens(bigram, Text, token = "ngrams", n = 2)

# separating bigrams into two columns
# chapter 4.1.3 Text Mining with R
bigrams.separated <- url.bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# calling the AFINN lexicon for sentiment analysis
# chapter 4.1.3 Text Mining with R
AFINN <- get_sentiments("afinn")
# Enter an item from the menu, or 0 to exit
# Selection: Yes
# trying URL 'http://www2.imm.dtu.dk/pubdb/views/edoc_download.php/6010/zip/imm6010.zip'
# Content type 'application/zip' length 16227 bytes (15 KB)
# downloaded 15 KB

# analyzing the most frequent words preceded by "not"
# chapter 4.1.3 Text Mining with R
not.words <- bigrams.separated %>%
  filter(word1 == "not") %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, value, sort = TRUE)

# explore
# chapter 4.1.3 Text Mining with R
not.words

# visualize "not" words
# figure 4.2 Text Mining with R
not.words %>%
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, n * value, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  xlab("Words preceded by \"not\"") +
  ylab("Sentiment value * number of occurrences") +
  coord_flip()

# common negative words to precede other words
# chapter 4.1.3 Text Mining with R
negation.words <- c("not", "no", "never", "without")

# analyzing frequent words preceded by negation.words
# chapter 4.1.3 Text Mining with R
negated.words <- bigrams.separated %>%
  filter(word1 %in% negation.words) %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word1, word2, value, sort = TRUE)

# visualizing commonly negated words
# figure 4.3 Text Mining with R
negated.words %>%
  mutate(contribution = n * value,
         word2 = reorder(paste(word2, word1, sep = "__"), contribution)) %>%
  group_by(word1) %>%
  top_n(12, abs(contribution)) %>%
  ggplot(aes(word2, contribution, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ word1, scales = "free") +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  xlab("Words preceded by negation term") +
  ylab("Sentiment value * # of occurrences") +
  coord_flip()