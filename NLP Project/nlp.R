require(tidyverse)
require(tm)
require(stm)
require(quanteda)
require(stringr)

full_tweets <- read_rds("data/fulltweets.rds")
full_tweets <- full_tweets %>% subset(Username!= "everytract")


# test <- full_tweets %>% count(Username)
# test <- test[order(test$n, decreasing = T),]
# tst <- full_tweets %>% subset(Username == as.list(test[3,][1]))


full_tweets <- full_tweets %>% select(Datetime, 'Tweet Id', Text)


# dictionary 1 ------------------------------------------------------------
period1 <- full_tweets %>% subset(Datetime < "2018-11-01")
set.seed(123)
s_period1 <- period1[sample(nrow(period1), size = 10000),]

tweet_corpus <- Corpus(VectorSource(s_period1$Text))

corpus_clean <- tm_map(tweet_corpus, removePunctuation)
corpus_clean <- tm_map(corpus_clean, removeNumbers) 
corpus_clean <- tm_map(corpus_clean, content_transformer(tolower))
corpus_clean <- tm_map(corpus_clean, removeWords, stopwords())
corpus_clean <- tm_map(corpus_clean, stemDocument, language = "english")  
corpus_clean <- tm_map(corpus_clean, stripWhitespace)  

# head(tweet_corpus[[10000]]$content)
# head(corpus_clean[[10000]]$content)

clean_text <- data.frame(text = sapply(corpus_clean, as.character),
                         stringsAsFactors = FALSE)
s_period1$clean_text <- clean_text$text
tweet_dfm <- dfm(s_period1$clean_text)
tweet_dfm_df <- convert(tweet_dfm, to = "data.frame")

unique(str_extract(s_period1$clean_text, "alonei.*"))

health_dict <- c("depress", "anxieti", "anxious", "alonei", "stress", "frighten",
                 "scari", "scare", "fear", "strain",
                 "alarm", "danger", "racist", "racism", "mental")

# health_dict <- c("depress", "anx", "lonel", "alone", "stress", "fright", "horror",
#                  "panic", "scar", "terror", "fear", "concern", "tension", "strain",
#                  "alarm", "danger", "racist", "racism", "mental")

health_dfm_df <- tweet_dfm_df[health_dict]

health_dfm_df$total_words <- rowSums(health_dfm_df)
s_period1$total_words <- health_dfm_df$total_words
# saveRDS(s_period1, "data/period1.rds")

period1 <- read_rds("data/period1.rds")
test <- period1 %>% group_by(Datetime) %>% summarise(count = sum(total_words))
test <- test %>% subset(count != 0)

plot(test$Datetime, test$count)
matplot(test$Datetime, test$count, type = "l")

# dictionary 2 ------------------------------------------------------------
period2 <- full_tweets %>% subset(Datetime > "2019-08-31" &
                                  Datetime < "2020-06-01")
set.seed(123)
s_period2 <- period2[sample(nrow(period2), size = 10000),]

tweet_corpus <- Corpus(VectorSource(s_period2$Text))

corpus_clean <- tm_map(tweet_corpus, removePunctuation)
corpus_clean <- tm_map(corpus_clean, removeNumbers) 
corpus_clean <- tm_map(corpus_clean, content_transformer(tolower))
corpus_clean <- tm_map(corpus_clean, removeWords, stopwords())
corpus_clean <- tm_map(corpus_clean, stemDocument, language = "english")  
corpus_clean <- tm_map(corpus_clean, stripWhitespace)  

# head(tweet_corpus[[10000]]$content)
# head(corpus_clean[[10000]]$content)

clean_text <- data.frame(text = sapply(corpus_clean, as.character),
                         stringsAsFactors = FALSE)
s_period2$clean_text <- clean_text$text
tweet_dfm <- dfm(s_period2$clean_text)
tweet_dfm_df <- convert(tweet_dfm, to = "data.frame")

unique(str_extract(s_period2$clean_text, "alonei.*"))

health_dict <- c("depress", "anxieti", "anxious", "stress",
                 "scari", "scare", "fear", "frustrat",
                 "alarm", "danger", "racist", "racism", "mental")

# health_dict <- c("depress", "anx", "lonel", "alone", "stress", "fright", "horror",
#                  "panic", "scar", "terror", "fear", "concern", "tension", "strain",
#                  "alarm", "danger", "racist", "racism", "mental")

health_dfm_df <- tweet_dfm_df[health_dict]

health_dfm_df$total_words <- rowSums(health_dfm_df)
s_period2$total_words <- health_dfm_df$total_words
# saveRDS(s_period2, "data/period2.rds")

period2 <- read_rds("data/period2.rds")
test2 <- period2 %>% group_by(Datetime) %>% summarise(count = sum(total_words))
test2 <- test2 %>% subset(count != 0)

plot(test2$Datetime, test2$count)
matplot(test2$Datetime, test2$count, type = "l")

# dictionary 3 ------------------------------------------------------------
period3 <- full_tweets %>% subset(Datetime > "2021-09-30" &
                                  Datetime < "2022-07-01")
set.seed(123)
s_period3 <- period3[sample(nrow(period3), size = 10000),]

tweet_corpus <- Corpus(VectorSource(s_period3$Text))

corpus_clean <- tm_map(tweet_corpus, removePunctuation)
corpus_clean <- tm_map(corpus_clean, removeNumbers) 
corpus_clean <- tm_map(corpus_clean, content_transformer(tolower))
corpus_clean <- tm_map(corpus_clean, removeWords, stopwords())
corpus_clean <- tm_map(corpus_clean, stemDocument, language = "english")  
corpus_clean <- tm_map(corpus_clean, stripWhitespace)  

# head(tweet_corpus[[10000]]$content)
# head(corpus_clean[[10000]]$content)

clean_text <- data.frame(text = sapply(corpus_clean, as.character),
                         stringsAsFactors = FALSE)
s_period3$clean_text <- clean_text$text
tweet_dfm <- dfm(s_period3$clean_text)
tweet_dfm_df <- convert(tweet_dfm, to = "data.frame")

unique(str_extract(s_period3$clean_text, "alonei.*"))

health_dict <- c("depress", "anxieti", "stress",
                 "scari", "scare", "scared", "scary",
                 "fear", "frustrat",
                 "alarm", "danger", "racist", "racism", "mental")

# health_dict <- c("depress", "anx", "lonel", "alone", "stress", "fright", "horror",
#                  "panic", "scar", "terror", "fear", "concern", "tension", "strain",
#                  "alarm", "danger", "racist", "racism", "mental")

health_dfm_df <- tweet_dfm_df[health_dict]

health_dfm_df$total_words <- rowSums(health_dfm_df)
s_period3$total_words <- health_dfm_df$total_words
# saveRDS(s_period3, "data/period3.rds")

period3 <- read_rds("data/period3.rds")
test3 <- period3 %>% group_by(Datetime) %>% summarise(count = sum(total_words))
test3 <- test3 %>% subset(count != 0)

plot(test3$Datetime, test3$count)
matplot(test3$Datetime, test3$count, type = "l")


all_periods <- rbind(test, test2)
all_periods <- rbind(all_periods, test3)

#matplot(all_periods$Datetime, all_periods$count, type = "l", lwd=2)
#grid()

g1 <- ggplot(data = all_periods, aes(x=Datetime, y=count)) + 
      geom_line(size=1.05, color="firebrick") +
      ggtitle("Count of Dictionary Terms Across Time") #+
      #coord_cartesian(ylim = c(0,3))
g1 + theme_bw()

# period 1 ----------------------------------------------------------------

period1 <- full_tweets %>% subset(Datetime < "2018-11-01")
set.seed(123)
s_period1 <- period1[sample(nrow(period1), size = 10000),]

proc_1 <- textProcessor(s_period1$Text,
                             metadata = s_period1)
prep_1 <- prepDocuments(documents = proc_1$documents,
              vocab = proc_1$vocab,
              meta = proc_1$meta,
              lower.thresh = 100, verbose = TRUE)

stm_1 <- stm(documents= prep_1$documents,
                      vocab = prep_1$vocab,
                      K = 5,
                      seed = 123)
plot.STM(stm_1, type = "labels")

# period 2 ----------------------------------------------------------------
period2 <- full_tweets %>% subset(Datetime > "2019-08-31" &
                                    Datetime < "2020-06-01")
set.seed(123)
s_period2 <- period2[sample(nrow(period2), size = 10000),]

proc_2 <- textProcessor(s_period2$Text,
                             metadata = s_period2)
prep_2 <- prepDocuments(documents = proc_2$documents,
                        vocab = proc_2$vocab,
                        meta = proc_2$meta,
                        lower.thresh = 100, verbose = TRUE)

stm_2 <- stm(documents= prep_2$documents,
             vocab = prep_2$vocab,
             K = 20,
             seed = 123)
plot.STM(stm_2, type = "labels")
labelTopics(stm_2)

# period 3 ----------------------------------------------------------------
period3 <- full_tweets %>% subset(Datetime > "2021-09-30" &
                                    Datetime < "2022-07-01")
set.seed(123)
s_period3 <- period3[sample(nrow(period3), size = 10000),]

proc_3 <- textProcessor(s_period3$Text,
                             metadata = s_period3)
prep_3 <- prepDocuments(documents = proc_3$documents,
                        vocab = proc_3$vocab,
                        meta = proc_3$meta,
                        lower.thresh = 100, verbose = TRUE)

stm_3 <- stm(documents= prep_3$documents,
             vocab = prep_3$vocab,
             K = 5,
             seed = 123)
plot.STM(stm_3, type = "labels")