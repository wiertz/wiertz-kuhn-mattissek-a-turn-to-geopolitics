library(openxlsx)
library(academictwitteR)
library(dplyr)
library(lubridate)
library(stringr)


#########################
### 1 Download tweets ###
#########################


# Bearer token needs to be set before the following commands work.
# Call set_bearer() for details.

get_all_tweets(
  query = c("energiewende"),
  start_tweets = "2021-02-24T00:00:00Z",
  end_tweets = "2022-06-01T00:00:00Z",
  is_retweet = FALSE,
  remove_promoted = TRUE,
  data_path = "data_full/",
  bind_tweets = FALSE,
  n = Inf
)


######################
### 2 Process data ###
######################


# Load tweets into tabular form
raw_tweets <- bind_tweets("data_full/", output_format = "tidy") 

# Extract date from tweet timestamp
# Add column with popularity (likes + retweets + quotes),
# Create link to original tweet from username and tweet id
tweets <- raw_tweets %>% 
  mutate(
    date = date(as_datetime(created_at)),
    popularity = retweet_count + like_count + quote_count,
    link = str_c(
      'https://twitter.com/',
      user_username, 
      '/status/', 
      tweet_id)
    )

# Save as R dataset
saveRDS(tweets, "tweets.rds")


####################################################
### 3 Export most popular tweets since 24.2.2022 ###
####################################################

tweets <- readRDS("tweets.rds")

# Filter and slice 100 most popular tweets
tweets_for_qualitative_analysis <- tweets %>%
  filter(date >= "2022-02-24") %>% 
  select(
    tweet_id,
    link,
    user_username,
    user_description,
    user_followers_count,
    created_at,
    text,
    sourcetweet_text,
    popularity
  ) %>% 
  slice_max(popularity, n = 100)

# Set class of column link to "hyperlink" for correct reckognition in MS Excel
class(tweets_for_qualitative_analysis$link) <- "hyperlink"

# Write xlsx-File
write.xlsx(tweets_for_qualitative_analysis, "results/tweets.xlsx", asTable = TRUE, overwrite = TRUE)
