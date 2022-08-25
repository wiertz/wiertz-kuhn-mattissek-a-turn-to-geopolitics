library(openxlsx)
library(tidyverse)
library(tidytext)
library(quanteda)
library(quanteda.textstats)


##################################
### Analyze keyword statistics ###
##################################

# Load tweet dataset saved by download-tweets.R
tweets <- readRDS("./tweets.rds")

# Tokenize and count hashtags per tweet
hashtags <- tweets %>% 
  select(tweet_id, date, text) %>% 
  unnest_tokens(output = token, input = text, token = "tweets") %>% 
  filter(str_detect(token, "^#")) |> 
  count(tweet_id, date, token)

# Create vector to group tweets as pre-/post 24.2.22
tweets_before_war <- hashtags |> 
  distinct(tweet_id, date) |> 
  mutate(before_war = date < "2022-02-24") |> 
  pull(before_war)

# Calculate hashtag statistics using quanteda textstats,
# filter for hashtags with positive and significant frequency 
# change (p < 0.01), and sort using total occurrences after 24.2
hashtag_stats <- hashtags |> 
  cast_dfm(document = tweet_id, term = token, value = n) |> 
  dfm_group(tweets_before_war) |> 
  textstat_keyness(measure = "lr", sort = F) |> 
  tibble() |> 
  mutate(
    f_target = n_target / sum(n_target),
    f_reference = n_reference / sum(n_reference),
    f_change = f_target - f_reference
  ) |> 
  filter(p <= 0.01, f_change > 0) |>
  arrange(-n_target)

# Save results as Excel spreadsheet
write.xlsx(hashtag_stats, "results/hashtag-stats.xlsx")


#######################################################
### Visualize frequency change of selected hashtags ###
#######################################################

# Load manually curated list of hashtags (incl. translation) for visualization
hashtags_of_interest <- read_csv("hashtags-of-interest.csv")

# Create plot
plot <- hashtag_stats |>
  filter(feature %in% hashtags_of_interest$hashtag) |> 
  ggplot(aes(x = f_change * 100, y = fct_reorder(feature, f_change))) +
  geom_col(fill = "deepskyblue4", width = 0.5) +
  scale_y_discrete(labels = paste0(hashtags_of_interest$hashtag, "\n(", hashtags_of_interest$translation, ") ")) +
  xlab(
    label = expression("Change in frequency per 100 words (p < 0.01)")
  ) +
  ylab(label = NULL) +
  ggtitle(
    "Frequency change of selected hashtags co-occuring with 'energiewende'",
    subtitle = "comparing tweets from 24.2.21-23.2.22 and 24.2.-30.5.22"
    ) +
  theme_bw() +
  theme(
    axis.text=element_text(size=11),
    legend.position = "None",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
    )

# Preview plot
plot

# Save plot to file
ggsave("results/hashtag-frequency-change.jpg", plot, width = 2400, height = 2900, units = "px")  


