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
  filter(str_detect(token, "^#"), token != "#energiewende") |> 
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

# Save results as R data file
saveRDS(hashtag_stats, './hashtag_stats.rds')


#######################################################
### Visualize frequency change of selected hashtags ###
#######################################################

library(tidyverse)
library(ggtext)
library(showtext)

# Load saved results
hashtag_stats <- readRDS('./hashtag_stats.rds')

# Load font


# Load manually curated list of hashtags (incl. translation) for visualization
hashtags_of_interest <- read_csv("hashtags-of-interest.csv")

# Filter and reformat plot
hashtag_plot_data <- hashtag_stats |> 
  filter(feature %in% hashtags_of_interest$hashtag) |> 
  pivot_longer(cols = c(f_target, f_reference), names_to = 'corpus', values_to = 'frequency') |> 
  mutate(before_war = corpus == 'f_reference') |> 
  select(hashtag = feature, n_postwar = n_target, frequency, before_war) |> 
  right_join(hashtags_of_interest, by = 'hashtag') |> 
  mutate(hashtag = fct_reorder(hashtag, n_postwar),
         translation = fct_reorder(translation, n_postwar))

# Prepare plot labels
y_hashtag_labels <- paste0(
  levels(hashtag_plot_data$hashtag), 
  '<br>**', 
  levels(hashtag_plot_data$translation), 
  '**')

plot_title <- '**Frequent hashtags in Tweets about the *Energiewende*<br>since the beginning of the war (24.2.-23.3.2022)**'
plot_description <- "Beschreibung der Grafik"
legend_labels <- c('Frequency before the war', 'Frequency increase since the war')

plot <- hashtag_plot_data |>
  
  # Data mapping 
  ggplot(aes(x = frequency*100, y = hashtag, fill = before_war)) +
  geom_col(width = 0.4, position = 'identity') +
  
  # Axes
  #scale_y_discrete() +
  scale_y_discrete(labels = y_hashtag_labels) +
  scale_x_continuous(expand = c(.02,0)) +
  
  # Fill color
  scale_fill_manual(
    breaks = c(TRUE, FALSE),
    labels = legend_labels, 
    values = c('#A2AADB','#5564C2'), 
    guide = guide_legend(title = NULL)) +
  
  # Title, caption and description
  labs(
    title = plot_title,
    caption = plot_description,
    x = "Hashtag frequency in percent",
    y = NULL
  ) +
  
  # Format plot
  theme_bw() +
  theme(
    plot.title = element_markdown(size=17, lineheight = 1.2),
    plot.caption = element_markdown(size=11, lineheight = 1.1),
    axis.text.y = element_markdown(size=11, lineheight = 1.1),
    axis.title.x = element_markdown(size=11, lineheight = 1.1),
    
    # Format legend
    legend.text = element_markdown(size=11, lineheight = 1.1),
    legend.title = element_markdown(size=11, lineheight = 1.1),
    
    legend.position = c(0.72,0.1),
    
    # Remove grid
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank()
  )


# Preview plot
plot

# Save plot to file
ggsave("results/hashtag-frequency-change.jpg", plot, width = 2500, height = 2900, units = "px")  


