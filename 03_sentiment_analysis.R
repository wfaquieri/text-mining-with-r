
## Counting the NRC sentiments

# Load the tidyverse and tidytext packages
library(tidyverse)
library(tidytext)

# Count the number of words associated with each sentiment in nrc
get_sentiments("nrc") %>% 
  count(sentiment) %>% 
  # Arrange the counts in descending order
  arrange(desc(n))


## Visualizing the NRC sentiments

# Pull in the nrc dictionary, count the sentiments and reorder them by count
sentiment_counts <- get_sentiments('nrc') %>% 
  count(sentiment) %>% 
  mutate(sentiment2 = fct_reorder(sentiment, n))

# Visualize sentiment_counts using the new sentiment factor column
ggplot(sentiment_counts, aes(sentiment2, n)) +
  geom_col() +
  coord_flip() +
  # Change the title to "Sentiment Counts in NRC", x-axis to "Sentiment", and y-axis to "Counts"
  labs(
    title = "Sentiment Counts in NRC",
    x = "Sentiment",
    y = "Counts"
  )



## Counting sentiment

# Join tidy_twitter and the NRC sentiment dictionary
sentiment_twitter <- tidy_twitter %>% 
  inner_join(get_sentiments("nrc"),by = "word")

# Count the sentiments in sentiment_twitter
sentiment_twitter %>% 
  count(sentiment) %>% 
  # Arrange the sentiment counts in descending order
  arrange(desc(n))



## Visualizing sentiment

word_counts <- tidy_twitter %>% 
  # Append the NRC dictionary and filter for positive, fear, and trust
  inner_join(get_sentiments('nrc')) %>% 
  filter(sentiment %in% c('positive','fear','trust')) %>%
  # Count by word and sentiment and keep the top 10 of each
  count(word,sentiment) %>% 
  group_by(sentiment) %>% 
  top_n(10,n) %>% 
  ungroup() %>% 
  # Create a factor called word2 that has each word ordered by the count
  mutate(word2 = fct_reorder(word,n))


# Create a bar plot out of the word counts colored by sentiment
ggplot(word_counts, aes(word2,n,fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  # Create a separate facet for each sentiment with free axes
  facet_wrap(~ sentiment,scales = 'free') +
  coord_flip() +
  # Title the plot "Sentiment Word Counts" with "Words" for the x-axis
  labs(
    title = "Sentiment Word Counts",
    x = "Words"
  )



## Practicing reshaping data

tidy_twitter %>% 
  # Append the NRC sentiment dictionary
  inner_join(get_sentiments('nrc')) %>% 
  # Count by complaint label and sentiment
  count(complaint_label,sentiment) %>% 
  # Spread the sentiment and count columns
  spread(sentiment,n)


tidy_twitter %>% 
  # Append the afinn sentiment dictionary
  inner_join(get_sentiments('afinn')) %>% 
  # Group by both complaint label and whether or not the user is verified
  group_by(complaint_label,usr_verified) %>% 
  # Summarize the data with an aggregate_value = sum(value)
  summarise(aggregate_value = sum(value)) %>% 
  # Spread the complaint_label and aggregate_value columns
  spread(complaint_label,aggregate_value) %>% 
  mutate(overall_sentiment = Complaint + `Non-Complaint`)


## Visualizing sentiment by complaint type

sentiment_twitter <- tidy_twitter %>% 
  # Append the bing sentiment dictionary
  inner_join(get_sentiments('bing')) %>% 
  # Count by complaint label and sentiment
  count(complaint_label,sentiment) %>% 
  # Spread the sentiment and count columns
  spread(sentiment,n) %>% 
  # Compute overall_sentiment = positive - negative
  mutate(overall_sentiment = positive - negative)


# Create a bar plot out of overall sentiment by complaint label, colored by complaint label as a factor
ggplot(
  sentiment_twitter, 
  aes(x = complaint_label, y = overall_sentiment, fill = as.factor(complaint_label))
) +
  geom_col(show.legend = FALSE) +
  coord_flip() + 
  # Title the plot "Overall Sentiment by Complaint Label" with an "Airline Twitter Data" subtitle
  labs(
    title = "Overall Sentiment by Complaint Label",
    subtitle = "Airline Twitter Data"
  )
