

## Visualizing non-complaints

word_counts <- tidy_twitter %>% 
  # Only keep the non-complaints
  filter(complaint_label == 'Non-Complaint') %>% 
  count(word) %>% 
  filter(n > 150)

# Create a bar plot using the new word_counts
ggplot(word_counts, aes(word, n)) +
  geom_col() +
  coord_flip() +
  # Title the plot "Non-Complaint Word Counts"
  ggtitle('Non-Complaint Word Counts')




## Adding custom stop words

custom_stop_words <- tribble(
  # Column names should match stop_words
  ~ word, ~ lexicon,
  # Add http, win, and t.co as custom stop words
  "http", "CUSTOM",
  "win", "CUSTOM",
  "t.co", "CUSTOM"
)

# Bind the custom stop words to stop_words
stop_words2 <- stop_words %>% 
  bind_rows(custom_stop_words)



## Visualizing word counts using factors

word_counts <- tidy_twitter %>% 
  filter(complaint_label == "Non-Complaint") %>% 
  count(word) %>% 
  # Keep terms that occur more than 100 times
  filter(n>100) %>% 
  # Reorder word as an ordered factor by word counts
  mutate(word2 = fct_reorder(word, n))

# Plot the new word column with type factor
ggplot(word_counts, aes(word2, n)) +
  geom_col() +
  coord_flip() +
  ggtitle("Non-Complaint Word Counts")




## Counting by product and reordering

word_counts <- tidy_twitter %>%
  # Count words by whether or not its a complaint
  count(word, complaint_label) %>%
  # Group by whether or not its a complaint
  group_by(complaint_label) %>%
  # Keep the top 20 words
  top_n(20, n) %>%
  # Ungroup before reordering word as a factor by the count
  ungroup() %>%
  mutate(word2 = fct_reorder(word, n))



## Visualizing word counts with facets

# Include a color aesthetic tied to whether or not its a complaint
ggplot(word_counts, aes(x = word2, y = n, fill = complaint_label)) +
  # Don't include the lengend for the column plot
  geom_col(show.legend = FALSE) +
  # Facet by whether or not its a complaint and make the y-axis free
  facet_wrap(~complaint_label, scales = "free_y") +
  # Flip the coordinates and add a title: "Twitter Word Counts"
  coord_flip() +
  ggtitle("Twitter Word Counts")


## Creating a word cloud

# Load the wordcloud package
library(wordcloud)

# Compute word counts and assign to word_counts
word_counts <- tidy_twitter %>% 
  count(word)

wordcloud(
  # Assign the word column to words
  words = word_counts$word, 
  # Assign the count column to freq
  freq = word_counts$n,
  max.words = 30
)


## Adding a splash of color

# Compute complaint word counts and assign to word_counts
word_counts <- tidy_twitter %>% 
  filter(complaint_label == 'Complaint') %>% 
  count(word)

# Create a complaint word cloud of the top 50 terms, colored red
wordcloud(
  words = word_counts$word, 
  freq = word_counts$n,  
  max.words = 50, 
  colors = 'red'
)
