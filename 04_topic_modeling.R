
library(tidyverse)

# Alocação de Dirichlet latente -------------------------------------------

# Start with the topics output from the LDA run
lda_topics %>% 
  # Arrange the topics by word probabilities in descending order
  arrange(desc(beta))


# Produce a grouped summary of the LDA output by topic
lda_topics %>% 
  group_by(topic) %>% 
  summarize(
    # Calculate the sum of the word probabilities
    sum = sum(beta),
    # Count the number of terms
    n = n()
  )

word_probs <- lda_topics %>%
  # Keep the top 10 highest word probabilities by topic
  group_by(topic) %>% 
  top_n(10, beta) %>% 
  ungroup() %>%
  # Create term2, a factor ordered by word probability
  mutate(term2 = fct_reorder(term, beta))

# Plot term2 and the word probabilities
ggplot(word_probs, aes(x = term2, y = beta)) +
  geom_col() +
  # Facet the bar plot by topic
  facet_wrap(~ topic, scales = "free") +
  coord_flip()


library(tidytext)

tidy_twitter = readRDS('data/tidy_twitter.rds')

# Start with the tidied Twitter data
tidy_twitter %>% 
  # Count each word used in each tweet
  count(word, tweet_id) %>% 
  # Use the word counts by tweet to create a DTM
  cast_dtm(tweet_id, word, n)

tidy_twitter_subset = tidy_twitter[1:100,]


# Assign the DTM to dtm_twitter
dtm_twitter <- tidy_twitter_subset %>% 
  count(word, tweet_id) %>% 
  # Cast the word counts by tweet into a DTM
  cast_dtm(tweet_id, word, n)

# Coerce dtm_twitter into a matrix called matrix_twitter
matrix_twitter <- as.matrix(dtm_twitter)

# Print rows 1 through 5 and columns 90 through 95
matrix_twitter[1:5,83:88]


# Load the topicmodels package
library(topicmodels)

# Cast the word counts by tweet into a DTM
dtm_twitter <- tidy_twitter %>% 
  count(word, tweet_id) %>% 
  cast_dtm(tweet_id, word, n)

# Run an LDA with 2 topics and a Gibbs sampler
lda_out <- LDA(
  dtm_twitter,
  k = 2,
  method = "Gibbs",
  control = list(seed = 42)
)



# Glimpse the topic model output
glimpse(lda_out)

# Tidy the matrix of word probabilities
lda_topics <- lda_out %>% 
  tidy(matrix = "beta")

# Arrange the topics by word probabilities in descending order
lda_topics %>% 
  arrange(desc(beta))


# Run an LDA with 3 topics and a Gibbs sampler
lda_out2 <- LDA(
  dtm_twitter,
  k = 3,
  method = "Gibbs",
  control = list(seed = 42)
)

# Tidy the matrix of word probabilities
lda_topics2 <- lda_out2 %>% 
  tidy(matrix = 'beta')

# Arrange the topics by word probabilities in descending order
lda_topics2 %>% 
  arrange(desc(beta))



# Select the top 15 terms by topic and reorder term
word_probs2 <- lda_topics2 %>% 
  group_by(topic) %>% 
  top_n(15, beta) %>% 
  ungroup() %>%
  mutate(term2 = fct_reorder(term, beta))

# Plot word probs, color and facet based on topic
ggplot(
  word_probs2, 
  aes(term2, beta, fill = as.factor(topic))
) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()


# Select the top 15 terms by topic and reorder term
word_probs3 <- lda_topics3 %>% 
  group_by(topic) %>% 
  top_n(15, beta) %>% 
  ungroup() %>%
  mutate(term2 = fct_reorder(term, beta))

# Plot word_probs3, color and facet based on topic
ggplot(
  word_probs3, 
  aes(term2, beta, fill = as.factor(topic))
) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()
