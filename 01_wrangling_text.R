
# Load the tidyverse packages
library(tidyverse)

twitter_data <- readRDS("data/ch_1_twitter_data.rds")

# Print twitter_data
twitter_data %>% glimpse

twitter_data %>% distinct(tweet_id)            # 7044
twitter_data %>% distinct(complaint_label)     # 2
twitter_data %>% distinct(usr_followers_count) # 2144

# Print just the complaints in twitter_data
twitter_data %>% 
  filter(complaint_label == 'Complaint')

# Qual o total de reclamações nos dados? Apenas 1676 reclamações ou 23,79%




# Start with the data frame
twitter_data %>% 
  # Group the data by whether or not the tweet is a complaint
  group_by(complaint_label) %>% 
  # Compute the mean, min, and max follower counts
  summarize(
    avg_followers = mean(usr_followers_count),
    min_followers = min(usr_followers_count),
    max_followers = max(usr_followers_count)
  )

# Você espera que quem reclama tenha mais usuários ou menos usuários, em média, 
# do que quem não reclama? Na média, quem não reclama tem um número mais elevado 
# de seguidores no twitter





# twitter_dataé composto por reclamações e não reclamações, conforme indicado pela 
# coluna complaint_label, e também inclui uma coluna indicando se o usuário é 
# verificado ou não (ou seja, foi confirmado pelo Twitter como sendo quem diz ser) 
# chamada usr_verified. Observe que a coluna é do tipo <lgl>, o que significa 
# lógico. Os usuários verificados reclamam mais?

twitter_data %>% 
  # Filter for just the complaints
  filter(complaint_label == 'Complaint') %>% 
  # Count the number of verified and non-verified users
  count(usr_verified)

# Já que você pode usar o count(), por que se preocupar em contar linhas 
# em um grupo como parte de um resumo agrupado? Às vezes, você deseja um resumo 
# mais detalhado e saber como calcular uma contagem como parte de um resumo 
# agrupado que mistura resumos numéricos e categóricos pode ser útil.




twitter_data %>% 
  # Group by whether or not a user is verified
  group_by(usr_verified) %>% 
  summarize(
    # Compute the average number of followers
    avg_followers = mean(usr_followers_count),
    # Count the number of users in each category
    n = n()
  )




## Tokenizing and counting

# Load the tidyverse and tidytext packages
library(tidyverse)
library(tidytext)

tidy_twitter <- twitter_data %>% 
  # Tokenize the twitter data
  unnest_tokens(word,tweet_text) 

tidy_twitter %>% 
  # Compute word counts
  count(word) %>% 
  # Arrange the counts in descending order
  arrange(desc(n))




## Cleaning and counting

tidy_twitter <- twitter_data %>% 
  # Tokenize the twitter data
  unnest_tokens(word,tweet_text) %>% 
  # Remove stop words
  anti_join(stop_words)

tidy_twitter %>% 
  # Filter to keep complaints only
  filter(complaint_label == "Complaint") %>% 
  # Compute word counts and arrange in descending order
  count(word) %>% 
  arrange(desc(n))

## Visualizing complaints
word_counts <- tidy_twitter %>% 
  filter(complaint_label == "Complaint") %>% 
  count(word) %>% 
  # Keep words with count greater than 100
  filter(n > 100)

# Create a bar plot using word_counts with x = word
ggplot(word_counts, aes(word, n)) +
  geom_col() +
  # Flip the plot coordinates
  coord_flip()



