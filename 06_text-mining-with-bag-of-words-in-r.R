
# class, 16-02-2022.

library(qdap)

text <- "Text mining usually involves the process of structuring the input text. The overarching goal is, essentially, to turn text into data for analysis, via the application of natural language processing (NLP) and analytical methods."

frequent_terms <- freq_terms(text, 4)

plot(frequent_terms)



# -------------------------------------------------------------------------

new_text = 
"DataCamp is the first online learning platform that focuses on building the best learning experience specifically for Data Science. We have offices in New York, London, and Belgium, and to date, we trained over 3.8 million (aspiring) data scientists in over 150 countries. These data science enthusiasts completed more than 185 million exercises. You can take free beginner courses, or subscribe for $29/month to get access to all premium courses."

# Load qdap
library(qdap)

# Print new_text to the console
new_text

# Find the 10 most frequent terms: term_count
term_count <- freq_terms(new_text, 10)

# Plot term_count
plot(term_count)



# -------------------------------------------------------------------------


# Import text data from CSV, no factors
tweets <- read.csv('data/coffee.csv',stringsAsFactors = FALSE)

# View the structure of tweets
str(tweets)

# Isolate text from tweets
coffee_tweets <- tweets$text

## Make the vector a VCorpus object (1)

# Load tm
library(tm)

# Make a vector source from coffee_tweets
# Definition: a corpus is a collection of documents

# There are two kinds of the corpus data type, the permanent corpus, PCorpus, 
# and the volatile corpus, VCorpus. In essence, the difference between the 
# two has to do with how the collection of documents is stored on your computer. 
# In this course, we will use the volatile corpus, which is held in your 
# computer's RAM rather than saved to disk, just to be more memory efficient.

# VCorpus > Memoria RAM > MORE EFFICIENT
# PCorpus > permanent corpus > Disc > less efficient

coffee_source = VectorSource(coffee_tweets)



## Make the vector a VCorpus object (2)

# Make a volatile corpus from coffee_corpus
coffee_corpus <- VCorpus(coffee_source)

# Print out coffee_corpus
coffee_corpus

# Print the 15th tweet in coffee_corpus
coffee_corpus[[15]]

# Print the contents of the 15th tweet in coffee_corpus
coffee_corpus[[15]][1]

# Now use content to review the plain text of the 10th tweet
content(coffee_corpus[[10]])




## Make a VCorpus from a data frame

# Create a DataframeSource from the example text
df_source <- DataframeSource(example_text)

# Convert df_source to a volatile corpus
df_corpus <- VCorpus(df_source)

# Examine df_corpus
df_corpus

# Examine df_corpus metadata
meta(df_corpus)

# Compare the number of documents in the vector source
vec_corpus

# Compare metadata in the vector corpus
meta(vec_corpus)
