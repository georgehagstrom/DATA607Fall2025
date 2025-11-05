# Coding vignette on sentiment analysis of movie reviews


library(tidyverse)
library(tidytext)

# I have downloaded a dataset of movie reviews from here https://ai.stanford.edu/~amaas/data/sentiment/

# The dataset is divided into a training and testing directory (and an unsupervised directory).
# Within the training and testing directories, there are separate directories for positive and negative reviews.

# We want to load both the positive and negative sentiment files into a tibble together so that we can perform sentiment analysis


path = "/home/georgehagstrom/work/Teaching/DATA607Fall2025/aclImdb/train/"

# This is an example of a file. The number after the underscore indicates the rating given

read_file(str_c(path,"neg/11010_1.txt"))


# These commands create lists of all the files containing reviews in the negative and positive review directories

neg_review_files = list.files(str_c(path,"neg"),pattern = "[.]txt",full.names = TRUE)
pos_review_files = list.files(str_c(path,"pos"),pattern = "[.]txt",full.names = TRUE)

# We read in the files. the set_names makes it so the actual filename and not the entire path is the name of each file

neg_reviews = neg_review_files |> set_names(basename) |> map(read_file)
pos_reviews = pos_review_files |> set_names(basename) |> map(read_file)

# Turn them into a tibble using some functions

parse_file = function(review){
  tibble(review = review)
}

# Processing the files

neg_reviews_df = neg_reviews |> map(parse_file) |> 
  list_rbind(names_to = "file_name") |> 
  mutate(file_name = str_remove(file_name,".txt")) |> 
  separate(file_name,c("index","rating"),sep = "_") |> 
  mutate(index = as.numeric(index),rating = as.numeric(rating)) |> arrange(index)

# Making sure the indices between the positive and negative reviews are distinct

max_index = max(neg_reviews_df$index)
max_index

pos_reviews_df = pos_reviews |> map(parse_file) |> 
  list_rbind(names_to = "file_name") |> 
  mutate(file_name = str_remove(file_name,".txt")) |> 
  separate(file_name,c("index","rating"),sep = "_") |> 
  mutate(index = as.numeric(index) + max_index,rating = as.numeric(rating)) |> arrange(index)


# Formaly putting them together

reviews_df = neg_reviews_df |> bind_rows(pos_reviews_df)

reviews_df


# Tokenizing by words, then attaching sentiments

reviews_tidy = reviews_df |> unnest_tokens(word,review) |> inner_join(get_sentiments("afinn"))


# Computing the sentiment for each review

reviews_sentiment = reviews_tidy |> group_by(index,rating) |> summarise(sentiment = sum(value)/n()) |> ungroup()


reviews_sentiment

# Plotting the sentiment

reviews_sentiment |>  ggplot(aes(group=rating,y=sentiment)) + geom_boxplot()

# Define some modifier terms

extreme = c("extreme","extremely","very","greatly","wildly","absolutely",   "utterly","entirely","fully","vastly","staggeringly","totally")
extreme_words = tibble(word = extreme, mod_value = 2)

# Define some negative words

not_words = tibble(word = c("not","don't"),mod_value = -1)

mod_words = not_words |> bind_rows(extreme_words)
mod_words

# Here we unnest into bigrams, then add the afinn sentiment to the second bigram word and the
# modifier to the first bigram word

reviews_double = reviews_df |> unnest_tokens(bigram,review,token="ngrams",n=2) |> 
  filter(!is.na(bigram)) |> 
  separate(bigram,c("word1","word2")) |> 
  inner_join(get_sentiments("afinn"),by = join_by("word2" == "word")) |> 
  left_join(mod_words,by = join_by("word1" == "word"))


# Multiply to do the modification

reviews_double_mod = reviews_double |> mutate(mod_value = if_else(is.na(mod_value),
                                                                  value,
                                                                  value*mod_value))
# Computing the modified rating


reviews_mod_sentiment = reviews_double_mod |> group_by(index,rating) |> 
  summarise(sentiment = sum(mod_value)/n())

reviews_mod_sentiment |> ggplot(aes(group=rating,y=sentiment)) + geom_boxplot()

# Things changed but not by much

cor(reviews_sentiment$rating,reviews_sentiment$sentiment)

cor(reviews_mod_sentiment$rating,reviews_mod_sentiment$sentiment)


