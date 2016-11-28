rm(list=ls())

library(dplyr)
library(tidyr)

# Cleaning the words here
setwd('C:\\Users\\Edwin\\Desktop\\Edwin(10-08-2012)\\Assignments and Modules\\Coursera\\Data Science - John Hopkins\\Capstone\\')

# Mono-gram data
# 1. read data
monograms_df        <- read.table('mono_tokens_train_py.txt', sep = "\t", header = F,  quote = "\"",  stringsAsFactors = FALSE)
names(monograms_df) <- c('Token', 'cnt')
head(monograms_df)

# 2. clean data
# Create a quote-cleaning function
cleaningQuote <- function(my_df, col.names){

# Replace the "'" characters except the contractions
# contractions: "n't" , "'s", "i'm", "'ve", "'d", "'ll", "o'"

  for(i in 1:length(col.names)){
    
    dummy_str <- my_df[, names(my_df) == col.names[i]]
    
    contraction_flag1 <- grepl("^.*(n\\'t)$", dummy_str)
    contraction_flag2 <- grepl("^.*(\\'s)$", dummy_str)
    contraction_flag3 <- grepl("^i\\'m$", dummy_str)
    contraction_flag4 <- grepl("^.*(\\'ve)$", dummy_str)
    contraction_flag5 <- grepl("^.*(\\'d)$", dummy_str)
    contraction_flag6 <- grepl("^.*(\\'ll)$", dummy_str)
    contraction_flag7 <- grepl("^(o\\').*$", dummy_str)
    
    contraction_flags <- contraction_flag1 | contraction_flag2 | contraction_flag3 |
      contraction_flag4 | contraction_flag5 | contraction_flag6 | contraction_flag7
    
    dummy_str[!contraction_flags] <- gsub("'", "", x = dummy_str[!contraction_flags]) 
    my_df[, names(my_df) == col.names[i]] <- dummy_str
      
  }
  
  return(my_df)
}

monograms_df <- cleaningQuote(monograms_df, col.names = 'Token')
monograms_df <- monograms_df %>% group_by(Token) %>% summarise(cnt = sum(cnt)) %>% ungroup()
monograms_df <- monograms_df %>% arrange(desc(cnt))

write.csv(monograms_df, 'mono_tokens_train_py.csv', row.names = F)
# monograms_df <- read.csv('mono_tokens_train_py.csv',  stringsAsFactors = FALSE)

# Bi-gram data
# 1. read data
bigrams_df        <- read.table('bigram_tokens_train_py.txt', sep = "\t", header = F,  quote = "\"",  stringsAsFactors = FALSE)
names(bigrams_df) <- c('Token1', 'Token2', 'cnt')
head(bigrams_df)

bigrams_df <- cleaningQuote(bigrams_df, col.names = c('Token1', 'Token2'))
bigrams_df <- bigrams_df %>% group_by(Token1, Token2) %>% summarise(cnt = sum(cnt)) %>% ungroup()
bigrams_df <- bigrams_df %>% arrange(desc(cnt))

write.csv(bigrams_df, 'bigram_tokens_train_py.csv', row.names = F)
# bigrams_df <- read.csv('bigram_tokens_train_py.csv', stringsAsFactors = FALSE)

how_many_start_lines2 <- bigrams_df %>% filter(Token1 == '<s1>') %>% summarise(cnt = sum(cnt))
how_many_end_lines2   <- bigrams_df %>% filter(Token2 == '<e1>') %>% summarise(cnt = sum(cnt))

# top choice besides starting and ending are "in the", "for the", "of the", "thanks for", "at the"


# Review criterialess 
# Does the link lead to an HTML page describing the exploratory analysis of the training data set?
# Has the data scientist done basic summaries of the three files? Word counts, line counts and basic data tables?
# Has the data scientist made basic plots, such as histograms to illustrate features of the data?
# Was the report written in a brief, concise style, in a way that a non-data scientist manager could appreciate?

# Questions to consider
# 
# Some words are more frequent than others - what are the distributions of word frequencies?
# What are the frequencies of 2-grams and 3-grams in the dataset?
# How many unique words do you need in a frequency sorted dictionary to cover 50% of all word instances in the language? 90%?
# How do you evaluate how many of the words come from foreign languages?
# Can you think of a way to increase the coverage -- identifying words that may not be in the corpora or using a smaller number of words in the dictionary to cover the same number of phrases?



#check the data
#how many "i"?

how_many_i <- bigrams_df %>% filter(Token1 == 'i' | Token2 == 'i')
sum(how_many_i$cnt)/2

sum(monograms_df$cnt[monograms_df$Token == 'i'])

#Trying TM package
library(tm)
data("acq")
data("crude")
crude[[1]]
(f <- content_transformer(function(x, pattern) gsub(pattern, "", x)))
tm_map(crude, f, "[[:digit:]]+")[[1]]


docs <- data.frame(c("This is a text.", "This another one."))
(ds <- DataframeSource(docs))
inspect(VCorpus(ds))


docs <- Corpus(DirSource('TextMining'))
writeLines(as.character(docs[[1]]))

toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern, ' ', x))})
docs <- tm_map(docs, toSpace, '-')
docs <- tm_map(docs, toSpace, ':')
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, content_transformer(tolower))
writeLines(as.character(docs[[1]]))

docs <- c("This is a text.", "This another one.")
vs <- VectorSource(docs)
elem <- getElem(stepNext(vs))
(result <- readPlain(elem, "en", "id1"))
meta(result)

library(ngram)
mct <- MC_tokenizer("Two sentence here....I'm handsome once again!")
x <- ngram('Hello World! Hi Hi Aunti! Hi Aunti Again! Hi Aunit Again! and Again!', n = 2, sep = " ")
print(x, output="full")
