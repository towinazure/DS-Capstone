rm(list=ls())
library(tm)
library(stringi)
library(RWeka)

base_wd <- 'C:\\Users\\Edwin\\Desktop\\Edwin(10-08-2012)\\Assignments and Modules\\Coursera\\Data Science - John Hopkins\\Capstone\\'

setwd(base_wd)
con <- file('en_US.news.txt')
news_lines <- readLines(con, encoding = 'UTF-8', skipNul = TRUE)
close(con)

con <- file('en_US.twitter.txt')
twitter_lines <- readLines(con, encoding = 'UTF-8', skipNul = TRUE)
close(con)

con <- file('en_US.blogs.txt')
blogs_lines <- readLines(con, encoding = 'UTF-8', skipNul = TRUE)
close(con)


#Partition Data
pp <- c(0.7, 0.2, 0.1) #Train, Valid, Test Portion

partitionData <- function(tvt.portion, n){
  
  train_n    <- floor(tvt.portion[1] * n)
  valid_n    <- floor(tvt.portion[2] * n)
  test_n     <- n - train_n - valid_n
  
  train_pos  <- sample(1:n, size = train_n)
  rest       <- (1:n)[-train_pos]
  valid_flag <- sample(1:length(rest), size = valid_n)
  valid_pos  <- rest[valid_flag]
  test_pos   <- rest[-valid_flag]
  
  return(list(train_pos = train_pos,
    valid_pos = valid_pos,
    test_pos = test_pos))
  
}

set.seed(7689)
pd_result  <- partitionData(tvt.portion = pp,  n = length(new_lines))

news_train <- news_lines[pd_result$train_pos]
news_valid <- news_lines[pd_result$valid_pos]
  news_test  <- news_lines[pd_result$test_pos]

pd_result  <- partitionData(tvt.portion = pp,  n = length(twitter_lines))

twitter_train <- twitter_lines[pd_result$train_pos]
twitter_valid <- twitter_lines[pd_result$valid_pos]
twitter_test  <- twitter_lines[pd_result$test_pos]

pd_result  <- partitionData(tvt.portion = pp,  n = length(blogs_lines))

blogs_train <- blogs_lines[pd_result$train_pos]
blogs_valid <- blogs_lines[pd_result$valid_pos]
blogs_test  <- blogs_lines[pd_result$test_pos]

remove_punc <- function(x){
  
  # Filter non-alphabetical character and quote
  x <- gsub("[^a-z']"," ", x)
  return(x)

}

reduceShortForm <- function(x){
  
  # Filter non-alphabetical character
  x <- gsub("[^a-z']"," ", x)
  short_form_flag <- grepl("(o')|('(ve)|(d)|(ll)|(s)(m))|(n't)", x)
  if(short_form_flag){
    return(x)
  }else{
    x <- gsub("'", " ", x)
    return(x)
  }
}



CleanCorpus <- function(x) {
  
  y <- Corpus(VectorSource(x))
  y <- tm_map(y, content_transformer(remove_punc))
  y <- tm_map(y, content_transformer(tolower))
  y <- tm_map(y, content_transformer(reduceShortForm))
  # y <- tm_map(y, removePunctuation) 
  # y <- tm_map(y, removeNumbers) 
  # y <- tm_map(x, removeWords, stopwords("english")) 
  y <- tm_map(y,  stripWhitespace)  
  # y <- tm_map(y, PlainTextDocument) 
  
  return(y)
}

train_set <-  c(news_train, twitter_train, blogs_train)
valid_set <-  c(news_valid, twitter_valid, blogs_valid)
test_set  <-  c(news_test, twitter_test, blogs_test)

save(train_set, valid_set, test_set, file = 'train_valid_test.RData')
rm(list = ls())
load(file = 'train_valid_test.RData')

train_corpus <- CleanCorpus(train_set)

##Document Term Matrix   
dt_train <- DocumentTermMatrix(train_corpus, control = list(wordLenghts=c(1, Inf)))  

#Term Document Matrix : Transpose of Document Term Matrix
# tdm_sample  <- TermDocumentMatrix(corpus_sample_1p)  
dt_train_reduced <- removeSparseTerms(dt_train, 0.99)   

words      <- Terms(dt_train_reduced)
words_freq <- sort(rowSums(as.matrix(dt_train_reduced)), decreasing = T)

words_df <- data.frame(words = words, freq=words_freq)   
head(words_df)  

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
tdm_sample_2gram <- TermDocumentMatrix(corpus_sample_1p, control = list(tokenize = BigramTokenizer))
tdms_sample_2gram <- removeSparseTerms(tdm_sample_2gram, 0.99) 
freq_sample_2gram <- sort(rowSums(as.matrix(tdms_sample_2gram)), decreasing = T)

