rm(list=ls())
library(dplyr)
library(tidyr)

base_wd <- 'C:\\Users\\Edwin\\Desktop\\Edwin(10-08-2012)\\Assignments and Modules\\Coursera\\Data Science - John Hopkins\\Capstone\\'
load(file = paste(base_wd, 'tokens_df.RData', sep =""))

cleaningQuote <- function(my_df, col.names){
  
  # Replace the "'" characters except the contractions
  # contractions: "n't" , "'s", "i'm", "'ve", "'d", "'ll", "o'"
  
  for(i in 1:length(col.names)){
    
    dummy_str <- as.vector(my_df[[col.names[i]]])
    
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
    
    #Replace <s1> by s1 and <e1> by e1 because of the html problem etc.
    dummy_str <- gsub('<s>', 's1', dummy_str)
    dummy_str <- gsub('<e>', 'e1', dummy_str)
    dummy_str <- gsub('<|>', '', dummy_str)
    
    my_df[, names(my_df) == col.names[i]] <- dummy_str
    
  }
  
  return(my_df)
}


##Construct Trigram
blogs_trigrams_df        <- read.table(paste(base_wd, 'blogs_tri_tokens_train_py.txt', sep =""),
  sep = "\t", header = F,  quote = "\"",  stringsAsFactors = FALSE)
names(blogs_trigrams_df) <- c('Token1', 'Token2', 'Token3', 'cnt')

blogs_trigrams_df <- cleaningQuote(blogs_trigrams_df, col.names = c('Token1', 'Token2', 'Token3'))
blogs_trigrams_df <- blogs_trigrams_df %>% group_by(Token1, Token2, Token3) %>% summarise(cnt = sum(cnt)) %>% ungroup()
blogs_trigrams_df <- blogs_trigrams_df %>% filter(cnt > 2)
blogs_trigrams_df <- blogs_trigrams_df %>% arrange(desc(cnt))



twitter_trigrams_df        <- read.table(paste(base_wd, 'twitter_tri_tokens_train_py.txt', sep =""),
  sep = "\t", header = F,  quote = "\"",  stringsAsFactors = FALSE)
names(twitter_trigrams_df) <- c('Token1', 'Token2', 'Token3', 'cnt')

twitter_trigrams_df <- cleaningQuote(twitter_trigrams_df, col.names = c('Token1', 'Token2', 'Token3'))
twitter_trigrams_df <- twitter_trigrams_df %>% group_by(Token1, Token2, Token3) %>% summarise(cnt = sum(cnt)) %>% ungroup()
twitter_trigrams_df <- twitter_trigrams_df %>% filter(cnt > 2)
twitter_trigrams_df <- twitter_trigrams_df %>% arrange(desc(cnt))



news_trigrams_df        <- read.table(paste(base_wd, 'news_tri_tokens_train_py.txt', sep =""),
  sep = "\t", header = F,  quote = "\"",  stringsAsFactors = FALSE)
names(news_trigrams_df) <- c('Token1', 'Token2', 'Token3', 'cnt')

news_trigrams_df <- cleaningQuote(news_trigrams_df, col.names = c('Token1', 'Token2', 'Token3'))
news_trigrams_df <- news_trigrams_df %>% group_by(Token1, Token2, Token3) %>% summarise(cnt = sum(cnt)) %>% ungroup()
news_trigrams_df <- news_trigrams_df %>% filter(cnt > 2)
news_trigrams_df <- news_trigrams_df %>% arrange(desc(cnt))


load(file = paste(base_wd, 'tokens_df2.RData', sep =""))

#Save as .RData
save(blogs_trigrams_df,
twitter_trigrams_df,
 news_trigrams_df,
  file = paste(base_wd, 'tokens_df2.RData', sep =""))

#Save as .RData
save(blogs_unigrams_df, blogs_bigrams_df,
  twitter_unigrams_df, twitter_bigrams_df,
  news_unigrams_df, news_bigrams_df, file = paste(base_wd, 'tokens_df.RData', sep =""))

predictBigram <- function(word){
  
  model_df <- rbind(news_bigrams_df, twitter_bigrams_df, blogs_bigrams_df)  
  word = tolower(word)

  word_df <- model_df %>% filter(Token1 == word)
  if(nrow(word_df) > 1){
    
    word_p_df <- data.frame(p = word_df$cnt/sum(word_df$cnt), 
    Input = word, Word_predicted = word_df$Token2)
    
  return(word_p_df)
  }else{
    
    return(NULL)
    
  }
}

predictTrigram <- function(words){
  

  model_df <- rbind(news_trigrams_df, twitter_trigrams_df, blogs_trigrams_df)  
  words = tolower(words)
  
  word_df <- model_df %>% filter(Token1 == words[1] & Token2 == words[2])
  if(nrow(word_df) > 1){
    
    word_p_df <- data.frame(p = word_df$cnt/sum(word_df$cnt), 
      Input = paste(words, collapse = ' '), Word_predicted = word_df$Token3)
    
    return(word_p_df)
  }else{
    
    return(NULL)
    
  }
}

base_wd <- 'C:\\Users\\Edwin\\Desktop\\Edwin(10-08-2012)\\Assignments and Modules\\Coursera\\Data Science - John Hopkins\\Capstone\\'
load(file = paste(base_wd, 'tokens_df2.RData', sep =""))

predictBigram('of')



result <- predictTrigram(c('case', 'of'))
result <- predictTrigram(c('mean', 'the'))




