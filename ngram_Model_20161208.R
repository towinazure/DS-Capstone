rm(list=ls())
library(dplyr)
library(tidyr)

base_wd <- 'C:\\Users\\Edwin\\Desktop\\Edwin(10-08-2012)\\Assignments and Modules\\Coursera\\Data Science - John Hopkins\\Capstone\\'
# load(file = paste(base_wd, 'tokens_df.RData', sep =""))

cleaningQuote <- function(my_df, col.names){
  
  # Replace the "'" characters except the contractions
  # contractions: "n't" , "'s", "i'm", "'ve", "'d", "'ll", "o'"
  
  for(i in 1:length(col.names)){
    
    dummy_str <- as.vector(my_df[[col.names[i]]])
    dummy_str <- gsub("(^')|('$)", "", x = dummy_str) 
    
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

# test_str <- c("we'll", "we've", "it's", "'i've", "it's'", "o'clock", "ep[rtl[eprtl", "i'd")
# 
# grepl("^(?!it)$", test_str, perl = T)
# grepl("((we)|(i)|(you)|(they))'ve", test_str)
# gsub("(?!(((we)|(i)|(you)|(they))'ve))", "", test_str, perl = T)
# gsub("(^')|('$)", "", x = test_str) 

##Construct Data Frame for Training
train_trigrams_df        <- read.table(paste(base_wd, 'tri_train_py.txt', sep =""),
  sep = "\t", header = F,  quote = "\"",  stringsAsFactors = FALSE)
names(train_trigrams_df) <- c('Token1', 'Token2', 'Token3', 'cnt')

train_trigrams_df <- cleaningQuote(train_trigrams_df, col.names = c('Token1', 'Token2', 'Token3'))
train_trigrams_df <- train_trigrams_df %>% group_by(Token1, Token2, Token3) %>% summarise(cnt = sum(cnt)) %>% ungroup()
train_trigrams_df <- train_trigrams_df %>% filter(cnt > 2)
train_trigrams_df <- train_trigrams_df %>% arrange(desc(cnt))

train_bigrams_df        <- read.table(paste(base_wd, 'bi_train_py.txt', sep =""),
  sep = "\t", header = F,  quote = "\"",  stringsAsFactors = FALSE)
names(train_bigrams_df) <- c('Token1', 'Token2', 'cnt')

train_bigrams_df <- cleaningQuote(train_bigrams_df, col.names = c('Token1', 'Token2'))
train_bigrams_df <- train_bigrams_df %>% group_by(Token1, Token2) %>% summarise(cnt = sum(cnt)) %>% ungroup()
train_bigrams_df <- train_bigrams_df %>% filter(cnt > 2)
train_bigrams_df <- train_bigrams_df %>% arrange(desc(cnt))

train_unigrams_df        <- read.table(paste(base_wd, 'uni_train_py.txt', sep =""),
  sep = "\t", header = F,  quote = "\"",  stringsAsFactors = FALSE)
names(train_unigrams_df) <- c('Token', 'cnt')

train_unigrams_df <- cleaningQuote(train_unigrams_df, col.names = c('Token1'))
train_unigrams_df <- train_unigrams_df %>% group_by(Token) %>% summarise(cnt = sum(cnt)) %>% ungroup()
train_unigrams_df <- train_unigrams_df %>% filter(cnt > 2)
train_unigrams_df <- train_unigrams_df %>% arrange(desc(cnt))

##Construct Data Frame for Validation
valid_trigrams_df        <- read.table(paste(base_wd, 'tri_valid_py.txt', sep =""),
  sep = "\t", header = F,  quote = "\"",  stringsAsFactors = FALSE)
names(valid_trigrams_df) <- c('Token1', 'Token2', 'Token3', 'cnt')

valid_trigrams_df <- cleaningQuote(valid_trigrams_df, col.names = c('Token1', 'Token2', 'Token3'))
valid_trigrams_df <- valid_trigrams_df %>% group_by(Token1, Token2, Token3) %>% summarise(cnt = sum(cnt)) %>% ungroup()
valid_trigrams_df <- valid_trigrams_df %>% filter(cnt > 2)
valid_trigrams_df <- valid_trigrams_df %>% arrange(desc(cnt))

##Construct Data Frame for Testing
test_trigrams_df        <- read.table(paste(base_wd, 'tri_test_py.txt', sep =""),
  sep = "\t", header = F,  quote = "\"",  stringsAsFactors = FALSE)
names(test_trigrams_df) <- c('Token1', 'Token2', 'Token3', 'cnt')

test_trigrams_df <- cleaningQuote(test_trigrams_df, col.names = c('Token1', 'Token2', 'Token3'))
test_trigrams_df <- test_trigrams_df %>% group_by(Token1, Token2, Token3) %>% summarise(cnt = sum(cnt)) %>% ungroup()
test_trigrams_df <- test_trigrams_df %>% filter(cnt > 2)
test_trigrams_df <- test_trigrams_df %>% arrange(desc(cnt))

#Save as .RData
save(train_trigrams_df, 
  train_bigrams_df,
  train_unigrams_df,
  file = paste(base_wd, 'train_df.RData', sep =""))
save(valid_trigrams_df, file = paste(base_wd, 'valid_df.RData', sep =""))
save(test_trigrams_df,  file = paste(base_wd, 'test_df.RData', sep =""))

load(paste(base_wd, file = 'train_df.RData', sep =""))
load(paste(base_wd, file = 'valid_df.RData', sep =""))
load(paste(base_wd, file = 'test_df.RData', sep =""))

cleaningMeaningLess <- function(my_df, col.names){
  
  for(i in 1:length(col.names)){
    
    dummy_str <- as.vector(my_df[[col.names[i]]])
    dummy_str <- gsub("(^')|('$)", "", x = dummy_str) 
    
    bad_word_flag <- grepl("^[b-hj-z]$", dummy_str)
    
    my_df <- my_df[!bad_word_flag, ] 
    
  }
  
  return(my_df)
}

train_trigrams_df  <- cleaningMeaningLess(train_trigrams_df, col.names = c('Token1', 'Token2', 'Token3'))
train_bigrams_df   <- cleaningMeaningLess(train_bigrams_df, col.names = c('Token1', 'Token2'))
train_unigrams_df  <- cleaningMeaningLess(train_unigrams_df, col.names = c('Token'))
valid_trigrams_df  <- cleaningMeaningLess(valid_trigrams_df, col.names = c('Token1', 'Token2', 'Token3'))
test_trigrams_df   <- cleaningMeaningLess(test_trigrams_df, col.names = c('Token1', 'Token2', 'Token3'))

save(train_trigrams_df, 
  train_bigrams_df,
  train_unigrams_df,
  file = paste(base_wd, 'train_df.RData', sep =""))
save(valid_trigrams_df, file = paste(base_wd, 'valid_df.RData', sep =""))
save(test_trigrams_df,  file = paste(base_wd, 'test_df.RData', sep =""))

load(paste(base_wd, file = 'train_df.RData', sep =""))
load(paste(base_wd, file = 'valid_df.RData', sep =""))
load(paste(base_wd, file = 'test_df.RData', sep =""))


# predictBigram <- function(word){
#   
#   model_df <- rbind(news_bigrams_df, twitter_bigrams_df, blogs_bigrams_df)  
#   word = tolower(word)
# 
#   word_df <- model_df %>% filter(Token1 == word)
#   if(nrow(word_df) > 1){
#     
#     word_p_df <- data.frame(p = word_df$cnt/sum(word_df$cnt), 
#     Input = word, Word_predicted = word_df$Token2)
#     
#   return(word_p_df)
#   }else{
#     
#     return(NULL)
#     
#   }
# }
# 
# predictTrigram <- function(words, model_df, discountSW = 0){
#   
#   words = tolower(words)
#   
#   word_df <- model_df %>% filter(Token1 == words[1] & Token2 == words[2])
#   if(nrow(word_df) > 1){
#     
#     word_df <- word_df %>% group_by(Token1, Token2, Token3) %>% summarise(cnt = sum(cnt)) %>% ungroup()
# 
#     word_p_df <- data.frame(p = word_df$cnt/sum(word_df$cnt), 
#       Input = paste(words, collapse = ' '), Word_predicted = word_df$Token3)
#     word_p_df <- word_p_df %>% arrange(desc(p))
#     return(word_p_df)
#     
#   }else{
#     
#     return(NULL)
#     
#   }
# }

#Let's Validate the model
head(valid_trigrams_df)
validation_result <- valid_trigrams_df %>% 
  filter(!Token1 %in% c('s1', 's2') & !Token2 %in% c('s1', 's2', 'e1' ,'e2') & !Token3 %in% c('e1', 'e2')) %>%
  rename(
    Input_tk1 = Token1,
    Input_tk2 = Token2,
    Input_tk3 = Token3, 
    Input_cnt = cnt) 

# validation_result$prob <- NA
head(validation_result)

freqThreshold_trigram <- 5
freqThreshold_bigram  <- 50
freqThreshold_unigram <- 1000

train_trigrams_df2 <- train_trigrams_df %>% 
  filter(!Token1 %in% c('s1', 's2') & !Token2 %in% c('s1', 's2', 'e1' ,'e2') & !Token3 %in% c('e1', 'e2')) %>%
  filter(cnt >= freqThreshold_trigram) %>%
  rename(Tri_tk1 = Token1,
    Tri_tk2 = Token2,
    Tri_tk3 = Token3)

train_bigrams_df2 <- train_bigrams_df %>% 
  filter(!Token1 %in% c('s1', 's2') & !Token2 %in% c('e1', 'e2')) %>%
  filter(cnt >= freqThreshold_bigram) %>%
  rename(Bi_tk1 = Token1,
    Bi_tk2 = Token2)

train_unigrams_df2 <- train_unigrams_df %>% 
  filter(!Token %in% c('<s>', '<e>')) %>%
  filter(cnt >= freqThreshold_unigram) %>%
  rename(Uni_tk1 = Token)


rm(train_trigrams_df, train_bigrams_df, train_unigrams_df)

# validation_result_test <- validation_result[1:10, ]
###############################################################################################################
#
# Ranking Approach
#
###############################################################################################################

# trigram_result <- validation_result %>% 
#   left_join(train_trigrams_df2, by = c('Input_tk1' = 'Tri_tk1', 'Input_tk2' = 'Tri_tk2')) %>%
#   group_by(Input_tk1, Input_tk2, Input_tk3) %>% mutate(cnt_rk = rank(desc(cnt))) %>% filter(cnt_rk <= 5) %>% ungroup() %>%
#   mutate(pred_correct = ifelse(Input_tk3 == Tri_Tk3, 1, 0)) %>% group_by(Input_tk1, Input_tk2, Input_tk3) %>%
#   summarise(pred_correct = sum(pred_correct)) %>% ungroup()
# 
# trigram_result <- trigram_result %>% inner_join(validation_result, 
#   by = c('Input_tk1', 'Input_tk2', 'Input_tk3'))
# 
# trigram_result <- trigram_result %>% arrange(desc(Input_cnt))
# 
# validation_unseen <- validation_result2 %>% filter(is.na(validation_result2$pred_correct))
# 
# ## Prediction Accuracy on Trigram
# sum(validation_result2$pred_correct * validation_result2$Input_cnt, na.rm = T)/sum(validation_result2$Input_cnt, na.rm = T)
# 
# 
# #Use Bigram for unseen
# validation_unseen_bigram <- validation_unseen %>% left_join(train_bigrams_df2, by = c('Input_tk2' = 'Bi_tk1')) %>%
#   group_by(Input_tk1, Input_tk2, Input_tk3) %>% mutate(cnt_rk = rank(desc(cnt))) %>% filter(cnt_rk <= 5) %>% ungroup() %>%
#   mutate(pred_correct = ifelse(Input_tk3 == Bi_tk2, 1, 0)) %>% group_by(Input_tk1, Input_tk2, Input_tk3) %>%
#   summarise(pred_correct = sum(pred_correct)) %>% ungroup()
# 
# validation_unseen_bigram <- validation_unseen_bigram %>% inner_join(validation_result, 
#   by = c('Input_tk1', 'Input_tk2', 'Input_tk3'))
# 
# validation_unseen_bigram$pred_correct[is.na(validation_unseen_bigram$pred_correct)] <- 0
# sum(validation_unseen_bigram$pred_correct * validation_unseen_bigram$Input_cnt, na.rm = T)/
#   sum(validation_unseen_bigram$Input_cnt, na.rm = T)

###############################################################################################################
#
# Probability Approach
#
###############################################################################################################

# validation_result_test <- validation_result[1:10, ]

#Trigram 
trigram_result <- validation_result %>% 
  left_join(train_trigrams_df2, by = c('Input_tk1' = 'Tri_tk1', 'Input_tk2' = 'Tri_tk2')) %>%
  mutate(pred_cnt = ifelse(Input_tk3 == Tri_Tk3, cnt, 0)) %>% group_by(Input_tk1, Input_tk2, Input_tk3) %>%
  summarise(pred_prob_tri = sum(pred_cnt)/sum(cnt)) %>% ungroup()

trigram_result <- trigram_result %>% inner_join(validation_result, 
  by = c('Input_tk1', 'Input_tk2', 'Input_tk3'))

trigram_result <- trigram_result %>% arrange(desc(Input_cnt))

#Bigram 
validation_result_bi <- validation_result %>% group_by(Input_tk2, Input_tk3) %>%
  summarise(Input_cnt = sum(Input_cnt)) %>% ungroup()

# x <- validation_result_bi %>% filter(Input_tk2 == 'a')

# validation_result_test <- validation_result_bi[1:10, ]

bigram_result <- validation_result_bi %>%  
  inner_join(train_bigrams_df2, by = c('Input_tk2' = 'Bi_tk1')) %>%
  mutate(pred_cnt = ifelse(Input_tk3 == Bi_tk2, cnt, 0)) %>% group_by(Input_tk2, Input_tk3) %>%
  summarise(pred_prob_bi = sum(pred_cnt)/sum(cnt)) %>% ungroup()

unigram_result <- train_unigrams_df2 %>% mutate(pred_prob_uni = cnt/sum(cnt)) %>% dplyr::select(-cnt)

trigram_result$tri_no_result <- 0
trigram_result$tri_no_result[is.na(trigram_result$pred_prob_tri)] <- 1
trigram_result$pred_prob_tri[is.na(trigram_result$pred_prob_tri)] <- 0

bigram_result$bi_no_result <- 0
bigram_result$bi_no_result[is.na(bigram_result$pred_prob_bi)] <- 1
bigram_result$pred_prob_bi[is.na(bigram_result$pred_prob_bi)] <- 0


analysis_result <- trigram_result %>% left_join(bigram_result, by = c('Input_tk2', 'Input_tk3')) %>%
  left_join(unigram_result, by = c('Input_tk3' = 'Uni_tk1'))

analysis_result$pred_prob_uni[is.na(analysis_result$pred_prob_uni)] <- 0
analysis_result$pred_prob_bi[is.na(analysis_result$pred_prob_bi)] <- 0
analysis_result$bi_no_result[is.na(analysis_result$bi_no_result)] <- 1

save(analysis_result,  file = paste(base_wd, 'analysis_result.RData', sep =""))
load(file = paste(base_wd, 'analysis_result.RData', sep =""))


analysis_result$dummy <- 1
# library(glmnet)
# x <- model.matrix(dummy ~ 0 + pred_prob_tri + pred_prob_bi + pred_prob_uni +
#     I(pred_prob_bi * tri_no_result) + I(pred_prob_uni * tri_no_result) +
#     I(bi_no_result  * pred_prob_uni) + I(bi_no_result * tri_no_result * pred_prob_uni),
#   data = analysis_result)
# 
# y <- analysis_result$dummy

glm_fit <- glm(dummy ~ 0 + pred_prob_tri + 
    pred_prob_bi +
    # pred_prob_uni +
    I(pred_prob_bi * tri_no_result),
    # I(pred_prob_uni * tri_no_result) +
    # I(bi_no_result  * pred_prob_uni) + I(bi_no_result * tri_no_result * pred_prob_uni),
    data = analysis_result,
  weights = Input_cnt^0.3)
summary(glm_fit)

# cv_fit <- cv.glmnet(x = x, y = y, weights = analysis_result$Input_cnt, alpha = 0.5, intercept = FALSE)
# lambda_min <- glmnet_fit$lambda.min
# glmnet_fit <- glmnet(x = x, y = y, weights = analysis_result$Input_cnt, alpha = 0.5, intercept = FALSE,
#   lambda = lambda_min)
# 
# coef(glmnet_fit)

coef_fit <- coef(glm_fit)

save(glm_fit,  file = paste(base_wd, 'glm_fit.RData', sep =""))
save(coef_fit,  file = paste(base_wd, 'coef_fit.RData', sep =""))

#Average Predictability...
sum(glm_fit$fitted.values * analysis_result$Input_cnt)/sum(analysis_result$Input_cnt)
load(file = paste(base_wd, 'glm_fit.RData', sep =""))

##########################################################################
#
# Write a Function to produce predicted words and probability...
#
##########################################################################

##Use Test Set...

freqThreshold_trigram <- 5
freqThreshold_bigram  <- 50
freqThreshold_unigram <- 1000

# train_trigrams_df2 <- train_trigrams_df %>% 
#   filter(!Token1 %in% c('s1', 's2') & !Token2 %in% c('s1', 's2', 'e1' ,'e2') & !Token3 %in% c('e1', 'e2')) %>%
#   filter(cnt >= freqThreshold_trigram) %>%
#   rename(Tri_tk1 = Token1,
#     Tri_tk2 = Token2,
#     Tri_Tk3 = Token3)
# 
# train_bigrams_df2 <- train_bigrams_df %>% 
#   filter(!Token1 %in% c('s1', 's2') & !Token2 %in% c('e1', 'e2')) %>%
#   filter(cnt >= freqThreshold_bigram) %>%
#   rename(Bi_tk1 = Token1,
#     Bi_tk2 = Token2)
# 
# train_unigrams_df2 <- train_unigrams_df %>% 
#   filter(!Token %in% c('<s>', '<e>')) %>%
#   filter(cnt >= freqThreshold_unigram) %>%
#   rename(Uni_tk1 = Token)

#Trigram 
test_trigrams_df <- test_trigrams_df %>% 
  filter(!Token1 %in% c('s1', 's2') & !Token2 %in% c('s1', 's2', 'e1' ,'e2') & !Token3 %in% c('e1', 'e2')) %>%
  rename(
    Input_tk1 = Token1,
    Input_tk2 = Token2,
    Input_tk3 = Token3, 
    Input_cnt = cnt) 


# load(paste(base_wd, file = 'train_df.RData', sep =""))
# load(paste(base_wd, file = 'valid_df.RData', sep =""))
# load(paste(base_wd, file = 'test_df.RData', sep =""))


save(tri_df, bi_df,
  file = paste(base_wd, 'simple_train_df.RData', sep =""))

predictNextWord(test_trigrams_df$Input_tk1[15], test_trigrams_df$Input_tk2[15], 
  tri_df, bi_df, coef_fit = glm_fit)

predictNextWord <- function(
  Input_tk1,
  Input_tk2,
  tri_df, bi_df, 
  freqThreshold_trigram = 5, freqThreshold_bigram = 50, coef_fit){
  
  # Input_tk1 <- test_trigrams_df$Input_tk1[1]
  # Input_tk2 <- test_trigrams_df$Input_tk2[1]
  # tri_df <- train_trigrams_df
  # bi_df <- train_bigrams_df
  # coef_fit <- glm_fit
  
  # tri_df <- tri_df %>% 
  # filter(!Token1 %in% c('s1', 's2') & !Token2 %in% c('s1', 's2', 'e1' ,'e2') & !Token3 %in% c('e1', 'e2')) %>%
  # filter(cnt >= freqThreshold_trigram) %>%
  # rename(Tri_tk1 = Token1,
  #   Tri_tk2 = Token2,
  #   Tri_tk3 = Token3)
  # 
  # bi_df <- bi_df %>% 
  # filter(!Token1 %in% c('s1', 's2') & !Token2 %in% c('e1', 'e2')) %>%
  # filter(cnt >= freqThreshold_bigram) %>%
  # rename(Bi_tk1 = Token1,
  #   Bi_tk2 = Token2)
  
  coefs <- coef(coef_fit)

  trigram_result <- tri_df %>% filter(Tri_tk1 == Input_tk1 & Tri_tk2 == Input_tk2)
  bigram_result  <- bi_df %>% filter(Bi_tk1 == Input_tk2)
  
  if(nrow(trigram_result) > 0){
    
    trigram_result <- trigram_result %>% mutate(tri_p = cnt/sum(cnt)) %>% dplyr::select(-cnt)
    if(nrow(bigram_result)){
      bigram_result   <- bigram_result %>% mutate(bi_p = cnt/sum(cnt)) %>% dplyr::select(-cnt)
      combined_result <- trigram_result %>% full_join(bigram_result, by = c('Tri_tk2' = 'Bi_tk1', 'Tri_tk3' = 'Bi_tk2'))
      combined_result$tri_p[is.na(combined_result$tri_p)] <- 0
      combined_result$bi_p[is.na(combined_result$bi_p)] <- 0
      combined_result$p <- combined_result$tri_p * coefs[1] + combined_result$bi_p * coefs[2]
      
      output <- combined_result %>% mutate(rk = rank(desc(p))) %>% filter(rk <= 5) 
      output <- output %>% arrange(rk)
      
      return(output)
    }else{
      
      trigram_result$p <- trigram_result$tri_p
      trigram_result   <- trigram_result %>% arrange(desc(p))
      
      output <- trigram_result %>% mutate(rk = rank(desc(p))) %>% filter(rk <= 5) 
      output <- output %>% arrange(rk)
      
      return(output)
      
    }
  }else{
    if(nrow(bigram_result) > 0){
      
      bigram_result    <- bigram_result %>% mutate(bi_p = cnt/sum(cnt)) %>% dplyr::select(-cnt)
      bigram_result$p  <- bigram_result$bi_p
      
      output <- bigram_result %>% mutate(rk = rank(desc(p))) %>% filter(rk <= 5) 
      output <- output %>% arrange(rk)
      
      return(output)
      
    }else{
      return(NULL)
    }
  }
}
  
  


