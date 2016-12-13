#Method 1
partitionTextFile <- function(file.name, tvt.portion = c(0.7, 0.2, 0.1)){
  
  con <- file(file.name) 
  x <- readLines(con, encoding = "UTF-8", skipNul = TRUE)
  close(con)
  
  n <- length(x)
  train_n    <- floor(tvt.portion[1] * n)
  valid_n    <- floor(tvt.portion[2] * n)
  test_n     <- n - train_n - valid_n
  
  train_pos  <- sample(1:n, size = train_n)
  rest       <- (1:n)[-train_pos]
  valid_flag <- sample(1:length(rest), size = valid_n)
  valid_pos  <- rest[valid_flag]
  test_pos   <- rest[-valid_flag]
  
  train_set <- x[train_pos]  
  valid_set <- x[valid_pos]
  valid_set <- x[test_pos]
  
  train_file_name <- gsub('.txt', '_train.txt', file.name)
  valid_file_name <- gsub('.txt', '_valid.txt', file.name)
  test_file_name  <- gsub('.txt', '_test.txt', file.name)
  
  write(x = train_set, file = train_file_name)
  write(x = valid_set, file = valid_file_name)
  write(x = test_set,  file = test_file_name)
  
}

set.seed(7689)
setwd('C:\\Users\\Edwin\\Desktop\\Edwin(10-08-2012)\\Assignments and Modules\\Coursera\\Data Science - John Hopkins\\Capstone\\')
file.names <- c('en_US.news.txt', 'en_US.blogs.txt', 'en_US.twitter.txt')
for(i in 1:length(file.names)){
  partitionTextFile(file.name = file.names[i], c(0.7, 0.2, 0.1))
}

#Method 2
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

train_set <-  c(news_train, twitter_train, blogs_train)
valid_set <-  c(news_valid, twitter_valid, blogs_valid)
test_set  <-  c(news_test, twitter_test, blogs_test)

save(train_set, valid_set, test_set, file = 'train_valid_test.RData')
rm(list = ls())
load(file = 'train_valid_test.RData')

write(x = train_set, file = 'train_set.txt')
write(x = valid_set, file = 'valid_set.txt')
write(x = test_set,  file = 'test_set.txt')
