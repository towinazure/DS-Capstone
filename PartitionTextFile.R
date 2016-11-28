partitionTextFile <- function(file.name, validation.p = 0.01){
  
  con <- file(file.name, "r") 
  x <- readLines(con)
  close(con)
  
  valid.pos <- sample(1:length(x), size = 
      round(length(x) * validation.p, 0))
  
  train_set <- x[-valid.pos]  
  valid_set <- x[valid.pos]
  
  train_file_name <- gsub('.txt', '_train.txt', file.name)
  valid_file_name <- gsub('.txt', '_valid.txt', file.name)
  
  write(x = train_set, file = train_file_name)
  write(x = valid_set, file = valid_file_name)
}

set.seed(12345)
setwd('C:\\Users\\Edwin\\Desktop\\Edwin(10-08-2012)\\Assignments and Modules\\Coursera\\Data Science - John Hopkins\\Capstone\\')
file.names <- c('en_US.news.txt', 'en_US.blogs.txt', 'en_US.twitter.txt')
for(i in 1:length(file.names)){
  partitionTextFile(file.name = file.names[i], 0.01)
}


