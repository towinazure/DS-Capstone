rm(list=ls())

library(caret)

setwd('C:\\Users\\Edwin\\Desktop\\Edwin(10-08-2012)\\Assignments and Modules\\Coursera\\Data Science - John Hopkins\\Capstone\\Coursera-SwiftKey\\final\\en_US')

con <- file("en_US.twitter.txt", "r") 
x <- readLines(con)
close(con)

set.seed(12345)
valid_index <- sample(1:length(x), size = 
    round(length(x)/100, 0))

train_set <- x[-valid_index]  
valid_set <- x[valid_index]

write(x = train_set, file = 'en_US.twitter_train.txt')
write(x = valid_set, file = 'en_US.twitter_valid.txt')



