rm(list=ls())
library(dplyr)
library(tidyr)
base_wd <- 'C:\\Users\\Edwin\\Desktop\\Edwin(10-08-2012)\\Assignments and Modules\\Coursera\\Data Science - John Hopkins\\Capstone\\'

predictNextWord <- function(
  Input_tk1,
  Input_tk2,
  tri_df, bi_df, 
  coefs,
  freqThreshold_trigram = 5, freqThreshold_bigram = 50, 
  n = 5){
  
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

      output <- combined_result %>% mutate(rk = rank(desc(p))) %>% filter(rk <= n) 
      output <- output %>% arrange(rk)
      
      return(output)
    }else{
      
      trigram_result$p <- trigram_result$tri_p
      trigram_result   <- trigram_result %>% arrange(desc(p))
      
      output <- trigram_result %>% mutate(rk = rank(desc(p))) %>% filter(rk <= n) 
      output <- output %>% arrange(rk)
      
      return(output)
      
    }
  }else{
    if(nrow(bigram_result) > 0){
      
      bigram_result    <- bigram_result %>% mutate(bi_p = cnt/sum(cnt)) %>% dplyr::select(-cnt)
      bigram_result$p  <- bigram_result$bi_p
      
      output <- bigram_result %>% mutate(rk = rank(desc(p))) %>% filter(rk <= n) 
      output <- output %>% arrange(rk)
      
      return(output)
      
    }else{
      return(NULL)
    }
  }
}

load(file = paste(base_wd, 'coef_fit.RData', sep =""))
load(file = paste(base_wd, 'simple_train_df.RData', sep =""))

predictNextWord("and", "i'd", tri_df, bi_df, coef_fit, n = 20)


