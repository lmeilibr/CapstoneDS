library(tm)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)
library(scales)
library(magrittr)
library(stringr)
library(dplyr)
library(reshape2)
library(RWeka)
library(readr)
library(tidyr)
library(data.table)

predict = function(text, unigram, bigram, trigram,quadgram, pentagram){
  text <- VCorpus(VectorSource(text))
  text <- text %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(removeNumbers) %>%
    tm_map(removePunctuation) %>%
    tm_map(stripWhitespace)
  text <- inspect(text[[1]]) #transform to string again
  data <- unlist(str_split(text, " "))
  wordCount <- length(data)
  result = NULL
  score <- NULL
  score5 <- NULL
  score4 <- NULL
  score3 <- NULL
  score2 <- NULL
  score1 <- NULL
  # if string number of words greater than 2
  if(wordCount >= 2){
    tail <- tail(data, n = 4)
    
    last1 <- tail[max(length(tail))]
    last2 <- tail[max(length(tail)-1)]
    last3 <- tail[max(length(tail)-1)]
    last4 <- tail[max(length(tail)-1)]
    if(wordCount >= 3){
      last3 <- tail[max(length(tail)-2)]
    }
    if(wordCount >= 4){
      last4 <- tail[max(length(tail)-3)]
    }
    
    #test to see if its a 5-gram
    if(wordCount >= 4 & last4 %in% pentagram$word1 & last3 %in% pentagram$word2 & last2 %in% pentagram$word3 & last1 %in% pentagram$word4){
      count4gram <- quadgram %>%
        filter(word1 %in% last4 & word2 %in% last3 & word3 %in% last2 & NextWord %in% last1) %>%
        select(word1, word2, word3, NextWord, freq)
      
      pick5gram <- pentagram %>%
        filter(word1 %in% last4 & word2 %in% last3 & word3 %in% last2 & word4 %in% last1) %>%
        select(word1, word2, word3, word4, NextWord, freq)

      score5 <- pick5gram %>%
        mutate(score = freq / count4gram$freq) %>%
        group_by(NextWord) %>%
        summarise(score = max(score))
      score <- bind_rows(score, score5)
    }
    
    #test to see if its a 4-gram
    if(wordCount >= 3 & last3 %in% quadgram$word1 & last2 %in% quadgram$word2 & last1 %in% quadgram$word3){
      count3gram <- trigram %>%
        filter(word1 %in% last3 & word2 %in% last2 & NextWord %in% last1) %>%
        select(word1, word2, NextWord, freq)
      
      pick4gram <- quadgram %>%
        filter(word1 %in% last3 & word2 %in% last2 & word3 %in% last1) %>%
        select(word1, word2, word3, NextWord, freq)
      
      score4 <- pick4gram %>%
        mutate(score = freq / count3gram$freq) %>%
        group_by(NextWord) %>%
        summarise(score = max(score) * 0.4)
      score <- bind_rows(score, score4)
      
    }
    # test to see the 3-gram score
    if(last2 %in% trigram$word1 & last1 %in% trigram$word2){
      count2gram <- bigram %>%
        filter(word1 %in% last2 & NextWord %in% last1) %>%
        select(word1, NextWord, freq)
      
      pick3gram <- trigram %>%
        filter(word1 %in% last2 & word2 %in% last1) %>%
        select(word1, word2, NextWord, freq)
      
      score3 <- pick3gram %>%
        mutate(score = freq / count2gram$freq) %>%
        group_by(NextWord) %>%
        summarise(score = max(score) * 0.4 * 0.4)
      score <- bind_rows(score, score3)
    }
    # test to see the 2-gram score
    if(last1 %in% bigram$word1){
      count1gram <- unigram %>%
        filter(NextWord %in% last1) %>%
        select(NextWord, freq)
      
      pick2gram <- bigram %>%
        filter(word1 %in% last1) %>%
        select(word1, NextWord, freq)
      
      score2 <- pick2gram %>%
        mutate(score = freq / count1gram$freq) %>%
        group_by(NextWord) %>%
        summarise(score = max(score) * 0.4 * 0.4 * 0.4)
      score <- bind_rows(score, score2)
    }
  }
  if(!is.null(score)){
  score <- score %>%
    group_by(NextWord) %>%
    summarise(score = max(score)) %>%
    arrange(-score)
  return(score[1:5,])
  }
  return (as.data.frame("Sorry! Can't guess word"))
  }