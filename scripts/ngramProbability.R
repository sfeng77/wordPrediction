library(dplyr)

calcDiscount <- function(ngramTable){
  
  freqTable <- ngramTable %>% group_by(n) %>% summarise(count = n()) %>% ungroup() %>% arrange(n)
  freqTable$discount <- 1
  for(i in 1:5){
    currN = freqTable$count[i]
    nextN = freqTable$count[i + 1]
    freqTable$discount[i] <- (i + 1)/i * (nextN / currN)
  }
  freqTable <- freqTable %>% select(n, discount)
  ngramTable %>% select(first, last, n) %>% left_join(freqTable) %>% ungroup()
}


calcProbNgram <- function(ngram){
  ngram <- calcDiscount(ngram)
  sums <- ngram %>% group_by(first) %>% summarise(sum = sum(n))
  ngram %>% left_join(sums) %>% mutate(p = discount * n / sum) %>% select(first, last, p)
}