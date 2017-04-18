library(dplyr)

calcProb <- function(f, l){
  three = tail(unlist(strsplit(f," ")),3)
  leftover <- 1
  quadtab <- quadgrams %>% filter(first == paste(three, collapse = " "))
  r <-  quadtab %>% filter(last == l)
  if (nrow(r) > 0) {
    d = r$discount[1]
    n = r$n[1]
    return(d * n / sum(quadtab$n))
  }
  
  if (nrow(quadtab) > 1)
    leftover <-  1 - sum(quadtab$discount * quadtab$n) / sum(quadtab$n)
  
  tritab <- trigrams %>% filter(first == paste(tail(three,2), collapse = " ")) %>% 
    anti_join(quadtab, by = "last")
  
  r <-  tritab %>% filter(last == l)
  if (nrow(r) > 0) {
    d = r$discount[1]
    n = r$n[1]
    return(leftover * d * n / sum(tritab$n))
  }
  
  if (nrow(tritab) > 1)
    leftover <-  leftover * ( 1 - sum(tritab$discount * tritab$n) / sum(tritab$n))
  
  bitab <- bigrams %>% 
    filter(first == tail(three, 1)) %>% 
    anti_join(quadtab, by = "last")%>% 
    anti_join(tritab, by = "last")
  
  r <-  bitab %>% filter(last == l)
  if (nrow(r) > 0) {
    d = r$discount[1]
    n = r$n[1]
    return(leftover * d * n / sum(bitab$n))
  }
  
  leftover <- leftover * ( 1 - sum(bitab$discount * bitab$n) / sum(bitab$n))
  
  return(leftover * filter(unigram, word == l)$p)
}