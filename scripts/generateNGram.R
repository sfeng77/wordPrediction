library(dplyr)

ngram <- function(text, n){
  text %>% unnest_tokens(ngram, text, token = "ngrams", n = n) %>%
    filter(!grepl("[+-]?([0-9]*[.])?[0-9]+", ngram)) %>% 
    count(ngram) %>%
    extract(ngram, into = c("first", "last"), '(.*)\\s+([^ ]+)$') %>%
    anti_join(bad.words, by = c("last" = "word"))%>%
    ungroup() %>%
    arrange(desc(n))
}