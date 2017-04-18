sampleData <- function(){
  if(!file.exists("data/training.rds")){
    tw <- readLines("sample/twitter_sample.txt")
    bl <- readLines("sample/blogs_sample.txt")
    nw <- readLines("sample/news_sample.txt")
    text = c(tw, bl, nw)
    Encoding(text) <- "UTF-8"
    # docs <- text %>% strsplit(split = "(?!')[[:punct:]]", perl = TRUE) %>% unlist() %>%data_frame(text = .) %>% filter(!text == "")
    docs <- data_frame(text)
    set.seed(4869)
    intrain <- sample(nrow(docs), 0.6 * nrow(docs))
    training <- docs[intrain,]
    dir.create(file.path(".", "data"), showWarnings = FALSE)
    saveRDS(training, "data/training.rds")
    testing <- docs[-intrain, ]
    invalid <- sample(nrow(testing), 0.5 * nrow(testing))
    validating <- testing[invalid,]
    testing <- testing[-invalid,]
    saveRDS(validating, "data/validating.rds")
    saveRDS(testing, "data/testing.rds")
  } else{
    training <- readRDS("data/training.rds")
  }
}