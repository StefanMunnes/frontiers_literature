
# load coded reviews to use dictionarys on it
load("../data/frontiers_reviews_coded_prep.Rdata")

revs_coded_prep <- list(min = revs_coded_min, max = revs_coded_max)

glove_dict_ls <- readRDS("../data/glove_dict_ls.RDS")



# ---- 1. dictionary from self-trained GloVe model  ----

min_max <- c("min", "max")

test_glove <- lapply(glove_dict_ls[1:2], function(seed) {

  lapply(min_max, function (min_max) {

    lapply(seed[[min_max]], function(ngram) {

      dict_run(revs_coded_prep[[min_max]], as.dictionary(ngram)) %>%
        test_sent()
    })
  })
})

names(test_glove[[1]]) <- min_max
names(test_glove[[2]]) <- min_max



# ---- 2. dictionary from pre-trained GloVe model ----

test_glove_pre <- lapply(glove_dict_ls$pre, function(seed) {

  dict_run(revs_coded_prep$min, as.dictionary(seed), case_in = TRUE) %>%
    test_sent()
})



# ---- 3. save test results ----

test <- readRDS("../data/test_results.RDS")

test$glove <- test_glove
test$glove_pre  <- test_glove_pre

saveRDS(test, "../data/test_results.RDS")
