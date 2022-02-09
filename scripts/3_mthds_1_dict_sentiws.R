
load("../data/frontiers_reviews_coded_prep.Rdata")

revs_coded_prep <- list(min = revs_coded_min, max = revs_coded_max)


# ---- 1.1 prepare sentiWS dictionary from quanteda ----

# minimum pre-processing
dict_sws_min <- data_dictionary_sentiws

# maximum pre-processing
dict_sws_max <- lapply(as.list(dict_sws_min), dict_max) %>%
  dictionary()

dict_sws <- list(min = dict_sws_min, max = dict_sws_max)


# ---- 1.2 use dictionary and calculate reliability meausures ----
min_max <- c("min", "max")

test_sws <- lapply(min_max, function(prep) {
  dict_run(revs_coded_prep[[prep]], dict_sws[[prep]]) %>% 
    test_sent()
})

names(test_sws) <- min_max


# ---- 2.1 add negations ----

# new list of positive & negative dictionary with negations
dict_sws_not <- lapply(dict_sws, function(min_max) {

  list(negative = c(min_max$negative,
                    sapply(min_max$positive, function(x) paste0("not_", x))),
       positive = c(min_max$positive,
                    sapply(min_max$negative, function(x) paste0("not_", x)))) %>%
  dictionary()
})

# create tokens with negations
revs_coded_not <- lapply(revs_coded_prep, function(data) {

  tokens_replace(data,
                 pattern = not_words,
                 replacement = rep("not", length(not_words))) %>%
    tokens_compound(pattern = phrase(c("not *")))
})


# ---- 2.2 use dictionary and calculate reliability meausures ----
test_sws_not <- lapply(min_max, function(prep) {

  dict_run(revs_coded_not[[prep]], dict_sws_not[[prep]]) %>% 
    test_sent()
})

names(test_sws_not) <- min_max


# ---- 3. store test results ----
test <- readRDS("../data/test_results.RDS")

test$sws <- test_sws
test$sws_not  <- test_sws_not

saveRDS(test, "../data/test_results.RDS")
