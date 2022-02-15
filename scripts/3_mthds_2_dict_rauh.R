
#---Rauh Dictionary with maximal and minimal preprocessing

#---1. load coded data, load minimal and maximal preprocessed corpus and convert to df
load("../data/frontiers_reviews_coded_prep.Rdata")
revs_coded_prep <- list(min = revs_coded_min, max = revs_coded_max)

#load and prepare Rauh dictionary from quanteda
dict_rauh_ls <- as.list(data_dictionary_Rauh)


dict_rauh_nonot <- list(pos = unlist(dict_rauh_ls[4]),
                        neg = unlist(dict_rauh_ls[3])) %>%
  dictionary()


dict_rauh_not <- list(pos = c(dict_rauh_ls$neg_negative, dict_rauh_ls$positive),
                      neg = c(dict_rauh_ls$neg_positive, dict_rauh_ls$negative)) %>%
  lapply(function(x) str_replace_all(x, " ", "_")) %>%
  dictionary()


# preprocessing
# minimum pre-processing
dict_rauh_not_min <- dict_rauh_not


# maximum pre-processing
dict_rauh_not_max <- lapply(as.list(dict_rauh_not_min), dict_max) %>%
  dictionary()

dict_rauh_not_ls <- list(min = dict_rauh_not_min, max = dict_rauh_not_max)


# ---- 1.2 create tokens with negations ----
revs_coded_not <- lapply(revs_coded_prep, not_tokens)


# ---- 1.3 use dictionary and calculate reliability meausures ----
min_max <- c("min", "max")

test_rauh_not <- lapply(min_max, function(prep) {
  dict_run(revs_coded_not[[prep]], dict_rauh_not_ls[[prep]]) %>%
   test_sent()
})

names(test_rauh_not) <- min_max



# preprocessing without negations
# minimum pre-processing
dict_rauh_nonot_min <- dict_rauh_nonot


# maximum pre-processing
dict_rauh_nonot_max <- lapply(as.list(dict_rauh_nonot_min), dict_max) %>%
  dictionary()

dict_rauh_nonot_ls <- list(min = dict_rauh_nonot_min, max = dict_rauh_nonot_max)


test_rauh_nonot <- lapply(min_max, function(prep) {
  dict_run(revs_coded_prep[[prep]], dict_rauh_nonot_ls[[prep]]) %>%
    test_sent()
})

names(test_rauh_nonot) <- min_max


# ---- store test results ----
test <- readRDS("../data/test_results.RDS")

test$rauh <- test_rauh_not
test$rauh_nonot <- test_rauh_nonot

saveRDS(test, "../data/test_results.RDS")

