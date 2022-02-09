
#---Rauh Dictionary with maximal and minimal preprocessing
#---1. load coded data, load minimal and maximal preprocessed corpus and convert to df
#reviews_coded <-readRDS("../data/frontiers_reviews_coded.RDS")
load("../data/frontiers_reviews_coded_prep.Rdata")

#load and prepare Rauh dictonary from quanteda
dict_rauh_ls <- as.list(data_dictionary_Rauh)

dict_rauh_nonot <- dictionary(dict_rauh_ls[3:4])

dict_rauh_not <- list(positive = unlist(dict_rauh_ls[1], dict_rauh_ls[4]),
                      negative = unlist(dict_rauh_ls[2], dict_rauh_ls[3])) %>% 
  dictionary()



# ---- prepare Rauh Dictionary form replication data  ----
load("../data/Rauh_SentDictionaryGerman_Negation.Rdata")
load("../data/Rauh_SentDictionaryGerman.Rdata")

neg.sent.dictionary <- neg.sent.dictionary %>%
  mutate(word=feature)%>%
  mutate(sentiment = ifelse(sentiment == 1, "pos", "neg"))

sent.dictionary <- sent.dictionary %>%
  mutate(word = feature) %>%
  mutate(sentiment = ifelse(sentiment == -1, "neg", "pos"))

dictionary_rauh <- bind_rows(sent.dictionary, neg.sent.dictionary)
dictionary_Rauh <- quanteda::as.dictionary(dictionary_rauh)

dict_Rauh_min <-dictionary_Rauh

dict_Rauh_max <- lapply(as.list(dictionary_Rauh), function(sent){
  tolower(sent) %>%
    stri_trans_general("de-ASCII") %>%
    wordStem(language = "de") %>%
    unique()
}) %>%
  dictionary()

dict_Rauh <- list(min = dict_Rauh_min, max = dict_Rauh_max)

# replace negations with "not" (list as suggested by Rauh)
revs_coded_prep <- lapply(list(min = revs_coded_min, max = revs_coded_max), function(data) {
  tokens_replace(data,
                 pattern = c("nicht", "nichts", "kein", "keine", "keinen"),
                 replacement = rep("not", 5)) %>%
    tokens_compound(pattern = phrase(c("not *")))
})



# ---- use dictionary and calculate reliability meausures ----
min_max <- c("min", "max")


test_Rauh <- lapply(min_max, function(prep) {
  test_dict(revs_coded_prep[[prep]], dict_Rauh[[prep]], case_in = TRUE)
})

names(test_Rauh) <- min_max




#-----only use non-negated terms of the Rauh dictionary----
# ---- prepare Dictionary form Rauh form replication data  ----
noneg_dict_rauh_min <- quanteda::as.dictionary(sent.dictionary)

noneg_dict_rauh_max <- quanteda::as.dictionary(sent.dictionary)
noneg_dict_rauh_max <- lapply(as.list(noneg_dict_rauh_max), function(sent){
    tolower(sent) %>%
      stri_trans_general("de-ASCII") %>%
      wordStem(language = "de") %>%
      unique()
  }) %>%
    dictionary()

noneg_dict_Rauh <- list(min = noneg_dict_rauh_min, max = noneg_dict_rauh_max)

# ---- use dictionary and calculate reliability meausures ----
min_max <- c("min", "max")


noneg_test_Rauh <- lapply(min_max, function(prep) {
  test_dict(revs_coded_prep[[prep]], noneg_dict_Rauh[[prep]], case_in = TRUE)
})

names(noneg_test_Rauh) <- min_max

# ---- store test results ----
test <- readRDS("../data/test_results.RDS")

test$Rauh <- test_Rauh
test$Rauh_noneg <- noneg_test_Rauh

saveRDS(test, "../data/test_results.RDS")

