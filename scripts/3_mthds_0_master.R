
# function to maximal preprocess the dictionarys
dict_max <- function(sent) {
  tolower(sent) %>%
    wordStem(language = "de") %>%
    stri_trans_general("de-ASCII") %>%
    unique()
}

# function to run dictionary approaches
dict_run <- function(data, dict, case_in = FALSE) {

  tokens <- tokens_lookup(data, dictionary = dict, case_insensitive = case_in)

  sent_df <- convert(dfm(tokens), to = "data.frame") %>%
    cbind(docvars(tokens)) %>%
    rename_with(~ substr(.x, 1, 3), starts_with(c("pos", "neg"))) %>%
    mutate(sentiment = (pos - neg) / (pos + neg),
           fre = pos + neg)

  return(sent_df)
}

# function to test reliability of methods
test_sent <- function(data, freq = TRUE) {

  sent_df <- mutate(data, sent_z = scale(sentiment))

  num <- nrow(sent_df[sent_df$fre > 0,])
  cor <- cor(sent_df$sent_hc_z, sent_df$sent_z, use = "complete.obs")
  # icc <- icc(select(sent_df, sent_hc_z, sent_z))

  test <- list(num = num, cor = cor)

  if (freq == TRUE) {
    fre  <- mean(sent_df$fre, na.rm = TRUE)
    test <- append(test, list(fre = fre))
  }

  return(test)
}



# prepare empty list to store results from different methods
test <- list()
saveRDS(test, "../data/test_results.RDS")


# list of words for negations in the corpus for dictionarys approaches
not_words <- c("nicht", "nichts", "kein", "keine", "keinen", "ohne")


# 1. Dictionary: sentiWS
source("3_mthds_1_dict_sentiws.R")

# 2. Dictionary: Rauh
source("3_mthds_2_dict_rauh.R")

# 3. Dictionary: gerVADER
source("3_mthds_3_dict_gervader.R")

# 4. Word embeddings GloVe: train own model
source("3_mthds_4_glove_train.R")

# 5. Word embeddings GloVe: create list of seeds
source("3_mthds_5_glove_seeds.R")

# 6. Word embeddings GloVe: create dictionarys
source("3_mthds_6_glove_dict.R")

# 7. Word embeddings GloVe: get sentiments from dictionarys
source("3_mthds_7_glove_sent.R")

# 8. Wordscores
source("3_mthds_8_wordscores.R")

# 9. Wordfish
source("3_mthds_9_wordfish.R")
