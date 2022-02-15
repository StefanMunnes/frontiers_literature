
# function to maximal preprocess the dictionarys
dict_max <- function(sent) {
  tolower(sent) %>%
    wordStem(language = "de") %>%
    stri_trans_general("de-ASCII") %>%
    unique()
}

# list of words for negations in the corpus for dictionarys approaches
not_words <- c("nicht", "nichts", "kein", "keine", "keinen", "ohne")

# function to create tokens with negations in reviews
not_tokens <- function(data) {
  tokens_replace(data,
                 pattern = not_words,
                 replacement = rep("not", length(not_words))) %>%
    tokens_compound(pattern = phrase(c("not *")))
}

# function to run dictionary approaches
dict_run <- function(data, dict, case_in = FALSE) {

  tokens <- tokens_lookup(data, dictionary = dict, case_insensitive = case_in)

  sent_df <- convert(dfm(tokens), to = "data.frame") %>%
    cbind(docvars(tokens)) %>%
    rename_with(~ substr(.x, 1, 3), starts_with(c("pos", "neg"))) %>%
    mutate(sentiment = (pos - neg) / (pos + neg),
           fre = pos + neg,
           diff = sent_hc_z - sentiment)

  return(sent_df)
}

# function to test reliability of methods
test_sent <- function(data, num = TRUE, freq = TRUE) {

  sent_df <- mutate(data,
                    sent_z = scale(sentiment),
                    diff1 = abs(sent_hc_z - sent_z) > 1,
                    diff2 = abs(sent_hc_z - sent_z) > 2)

  if (num == TRUE) {
    num <- nrow(sent_df[sent_df$fre > 0,])
  }
  if (num == FALSE) {
    num <- nrow(filter(sent_df, !is.na(sentimentsent_df)))
  }

  cor <- as.numeric(cor(sent_df$sent_hc_z, sent_df$sent_z, use = "complete.obs"))
  diff1 <- nrow(filter(sent_df, diff1 == TRUE))
  diff2 <- nrow(filter(sent_df, diff2 == TRUE))

  test <- list(num = num, cor = cor, diff = list("> 1 sd" = diff1,
                                                 "> 2 sd" = diff2))

  if (freq == TRUE) {
    fre  <- mean(sent_df$fre, na.rm = TRUE)
    test <- append(test, list(fre = fre))
  }

  return(test)
}



# prepare empty list to store results from different methods
test <- list()
saveRDS(test, "../data/test_results.RDS")


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
