
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
           matches = pos + neg,
           diff = sent_hc_z - sentiment)

  return(sent_df)
}

# function to test reliability of methods
test_sent <- function(data, num = TRUE, freq = TRUE) {
  
  # 1. standardize sentiment scale & count more if more than two 2 SD differ
  sent_df <- mutate(data,
                    sent_z = scale(sentiment),
                    diff = abs(sent_hc_z - sent_z) > 2)

  # 2. get all four values, depending on input
  if (num == TRUE)  num <- nrow(sent_df[sent_df$fre > 0,])
  if (num == FALSE) num <- nrow(filter(sent_df, !is.na(sentiment)))

  cor <- cor(sent_df$sent_hc_z, sent_df$sent_z, use = "complete.obs") %>% 
    as.numeric() %>% 
    round(2)

  if (freq == TRUE)  matches <- round(mean(sent_df$matches, na.rm = TRUE), 2)
  if (freq == FALSE) matches <- NA
  
  diff <- nrow(filter(sent_df, diff == TRUE))
  
  # 3. create list and return as data.frame with column names
  test_ls <- list(num = num, cor = cor, matches = matches, diff = diff)

  return(as.data.frame(test_ls, col.names = c("N", "Cor", "Matches", "Diff")))
}


# prepare empty list to store results from different methods
test_results <- 
  data.frame(Source = character(), Preprocessing = character(),
             Negation = logical(), Similarity = numeric(),
             Negative = numeric(), Positive = numeric(),
             N = numeric(), Cor = numeric(), Matches = numeric(), Diff = numeric())

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





result.table <- data.frame(
  Source = c(rep("SentiWS", 4), rep("Rauh", 4), rep("GERVader", 2),
             rep("GloVe", 4), rep("GloVe", 4), rep("GloVe", 3), rep("GloVe", 3),
             rep("Wordscores", 2), rep("Wordfish", 2)),
  Preprocessing = c(rep("Minimal", 2), rep("Maximal", 2), 
                    rep("Minimal", 2), rep("Maximal", 2))
)


columns <- c("Source", "Preprocessing", "Negation", "Similarity", "Negative", 
             "Positive", "N", "Cor", "Matches", "Diff")



a <- data.frame(Source = "SentiWS", Preprocessing = "Minimal", Negation = "FALSE", 
                Similarity = NA, Negative = 15559, Positive = 15591, 
                N = test$sws$min$num, Cor = test$sws$min$cor, 
                Matches = test$sws$min$fre, diff = test$sws$min$diff[[2]])

list("SentiWS", "Minimal", FALSE, NA, 15559, 15591, test$sws$min$num, 
     test$sws$min$cor, test$sws$min$fre, test$sws$min$diff[[2]])

c <- as.data.frame(list(NA, 15529, 12391, test$sws$min$num))
