# Script: Gervader TSV export & import
# Date: 21.01.22
# Author: Marcel


# ---- 1. Load data ----
load("../data/frontiers_reviews_coded_prep.Rdata")
revs_coded <- readRDS("../data/frontiers_reviews_coded.RDS") %>%
  mutate(doc_id = paste0("text", row_number()))

# Convert corpus to dataframe
minimal <- vapply(revs_coded_min, paste, FUN.VALUE = character(1), collapse = " ") %>%
  corpus() %>%
  convert("data.frame")

maximal <- vapply(revs_coded_max, paste, FUN.VALUE = character(1), collapse = " ") %>%
  corpus() %>%
  convert("data.frame")

remove(revs_coded_max, revs_coded_min)

revs_coded_prep <- list(minimal, maximal)


# ---- 2. Export GERVADER data ----

min_max <- c("min", "max")

revs_coded_prep <- lapply(revs_coded_prep, function(x) {

  mutate(x,
    text = str_remove_all(text, ";|\t|\r|\n") %>%
            trimws()) %>%
    relocate(text, .after = doc_id)
})

names(revs_coded_prep) <- min_max


# Write .tsv file (required for GERvader) for minimal processed data
write.table(revs_coded_prep["min"], file = "GERvader/gervader_data_min.tsv", quote = FALSE, sep= "\t", col.names = NA)

# Write .tsv file (required for GERvader) for maximal processed data
write.table(revs_coded_prep["max"], file = "GERvader/gervader_data_max.tsv", quote = FALSE, sep= "\t", col.names = NA)


#-----------------------------------------------------------------------------

# ---- 3. Import Gervader lexicon for processing ----
GERVaderLexicon <- read.delim("GERvader/GERVaderLexicon.txt", header = F)

# Maximal Processing
GERVaderLexicon_max <- GERVaderLexicon %>%
  slice_tail(n = 34513) %>% # Remove Special Emojis
  slice_head(n = 34497) %>% # Remove Special Emojis
  mutate(V1 = tolower(V1) %>%
                stri_trans_general("de-ASCII")
                wordStem(language = "de") %>%
                str_remove_all(";|\t|\r|\n") %>%
                trimws()) %>%
  unique()

# Minimal Processing
GERVaderLexicon_min <- GERVaderLexicon %>%
  slice_tail(n = 34513) %>% # Remove Special Emojis
  slice_head(n = 34497)


# Count Entries for minimum dictionary
sum(GERVaderLexicon_min$V2 > 0)  # Positive Entries: 16477
sum(GERVaderLexicon_min$V2 < 0)  # Negative Entries: 18020


# Count entries for maximum dictionary
sum(GERVaderLexicon_max$V2 > 0)  # Positive Entries: 3331
sum(GERVaderLexicon_max$V2 < 0)  # Negative Entries: 4072



write.table(GERVaderLexicon_max, file = "GERvader/GERVaderLexicon_max.txt", col.names = F, row.names = F, quote = F, sep = "\t")
write.table(GERVaderLexicon_min, file = "GERvader/GERVaderLexicon_min.txt", col.names = F, row.names = F, quote = F, sep = "\t")


#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
#                           Run Python Script GervaderModule
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------



# ---- 4. Import minimal GERVADER data ----
files_min <- list.files("GERvader/results/mode_All/data_min", full.names = F)

list_results_min <- lapply(files_min, function(x) {
  read.csv(paste0("GERvader/results/mode_All/data_min/",x),
           sep = "\t", header = F)
})

names(list_results_min) <- files_min
list2env(list_results_min, .GlobalEnv)

gervader_sentiment_min <- bind_rows(GERVADER__negative.tsv,
                                    GERVADER_positive.tsv,
                                    GERVADER__neutral.tsv) %>%
  select(V1, V3, V2) %>%
  rename(review = V1) %>%
  rename(doc_id = V2) %>%
  filter(review != "min.text")


gervader_result_min <- right_join(revs_coded, gervader_sentiment_min, by = "doc_id")

remove(GERVADER__negative.tsv, GERVADER_positive.tsv, GERVADER__neutral.tsv, list_results_min)



# ---- 5. Import maximal GERVADER data ----
files_max <- list.files("GERvader/results/mode_All/data_max", full.names = F)
#files_max <- list.files("GERvader/results/mode_All/data_nostem", full.names = F)


list_results_max <- lapply(files_max, function(x) {
  read.csv(paste0("GERvader/results/mode_All/data_max/",x),
           sep = "\t", header = F)
})

names(list_results_max) <- files_max
list2env(list_results_max, .GlobalEnv)

gervader_sentiment_max <- bind_rows(GERVADER__negative.tsv,
                                    GERVADER_positive.tsv,
                                    GERVADER__neutral.tsv) %>%
  select(V1, V3, V2) %>%
  rename(review = V1) %>%
  rename(doc_id = V2) %>%
  filter(review != "max.text")

gervader_result_max <- right_join(revs_coded, gervader_sentiment_max, by = "doc_id")

remove(GERVADER__negative.tsv, GERVADER_positive.tsv, GERVADER__neutral.tsv, list_results_max)



# ---- 6. Results ----

gervader_result <- lapply(list(min = gervader_result_min,
                               max = gervader_result_max), function (x) {
  rename(x sentiment = V3) %>%
    filter(sentiment != 0)
})


test_gerv <- lapply(gervader_result, function(x) {
  test_sent(x, freq = F, num = F)
})


test <- readRDS("../data/test_results.RDS")

test$gervader <- test_gerv

saveRDS(test, "../data/test_results.RDS")
