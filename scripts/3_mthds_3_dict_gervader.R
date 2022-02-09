# Script: Gervader TSV export & import
# Date: 21.01.22
# Author: Marcel
#-----0. Install Packages-----------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,
               scales,
               quanteda,
               irr,
               irrNA)


#-----1. Load data-----------
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

#-----2. Export GERVADER data-----------
# Minimal Preprocessing
gervader_data_min <- minimal %>%
  mutate(text = str_remove_all(text, ";")) %>%
  mutate(text = str_remove_all(text, "\t")) %>%
  mutate(text = str_remove_all(text, "\r")) %>%
  mutate(text = str_remove_all(text, "\n")) %>%
  mutate(text = trimws(text))

gervader_data_min <- gervader_data_min %>%
  relocate(text, .after = doc_id)



# Maximal Preprocessing
gervader_data_max <- maximal %>%
  mutate(text = str_remove_all(text, ";")) %>%
  mutate(text = str_remove_all(text, "\t")) %>%
  mutate(text = str_remove_all(text, "\r")) %>%
  mutate(text = str_remove_all(text, "\n")) %>%
  mutate(text = trimws(text))

gervader_data_max <- gervader_data_max %>%
  relocate(text, .after = doc_id)


# Write .tsv file (required for GERvader) for minimal processed data
write.table(gervader_data_min, file = "GERvader/gervader_data_min.tsv", quote = FALSE, sep= "\t", col.names = NA)

# Write .tsv file (required for GERvader) for maximal processed data
write.table(gervader_data_max, file = "GERvader/gervader_data_max.tsv", quote = FALSE, sep= "\t", col.names = NA)

#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------

#-----4. Import Gervader lexicon for processing-----------

GERVaderLexicon <- read.delim("GERvader/GERVaderLexicon.txt", header = F)

# Maximal Processing 
GERVaderLexicon_max <- GERVaderLexicon %>% 
  mutate(V1 = tolower(V1)) %>%
  mutate(V1 = stri_trans_general(V1, "de-ASCII")) %>%
  mutate(V1 = wordStem(V1, language = "de")) %>%
  unique() %>% 
  mutate(V1 = str_remove_all(V1, ";")) %>%
  mutate(V1 = str_remove_all(V1, "\t")) %>%
  mutate(v1 = str_remove_all(V1, "\r")) %>%
  mutate(v1 = str_remove_all(V1, "\n")) %>%
  mutate(v1 = trimws(V1))
    

GERVaderLexicon_min <- GERVaderLexicon

write.table(GERVaderLexicon_max, file = "GERvader/GERVaderLexicon_max.txt", col.names = F, row.names = F, quote = F, sep = "\t")
write.table(GERVaderLexicon_min, file = "GERvader/GERVaderLexicon_min.txt", col.names = F, row.names = F, quote = F, sep = "\t")

GERVaderLexicon_max_test <- read.delim("GERvader/GERVaderLexicon_max.txt", header = F)

#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------

#-----3. Import minimal GERVADER data-----------
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
  filter(review != "text")


gervader_result_min <- right_join(revs_coded, gervader_sentiment_min, by = "doc_id")

remove(GERVADER__negative.tsv, GERVADER_positive.tsv, GERVADER__neutral.tsv, list_results_min)

#-----4. Import maximal GERVADER data-----------
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
  filter(review != "text")

gervader_result_max <- right_join(revs_coded, gervader_sentiment_max, by = "doc_id")

remove(GERVADER__negative.tsv, GERVADER_positive.tsv, GERVADER__neutral.tsv, list_results_max)




#-----6. Results-----------
gervader_result <- list(gervader_result_min, gervader_result_max)

gervader_result <- lapply(gervader_result, function (x)
  x %>%
    rename(sent_tmp = V3))

names(gervader_result) <- c("min", "max")


test_results_gerv <- lapply(gervader_result, test_meth) # rescale -1,1 nicht in der funktion


test <- readRDS("../data/test_results.RDS")

test$gervader <- test_results_gerv

saveRDS(test, "../data/test_results.RDS")
