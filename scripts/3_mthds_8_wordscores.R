# Script: Wordscores
# Author: Johannes
# Project: Frontiers
# Last updated: 01/02/2022


# ---- 1 Loading ----

# Packages
if(!require("pacman")) install.packages("pacman")
pacman::p_load("dplyr",              # select()
               "irr",                # icc() in test_meth()
               "magrittr",           # %>% and %<>%
               "ggplot2",            # geom_violin()
               "quanteda",           # corpus()
               "quanteda.textmodels",# textmodel_wordscores()
               "quanteda.textplots", # textplot_scale1d()
               "scales",             # rescale() in test_meth()
               "stringr",            # str_remove_all()
               "tibble")             # rownames_to_column()

# Data
load("../data/frontiers_reviews_coded_prep.Rdata")


# ---- 2 Prepare data ----

# Select minimally or maximally preprocessed data
for(i in c("min", "max")){
revs_txts <- paste0("revs_coded_", i) %>% get()

# Extract document variables
revs_dvar <- docvars(revs_txts)

# Convert tokens to data frame
revs_txts %<>%
  vapply(paste, FUN.VALUE = character(1), collapse = " ") %>%
  corpus() %>%
  convert("data.frame")

# Combine texts and docvars
revs_df <-
  revs_dvar %>%
  mutate(doc_id = revs_txts %>% pull(doc_id),
         text = revs_txts %>% pull(text),
         types = ntype(text),
         .before = "book") %>%
  arrange(-sent_hc_7, -types) %>%
  # training set across all dimensions
  group_by(sent_hc_7) %>%
  # training set each 50% with most types
  mutate(train = row_number() < 0.5 * n()) %>%
  ungroup() %>%
  mutate(rank = row_number(), .before = "doc_id")
rm(revs_txts, revs_dvar)

# Create document feature matrix
revs_dfm <-
  revs_df %>%
  corpus() %>%
  tokens() %>%
  dfm() %>%
  dfm_trim(min_termfreq = 5,
           termfreq_type = "count")
# %>%
#   dfm_trim(min_docfreq = 2,
#            max_docfreq = 0.9 * nrow(.),
#            docfreq_type = "count")


# ---- 3 Perform wordscores analysis ----

# Training set
train_dfm <-
  revs_dfm %>%
  dfm_subset(train == TRUE)

# Scoring words
tmod_ws <- textmodel_wordscores(train_dfm, train_dfm$sent_hc_7)
scrs_feat_ws <-
  full_join(
  tmod_ws$wordscores %>%
    data.frame(score = .) %>%
    rownames_to_column(var = "term"),
  tmod_ws$x %>%
    colSums() %>% log() %>%
    data.frame(logfreq = .) %>%
    rownames_to_column(var = "term"),
  by = "term") %>%
  arrange(-score, -logfreq)
# textplot_scale1d(tmod_ws, margin = "features", highlighted = c("hymnisch", "jubelt",
#                                                                "nicht",
#                                                                "haar", "verriss"))

# High logfreq and
# positive sentiment
scrs_feat_ws %>% arrange(-logfreq) %>% filter(score > 6.5) %>% head(10)
# neutral sentiment
scrs_feat_ws %>% arrange(-logfreq) %>% filter(score > 4.5 & score < 5.5) %>% head(10)
# negative sentiment
scrs_feat_ws %>% arrange(-logfreq) %>% filter(score < 2.5) %>% head(10)

# Estimation set
estim_dfm <- revs_dfm %>%
  dfm_subset(!(docnames(revs_dfm) %in%
                 docnames(train_dfm)))

# Scoring texts
tmod_ws_pred <- predict(tmod_ws, se.fit = TRUE, estim_dfm)
scrs_text_ws <-
  tmod_ws_pred %>%
  .[["fit"]] %>%
  data.frame() %>%
  rownames_to_column(var = "doc_id") %>%
  rename(., sent_tmp = `.`) %>%
  arrange(-sent_tmp)
# textplot_scale1d(tmod_ws_pred, margin = "documents")

# Merge with original data
revs_df %<>%
  left_join(scrs_text_ws, by = "doc_id")

assign(paste0("scrs_", i), revs_df)

}

# Save results in list
sent_ws <- list(min = scrs_min,
                max = scrs_max)

# Apply custom function
test_ws <- sent_ws %>%
  lapply(test_meth)

# # test reliability
# sent_df <- sent_ws$min %>%
#   mutate(sent_z = scale(sent_tmp),
#          sent_7 = rescale(sent_tmp, to = c(1, 7), digits = 0),
#          sent_3 = case_when(sent_7 < 3 ~ 1,
#                             sent_7 < 6 ~ 2,
#                             sent_7 < 8 ~ 3))
# 
# cor <-   cor(sent_df$sent_hc_z, sent_df$sent_z, use = "complete.obs")
# icc_7 <- icc(select(sent_df, sent_hc_7, sent_7))
# icc_3 <- icc(select(sent_df, sent_hc_3, sent_3))
# 
# list(num = icc_7$subjects,
#      cor = as.numeric(cor),
#      icc_7 = icc_7$value,
#      icc_3 = icc_3$value)


# ---- 4. Save test results ----

test <- readRDS("../data/test_results.RDS")

test$wordscores <- test_ws

saveRDS(test, "../data/test_results.RDS")


# # Hand-coded vs. estimated
# cor(revs_df$sent_hc_7,
#     revs_df$ws,
#     use = "complete.obs")
# revs_df %>%
#   with((sent_hc_7 - sent_tmp)^2) %>%
#   mean(na.rm = TRUE)
# 
# revs_df %>%
#   filter(!is.na(ws)) %>%
#   mutate(across(c(sent_hc_7, sent_tmp), scale)) %>%
#   # mutate(across(c(sent_hc_7, sent_tmp), round)) %>%
#   select(sent_hc_7, sent_tmp) %>%
#   # filter(sent_hc_7 == sent_tmp) %>%
#   # nrow() / nrow(scrs_text_ws)
#   irr::icc()
# 
# ## R                      = 0.578 (max: 0.597)
# ## MSE                    = 2.560 (2.488)
# ## ICC                    = 0.068 (0.097)
# ## ICC scaled             = 0.578 (0.597)
# ## ICC scaled round       = 0.539 (0.544)
# ## precision round        = 0.192 (0.192)
# ## precision scaled round = 0.379 (0.400)