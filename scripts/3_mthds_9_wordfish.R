# Script: Wordfish
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
               "stringr")            # str_remove_all()

# Data
load("../data/frontiers_reviews_coded_prep.Rdata")

# All reviews yields error message
## Error in asMethod(object) : 
## Cholmod error 'problem too large' at file ../Core/cholmod_dense.c, line 102


# ---- 2 Prepare data ----

# Select minimally or maximally preprocessed data
for(i in c("min", "max")){
revs_txts <- paste0("revs_coded_", i) %>% get()

# Create document feature matrix
revs_dfm <- revs_txts %>%
  dfm() %>%
  dfm_trim(min_termfreq = 5,
           termfreq_type = "count") %>%
  # dfm_trim(min_docfreq = 2,
  #          max_docfreq = 0.9 * nrow(.),
  #          docfreq_type = "count") %>%
  dfm_subset(rowSums(.) != 0)

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
  arrange(-sent_hc_7, -types)
rm(revs_txts, revs_dvar)


# ---- 3 Perform wordfish analysis ----

# tmod_wf <- textmodel_wordfish(revs_dfm)
# saveRDS(tmod_wf, paste0("../data/tmp/tmod_wf_", i, ".rds"))
tmod_wf <- readRDS(paste0("../data/tmp/tmod_wf_", i, ".rds"))

# View model summary
tmod_wf %>% summary()

# Extract scored words
scrs_feat_wf <- data.frame(feat = tmod_wf$features,
                           wf = tmod_wf$beta) %>%
  arrange(-wf)

# # Plot word scores
# textplot_scale1d(tmod_wf, margin = "features", highlighted = c("hymnisch", "jubelt",
#                                                                "nicht",
#                                                                "haar", "verriss"))

# -> dimension novel -- non-fiction?

# Predicted texts
scrs_text_wf <-
  data.frame(doc_id = tmod_wf$docs,
             sent_tmp = tmod_wf$theta) %>%
  arrange(-sent_tmp)

# # Plot text scores
# textplot_scale1d(tmod_wf, margin = "documents")

# Merge with original data
revs_df %<>%
  left_join(scrs_text_wf, by = "doc_id")

assign(paste0("scrs_", i), revs_df)

}

# Save results in list
sent_wf <- list(min = scrs_min,
                max = scrs_max)

# Apply custom function
test_wf <- sent_wf %>%
  lapply(test_meth)


# ---- 4. Save test results ----

test <- readRDS("../data/test_results.RDS")

test$wordfish <- test_wf

saveRDS(test, "../data/test_results.RDS")


# # Hand-coded vs. estimated
# cor(revs_df$sentiment,
#     revs_df$wf,
#     use = "complete.obs")
# revs_df %>%
#   with((sentiment - wf)^2) %>%
#   mean(na.rm = TRUE)
# 
# # minimal: R = -0.047, MSE = 29.854
# # maximal: R = -0.015, MSE = 29,747
# 
# revs_df %>%
#   filter(!is.na(wf)) %>%
#   mutate(across(c(sentiment, wf), scale)) %>%
#   # mutate(across(c(sentiment, wf), round)) %>%
#   select(sentiment, wf) %>%
#   # filter(sentiment == wf) %>%
#   # nrow() / nrow(scrs_text_wf)
#   irr::icc()
# 
# ## R                      = -0.047 (max: -0.015)
# ## MSE                    = 29.854 (29.747)
# ## ICC                    = -0.786 (-0.780)
# ## ICC scaled             = -0.047 (-0.015)
# ## ICC scaled round       = -0.061 (-0.026)
# ## precision round        =  0.027 ( 0.023)
# ## precision scaled round =  0.308 ( 0.301)