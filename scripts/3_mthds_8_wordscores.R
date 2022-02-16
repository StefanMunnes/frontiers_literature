# ---- 1 Prepare data ----

# Load data
load("../data/frontiers_reviews_coded_prep.Rdata")

# Start timer
tic()

# Run on minimally and maximally preprocessed
sent_ws <- c("min", "max") %>%

  lapply(function(i){
    
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
      
      # define training set
      arrange(-sent_hc_7, -types) %>%
      group_by(sent_hc_7) %>%                       # across all dimensions
      mutate(train = row_number() < 0.5 * n()) %>%  # 50% with most types
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
    
    # ---- 2 Perform wordscores analysis ----
    
    # Training set
    train_dfm <-
      revs_dfm %>%
      dfm_subset(train == TRUE)
    
    # Score words
    tmod_ws <- textmodel_wordscores(train_dfm, train_dfm$sent_hc_7)
    
    # Results in data frame
    scrs_feat_ws <-
      full_join(
        tmod_ws$wordscores %>%
          tibble(score = .) %>%
          rownames_to_column(var = "term"),
        tmod_ws$x %>%
          colSums() %>% log() %>%
          tibble(logfreq = .) %>%
          rownames_to_column(var = "term"),
        by = "term") %>%
      arrange(-logfreq)
    
    # # Plot wordscores
    # textplot_scale1d(tmod_ws, margin = "features", highlighted = c("hymnisch", "jubelt",
    #                                                                "nicht",
    #                                                                "haar", "verriss"))
    
    # Estimation set
    estim_dfm <- revs_dfm %>%
      dfm_subset(!(docnames(revs_dfm) %in%
                     docnames(train_dfm)))
    
    # Score texts
    tmod_ws_pred <- predict(tmod_ws, se.fit = TRUE, estim_dfm)
    
    # Results in data frame
    scrs_text_ws <-
      tmod_ws_pred %>%
      .[["fit"]] %>%
      data.frame() %>%
      rownames_to_column(var = "doc_id") %>%
      tibble() %>%
      rename(., sentiment = `.`) %>%
      arrange(-sentiment)
    
    # Merge with original data
    revs_df %<>%
      left_join(scrs_text_ws, by = "doc_id")
    
    # Combine in list
    list(text = revs_df,
         feat = scrs_feat_ws)
    
  })

# Stop timer
runtime_ws <- toc()

# Name list elements
names(sent_ws) <- c("min", "max")

# Extract text scores
text_ws <- list(min = sent_ws$min$text,
                max = sent_ws$max$text)

# Custom test statistics
test_ws <-
  text_ws %>%
  lapply(function(df){
    test_sent(df, num = FALSE, freq = FALSE)
    })

# Extract feature scores
feat_ws <- list(min = sent_ws$min$feat,
                max = sent_ws$max$feat)

# Negative terms
feat_ws %>%
  lapply(function(df){
    df %>%
      filter(score < 4) %>%
      nrow()
  })

# Positive terms
feat_ws %>%
  lapply(function(df){
    df %>%
      filter(score > 4) %>%
      nrow()
  })

# Matches
c("min", "max") %>%
  lapply(function(i){
    text_ws %>%
      .[[i]] %>%
      pull(text) %>%
      tokens() %>%
      tokens_tolower() %>%
      tokens_remove(feat_ws %>%
                      .[[i]] %>%
                      filter(score %>% is.na()) %>%
                      pull(term)) %>%
      ntoken() %>%
      mean()
  })

# ---- 3 Save test results ----

test <- readRDS("../data/test_results.RDS")

test$wordscores <- test_ws

saveRDS(test, "../data/test_results.RDS")
