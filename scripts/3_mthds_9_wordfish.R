# ---- 1 Prepare data ----

# Load data
load("../data/frontiers_reviews_coded_prep.Rdata")

# Run on minimally and maximally preprocessed
sent_wf <- c("min", "max") %>%
  
  lapply(function(i){
    
    revs_txts <- paste0("revs_coded_", i) %>% get()
    
    # Create document feature matrix
    revs_dfm <- revs_txts %>%
      dfm() %>%
      dfm_trim(min_termfreq = 5,
               termfreq_type = "count") %>%
      
      # Remove rows empty after trimming
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
             .before = "book")
    rm(revs_txts, revs_dvar)
    
    
    # ---- 2 Perform wordfish analysis ----
    
    # Start timer
    tic()
    
    # Train and predict
    tmod_wf <- textmodel_wordfish(revs_dfm)

    # Scored words in data frame
    scrs_feat_wf <- tibble(term = tmod_wf$features,
                           score = tmod_wf$beta,
                           psi = tmod_wf$psi) %>%
      arrange(-psi)
    
    # Scored texts in data frame
    scrs_text_wf <-
      tibble(doc_id = tmod_wf$docs,
             sentiment = tmod_wf$theta) %>%
      arrange(-sentiment)
    
    # Stop timer
    runtime_wf <- toc()
    
    # Merge with original data
    revs_df %<>%
      left_join(scrs_text_wf, by = "doc_id")
    
    # Plot word scores
    scrs_feat_wf %>%
      filter(!is.na(score)) %>%
      ggplot(aes(x = score, y = psi)) +
      geom_point(data = . %>% filter(!term %in% c("hymnisch", "jubelt", "nicht", "haar", "verriss", "bach", "komponist")),
                 color = "grey65") +
      geom_point(data = . %>% filter(term %in% c("hymnisch", "jubelt", "nicht", "haar", "verriss", "bach", "komponist")),
                 color = "grey30") +
      geom_text_repel(data = . %>% filter(term %in% c("hymnisch", "jubelt", "nicht", "haar", "verriss", "bach", "komponist")),
                      aes(label = term),
                      seed = 42,
                      direction = "both",
                      point.size = 1,
                      min.segment.length = 99,
                      color = "grey30",
                      size = 3.5) +
      theme_minimal() +
      xlab("Estimated sentiment") +
      ylab("Term fixed effect")
    ggsave(filename = paste0("../graphs/wordfish_", i, ".png"))
    ggsave(filename = paste0("../graphs/wordfish_", i, ".pdf"))
    
    # Combine in list
    list(text = revs_df,
         feat = scrs_feat_wf,
         time = with(runtime_wf, toc - tic))
    
  })

# Name list elements
names(sent_wf) <- c("min", "max")

# Extract text scores
text_wf <- list(min = sent_wf$min$text,
                max = sent_wf$max$text)

# Custom test statistics
test_wf <-
  text_wf %>%
  lapply(function(df){
    test_sent(df, num = FALSE, freq = FALSE)
  })

# Extract feature scores
feat_wf <- list(min = sent_wf$min$feat,
                max = sent_wf$max$feat)

# Negative terms
feat_wf %>%
  lapply(function(df){
    df %>%
      filter(score < 4) %>%
      nrow()
  })

# Positive terms
feat_wf %>%
  lapply(function(df){
    df %>%
      filter(score > 4) %>%
      nrow()
  })

# Matches
c("min", "max") %>%
  lapply(function(i){
    text_wf %>%
      .[[i]] %>%
      pull(text) %>%
      tokens() %>%
      tokens_tolower() %>%
      tokens_remove(feat_wf %>%
                      .[[i]] %>%
                      filter(score %>% is.na()) %>%
                      pull(term)) %>%
      ntoken() %>%
      mean()
  })

# ---- 3 Save test results ----

test <- readRDS("../data/test_results.RDS")

test$wordfish <- test_wf

saveRDS(test, "../data/test_results.RDS")
