# ---- create word embeddings with GloVe model ----

# compare min & max
# compare 1gram & 2gram
# keep constant: term_count_min; window_size; rank/dimensions, iterations

load("../data/frontiers_reviews_all_prep.Rdata")

# iterator object over
it <- lapply(list(min = revs_all_min, max = revs_all_max), function(data) {
  itoken(as.list(data), progressbar = TRUE)
})


## create vocabulary
# just 1-grams
vocab_1 <- pblapply(it, function(docs) {
  create_vocabulary(docs) %>%
    prune_vocabulary(term_count_min = 5) %>%
    vocab_vectorizer()
})

# 1- & 2-grams
vocab_2 <- pblapply(it, function(docs) {
  create_vocabulary(docs, ngram = c(ngram_min = 1L, ngram_max = 2L)) %>%
    prune_vocabulary(term_count_min = 5) %>%
    filter(!str_detect(term, "^_|_$")) %>%
    vocab_vectorizer()
})


# create term co-occurrence matrices (see stanford: 10 for semantic information)
tcm_ls <- pblapply(list(onegram = vocab_1, bigram = vocab_2), function(ngram) {

  pblapply(list(min = "min", max = "max"), function(prep) {
    
    create_tcm(it = it[[prep]], 
               vectorizer = ngram[[prep]], 
               skip_grams_window = 10)
  })
})


# estimate GloVe word vectors
set.seed(333)

glove_ls <- lapply(tcm_ls, function(tcm_ngram) {

  lapply(tcm_ngram, function(tcm_prep) {

    glove <- GlobalVectors$new(rank = 300, x_max = 10, learning_rate = 0.1)
    wv_main <- glove$fit_transform(tcm_prep, n_iter = 10, n_threads = detectCores())
    wv_context <- glove$components
    word_vectors <- wv_main + t(wv_context)

    return(word_vectors)
  })
})


# save GloVe word vectors
saveRDS(glove_ls, file = "../data/glove_word_vectors.RDS")


# get length and size of objects
glove_ls_size <- lapply(glove_ls, function(ngram) {
  
  lapply(ngram, function(vocab) {
    
    nrow = nrow(vocab)
    size = format(object.size(vocab), units = "Mb")
    return(list(nrow = nrow, size = size))
  })
})



# In a few words, GloVe is an unsupervised learning algorithm that puts emphasis
# on the importance of word-word co-occurences to extract meaning rather than
# other techniques such as skip-gram or bag of words. The idea behind it is that
# a certain word generally co-occurs more often with one word than another.
# The word ice is more likely to occur alongside the word water for instance.
