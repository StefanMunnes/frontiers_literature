

# ---- get pos. & neg. similar words from seeds for min. & max. pre-processed corpus ----

# function to create list of unique pos. & neg. words from list of seeds for
#   min. & max. pre-processed list of word_vector list
seeds2dicts <- function(word_vectors_ls, seeds_ls, max = 400) {

  words_df_ls <- lapply(word_vectors_ls, function(wv) {

    lapply(seeds_ls, function(seeds) {

      lapply(seeds, function(seed) {

        tryCatch(
          { # extract similar vectors (words) from seed vectors (words)
            word_vec <- wv[seed, , drop = FALSE]
            cos_sim  <- sim2(x = wv, y = word_vec, method = "cosine", norm = "l2")
            cos_sim_head <- head(sort(cos_sim[,1], decreasing = TRUE), max)

            word_sim_df <- data.frame(word = names(cos_sim_head),
                                      cos_sim = cos_sim_head,
                                      seed = seed,
                                      row.names = NULL)

            return(word_sim_df)
          },
          error = function(error_message) {
            message(seed)
            message(error_message)
            message()
            return(NULL)
          }
        )
      }) %>% bind_rows() # %>% na.omit() %>% distinct()
    })
  })

  dict_ls <- lapply(words_df_ls, function(words_df) {

    bind_rows(words_df, .id = "sentiment")
  })

  return(dict_ls)
}


# create function to filter unique pos. or neg. words above given cosinus similarity in same amount
filter_words <- function(df, min_sim = .25) {

  # filter minimum of similarity
  filter(df, cos_sim > min_sim) %>%

    # keep unique words for each sentiment
    arrange(desc(cos_sim)) %>%
    distinct(sentiment, word, .keep_all = TRUE) %>%

    # keep words just in one sentiment
    group_by(word) %>%
    filter(n() == 1) %>%

    # keep just same amount of words for each sentiment
    group_by(sentiment) %>%
    mutate(max_size = max(row_number())) %>%
    ungroup() %>%
    mutate(max_size = min(max_size)) %>%
    group_by(sentiment) %>%
    filter(row_number() <= min(max_size))
}


# ---- self-trained GloVe models with min & max pre-processing  ----
glove_wv <- readRDS("../data/glove_word_vectors.RDS")

load("../data/glove_seeds.Rdata")

names_ngrams <- c("onegram", "bigram")


# hand coded seeds
dict_hc_min <- seeds2dicts(list(glove_wv$onegram$min, glove_wv$bigram$min), seeds_hc_min)
dict_hc_max <- seeds2dicts(list(glove_wv$onegram$max, glove_wv$bigram$max), seeds_hc_max)

names(dict_hc_min) <- names_ngrams
names(dict_hc_max) <- names_ngrams

dict_hc <- list(min = lapply(dict_hc_min, filter_words),
                max = lapply(dict_hc_max, filter_words))


# rize and zorn seeds
dict_rz_min <- seeds2dicts(list(glove_wv$onegram$min, glove_wv$bigram$min), seeds_rz_min)
dict_rz_max <- seeds2dicts(list(glove_wv$onegram$max, glove_wv$bigram$max), seeds_rz_max)

names(dict_rz_min) <- names_ngrams
names(dict_rz_max) <- names_ngrams

dict_rz <- list(min = lapply(dict_rz_min, filter_words),
                max = lapply(dict_rz_max, filter_words))


tmp <- list(hc = dict_hc, rz = dict_rz)

saveRDS(tmp, file = "../data/glove_dict_tmp.RDS")


# ---- pre-trained german GloVe word vectors from deepset ----
wv_deepset <- vroom("C:/Users/munnes/Daten/frontiers_embeddings/deepset_vectors_glove.txt",
              delim = " ", col_names = FALSE)

row_names <- wv_deepset$X1
wv_deepset <- as.matrix(wv_deepset[, -1])
rownames(wv_deepset) = row_names

# 1309281 rows * 300 dimensions

# hand coded seeds
dict_glove_pre_hc <- seeds2dicts(list(wv_deepset), lapply(seeds_hc_min, tolower))

# rize and zorn seeds
dict_glove_pre_rz <- seeds2dicts(list(wv_deepset), lapply(seeds_rz_min, tolower))


dict_glove_pre <- lapply(list(hc = dict_glove_pre_hc[[1]], 
                              rz = dict_glove_pre_rz[[1]]), function(dict) {
                                filter_words(dict, min_sim = .3)
                              })


# ---- save all dictionarys ----
glove_dict_ls <- list(hc = dict_hc, rz = dict_rz, pre = dict_glove_pre)

saveRDS(glove_dict_ls, file = "../data/glove_dict_ls.RDS")
