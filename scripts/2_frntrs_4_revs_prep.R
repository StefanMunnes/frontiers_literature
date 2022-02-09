# ---- 1. load and prepare review data ----

# all reviews
revs_all <- readRDS("../data/pt_all_reviews.RDS") %>%

  # manualy rename title bc special characters (problem in regex cleaning reviews)
  mutate(book_title =
           case_when(book_subti == "memoscript für reinhard döhl" ~ "experimentelle literatur und internet",
                     book_subti == "Wie wir vor lauter Kommunizieren unser Leben verpassen" ~ "LG",
                     TRUE ~ book_title)) %>%

  # filter empty and duplicates of reviews
  filter(!is.na(rev_text)) %>%
  distinct(rev_text, .keep_all = T) %>%

  rename(book = book_title,
         review = rev_text) %>%

  select(book, book_author, book_tags, book_tpcs, rev_name, review)


# hand coded reviews
revs_coded <- readRDS("../data/frontiers_reviews_coded.RDS")



# ---- 2. minimal pre-processing ----
revs_min_ls <- lapply(list(all = revs_all, coded = revs_coded), function(data) {

  corpus(data, text_field = "review") %>%

    tokens(what = "word",

           remove_punct = TRUE,
           remove_symbols = TRUE,
           remove_numbers = TRUE,
           remove_separators = TRUE,

           verbose = TRUE)
})

revs_all_min <- revs_min_ls[[1]]
revs_coded_min <- revs_min_ls[[2]]



# ---- 3. maximal pre-processing ----

# manipulate standart stopword list from quanteda
# (remove negations, reinforcements & corpus specific common words, add own words)
stopwords <- stopwords("german") %>%
	wordStem(language = "de") %>%
  stri_trans_general("de-ASCII") %>%
  unique() %>%
  str_remove_all("aber|einig|einmal|geg|jed|kein|nicht|nur|sehr|uber|weit|viel|zwar")
stopwords <- stopwords[stopwords != ""]
stopwords <- c(stopwords, "autor", "autorin", "rezension", "rezensent", "rezensentin",
                "buch", "roman", "geschicht")


revs_max_ls <- lapply(list(all = revs_all, coded = revs_coded), function(data) {

  mutate(data,

    across(book:review, ~ stri_trans_general(.x, "de-ASCII")),

    ## remove "noise" from review texts
    # replace book title with EMPTY string (empty tokens for GloVe model weightings)
    review = str_replace_all(review, book, "EMPTY"),

    # replace author, tags, topics & reviewer name with EMPTY string
    across(book_author:rev_name, ~ str_remove_all(.x, "([A-Z]*[().;,|])|Hg") %>%
             str_squish() %>%
             str_replace_all(" ", "|")),

    remove_string = paste(book_author, book_tags, book_tpcs, rev_name, sep = "|") %>%
      str_replace_all("\\|\\|", "\\|"),

    review = str_replace_all(review, remove_string, "EMPTY")
  ) %>%

  select(!c(book_tags, book_tpcs, rev_name, remove_string)) %>%

  corpus(text_field = "review") %>%

  tokens(what = "word",

         remove_punct = TRUE,
         remove_symbols = TRUE,
         remove_numbers = TRUE,
         remove_separators = TRUE,

         include_docvars = TRUE,

         verbose = TRUE) %>%

  tokens_wordstem(language = "de") %>%

  tokens_tolower() %>%

  tokens_remove(pattern = "*EMPTY*", padding = TRUE) %>%
  tokens_remove(stopwords, padding = TRUE)
})

revs_all_max <- revs_max_ls[[1]]
revs_coded_max <- revs_max_ls[[2]]


# ---- 4. save pre-processed review data ----
save(revs_all_min, revs_all_max,
     file = "../data/frontiers_reviews_all_prep.Rdata")

save(revs_coded_min, revs_coded_max,
     file = "../data/frontiers_reviews_coded_prep.Rdata")



# ---- 5. get descriptive statistics of corpus after preprocessing steps ----

revs_stats_ls <- lapply(list(min = revs_min_ls, max = revs_max_ls), function(i) {
  
    lapply(i, function(x) {
    
    corpus <- vapply(x, paste, FUN.VALUE = character(1), collapse = " ") %>%
      corpus()
    
    stats <- summary(corpus, n = length(corpus)) %>% 
      select(Types, Tokens) %>% 
      summary()
      
    return(stats)
  })
})

saveRDS(revs_stats_ls, file = "../data/frontiers_reviews_stats.RDS")
