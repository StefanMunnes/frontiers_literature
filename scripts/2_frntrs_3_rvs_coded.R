
coding_csvs <- list.files(path = "../../../data/coding_pt_all/server/user_data", full.names = T)
names(coding_csvs) <- str_sub(coding_csvs, -13, -5)

coder_names <- c("Rater1", "Rater2", "Rater3", "Rater4", "Rater5", "Rater6", "Rater7")

pt_reviews_df <- readRDS("../data/pt_all_reviews.RDS")


revs_coded_raw <- lapply(coding_csvs, read.csv, fileEncoding = "UTF-8") %>%
  bind_rows(.id = "file") %>%

  rename_with(tolower) %>%
  rename(book = buch) %>%

  # keep just reviews with valid sentiment rating
  filter(between(sentiment, 1, 7)) %>%

  mutate(coder = factor(str_sub(file, -1, -1), labels = coder_names)) %>%

  # recode double-coded sentiments from long to wide format
  pivot_wider(names_from = coder, values_from = sentiment, names_prefix = "sent_") %>%

  group_by(rev_id) %>%
  mutate(coder_n = n()) %>%
  fill(starts_with("sent_"), .direction = "downup") %>%
  filter(row_number() == 1) %>%
  ungroup()


# calculate ICC with random missing ratings
iccNA(select(revs_coded_raw, starts_with("sent_"))) # ICC(1) one-way random -> single measure


set.seed(33) # for jitter while round mean of double ratings

revs_coded <- revs_coded_raw %>%

  # create sentiment from two ratings; jitter randomly if in between (=.5)
  unite(col = "sentiment", starts_with("sent_"), na.rm = T) %>%
  separate(sentiment, c("sent_1", "sent_2"), remove = F, convert = T, fill = "right") %>%
  mutate(
         # random choice of sent_1 or sent_2 (just for double coded reviews)
         choise = ifelse(str_detect(sentiment, "_"), 
                         sample(2, n(), replace = TRUE), 1),
         sentiment = ifelse(choise == 1, sent_1, sent_2),

         sent_hc_z = scale(sentiment),
         sent_hc_7 = round(jitter(sentiment, 0.1))) %>%

  select(rev_id, review, book, starts_with("sent_hc")) %>%

  # add pt data to remove title, author and reviewer name
  left_join(pt_reviews_df, by = c("book" = "book_title", "review" = "rev_text")) %>%
  distinct(rev_id, review, .keep_all = T) %>%

  select(book, book_author, book_tags, book_tpcs, rev_name, review, starts_with("sent_hc"))


saveRDS(revs_coded, file = "../data/frontiers_reviews_coded.RDS")
