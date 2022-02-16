
# ---- 1. load raw csv files with ratings from shiny ----

coding_csvs <- list.files(path = "../../../data/coding_pt_all/server/user_data", full.names = T)
names(coding_csvs) <- str_sub(coding_csvs, -13, -5)

pt_reviews_df <- readRDS("../data/pt_all_reviews.RDS")



# ---- 2. create raw data in wide format for ICC ----
revs_coded_raw <- lapply(coding_csvs, read.csv, fileEncoding = "UTF-8") %>%
  bind_rows(.id = "file") %>%

  rename_with(tolower) %>%
  rename(book = buch) %>%

  # keep just reviews with valid sentiment rating
  filter(between(sentiment, 1, 7)) %>%

  mutate(coder = factor(str_sub(file, -1, -1), labels = paste0("Rater", 1:7))) %>% 
  
  group_by(rev_id) %>%
  mutate(rev_n = n()) %>% 
  ungroup()


# count unique and double coded reviews
group_by(revs_coded_raw, rev_n) %>%
  summarize(reviews = n()) %>% 
  mutate(reviews = ifelse(rev_n == 2, reviews / 2, reviews))



# recode double-coded sentiments from long to wide format
revs_coded_raw_wide <- revs_coded_raw %>%
  pivot_wider(names_from = coder, values_from = sentiment, names_prefix = "sent_") %>%

  group_by(rev_id) %>%
  mutate(coder_n = n()) %>%
  fill(starts_with("sent_"), .direction = "downup") %>%
  filter(row_number() == 1) %>%
  ungroup()



# ---- 3. calculate ICC with random missing ratings ----
iccNA(select(revs_coded_raw_wide, starts_with("sent_"))) # ICC(1) one-way random -> single measure



# ---- 4. create finale data set with only one rating per unique review

set.seed(33) # for random choice of double-rated sentiments

revs_coded <- revs_coded_raw_wide %>%

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



# ---- 5. create plot for visualization of distribution of double-coded reviews ----

# create data frame with groups for double-rated reviews
revs_coded_icc_1 <- group_by(revs_coded_raw, rev_id) %>%
  rename(sent = sentiment) %>%
  summarize(coder = paste(coder, collapse = ";"),
            sent  = paste(sent,  collapse = ";")) %>%
  separate(coder, into = c("coder_1", "coder_2"), sep = ";") %>%
  separate(sent,  into = c("sent_1",  "sent_2"),  sep = ";", convert = T) %>%
  
  distinct() %>%

  group_by(coder_1, coder_2) %>%
  na.omit() %>%
  mutate(intercept = 0, slope = 1,
         gr_n = n(),
         gr_id = cur_group_id()) %>% 
  ungroup()


# calculate icc for each rater group
revs_coded_icc_2 <- lapply(1:21, function(x) {
  
  icc_list <- filter(revs_coded_data_plot, gr_id == x) %>%
    select(sent_1, sent_2) %>% 
    icc()
  
  icc_data <- data.frame(icc = round(icc_list[["value"]], 2))
  
  return(icc_data)
  
}) %>% 
  bind_rows(.id = "gr_id") %>% 
  mutate(gr_id = as.numeric(gr_id))

# add icc values to rater_groups
revs_coded_icc_3 <- full_join(revs_coded_icc_1, revs_coded_icc_2)

# create and add mirrored data from original for seccond axis
revs_coded_icc_4 <- revs_coded_icc_3 %>% 
  rename(coder_1 = coder_2, coder_2 = coder_1,
         sent_1  = sent_2,  sent_2  = sent_1) %>%
  bind_rows(revs_coded_icc_3)

# create scatter plot for ratings of double-coded reviews for each rater group
revs_coded_plot <- ggplot(revs_coded_icc_4, aes(x = sent_1, y = sent_2)) +
  geom_point(alpha = .1, size = 1.8) +
  facet_grid(coder_2 ~ coder_1, drop = T) +
  geom_abline(data = revs_coded_icc_4, aes(intercept = intercept, slope = slope), alpha = .6) +
  geom_text(x = 1, y = 6.8, size = 2.2, hjust = 0, aes(label = paste("ICC:", icc))) +
  geom_text(x = 1, y = 6.2, size = 2.2, hjust = 0, aes(label = paste("N:", gr_n))) +
  scale_y_continuous(breaks = seq(1, 7, 1)) +
  scale_x_continuous(breaks = seq(1, 7, 1)) +
  theme_bw() +
  theme(axis.title = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 7.5))

ggsave(filename = "../graphs/revs_coded_icc.pdf", revs_coded_plot, 
       width = 20, height = 20, units = "cm")
ggsave(filename = "../graphs/revs_coded_icc.eps", revs_coded_plot, 
       width = 20, height = 20, units = "cm")
ggsave(filename = "../graphs/revs_coded_icc.jpeg", revs_coded_plot, 
       width = 20, height = 20, units = "cm")
