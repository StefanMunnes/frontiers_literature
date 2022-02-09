# create sample for 3 main blocks and 1 training block

# 7 coders with 1000 reviews, 10% overlap -> 6300 unique reviews in total

# all:   300 training, 2000 per block;
# Coder: ~43 training, ~286 per block

# double-checked reviews in  total: 700 (300 Block 0, je 200 Block 2 & 3)
# double-checked reviews for coder: 100 (~43 Block 0, je ~28 Block 2 & 3)

# Block 0:   every review double-checked:  ~43 per coder, ~7 from each other coder
# Block 2/3: ~28 per coder double-checked: ~14 per coder for two other coders

# number of reviews per coder may vary slightly, as we need to look at
#   multiple related reviews per book when splitting the sub-sample

if(!require("pacman")) install.packages("pacman")
pacman::p_load("dplyr", "stringr", "openxlsx")

set.seed(33)


# load and filter all reviews
pt_reviews_df <- readRDS("../data/pt_all_reviews.RDS")

rvws_all <- pt_reviews_df %>%

  # delete translations
  filter(book_revs_n > 0 & !str_detect(book_text, "Aus dem \\w* von")) %>%

  # delete Sachbücher
  filter(!str_detect(book_tags, "Sachb[uü]ch") | !str_detect(book_tpcs, "Sachb[uü]ch")) %>%

  mutate(rev_id = row_number())



# ---- get external book list of purposeful sample ----
rvws_pps <- read.xlsx("../data/misc/bookcollection_purposefull.xlsx") %>%
  rename(book_author = autor) %>%
  mutate_if(is.character, str_squish) %>%
  left_join(rvws_all, by = c("book_author", "book_title"))

# discriptives of pos, neg and mixed reviews
table(rvws_pps$sentiment) # bad: 230  good: 232  mixed: 350


# check for missing (not matching) books from external list
rvws_mis <- filter(rvws_pps, is.na(book_author_n))
write.xlsx(rvws_mis, "../misc/bookcollection_no_match.xlsx", overwrite = T)

# extract urls from books from external list for sample creation
urls_pps <- filter(rvws_pps, !is.na(url_book)) %>%
  select(url_book) %>%
  unique() %>%
  pull()



# ---- get shuffled sample of random books (without purposeful selected) ----
'%ni%' <- Negate('%in%')

urls_rand <- sample(unique(rvws_all$url_book[rvws_all$url_book %ni% urls_pps]), 4000) %>%
  as.data.frame() %>%
  rename(url_book = ".") %>%
  left_join(rvws_all, by = "url_book") %>%

  # create max row number of related reviews
  mutate(row_n = row_number()) %>%
  group_by(url_book) %>%
  mutate(row_book = row_n + (book_revs_n - row_number())) %>%
  ungroup() %>%

  # filter max number of reviews
  filter(row_book <= (6300 - nrow(rvws_pps[!is.na(rvws_pps$rev_text),]))) %>%

  pull(url_book) %>%
  unique()


# ---- combine and shuffle both samples (random and purposeful) ----
urls_smpl <- sample(c(urls_pps, urls_rand))

rvws_smpl <- data.frame(url_book = urls_smpl) %>%
  left_join(rvws_all, by = "url_book") %>%

  # create max row number of related reviews
  mutate(row_n = row_number()) %>%
  group_by(url_book) %>%
  mutate(row_book = row_n + (book_revs_n - row_number())) %>%
  ungroup() %>%

  # create code for blocks
  mutate(block = cut(row_book, breaks = c(0, 300, 2300, 4300, 6300)) %>%
           as.numeric() %>% -1) %>%

  select(rev_id, book_title, rev_text, block, book_revs_n, row_book, url_book)

# save(rvws_smpl, file = "data/sample_20211005.Rdata")
load("../data/sample_20211005.Rdata")


# ---- Block 0 - Test - 05.10. - 12.10. ----
rvws_0 <- filter(rvws_smpl, block == 0) %>%

  mutate(coder = as.numeric(cut(row_book, breaks = 7))) %>%

  group_by(coder) %>%
  mutate(coder_2 = as.numeric(cut(row_book, breaks = 6)) + coder,
         coder_2 = ifelse(coder_2 > 7, coder_2 - 7, coder_2),
         coder = paste0(coder, ";", coder_2)) %>%
  tidyr::separate_rows(coder, sep = ";") %>%

  select(rev_id, book_title, rev_text, coder) %>%
  group_split(coder, .keep = F)


lapply(1:7, function(num) {
  write.csv2(rvws_0[[num]],
             file = paste0("../data/block_0_", num,".csv"),
             row.names = F)
})



# ---- Block 1 - 13.10. - 24.10. ----
rvws_1 <- filter(rvws_smpl, block == 1) %>%

  mutate(coder = as.numeric(cut(row_book, breaks = 7))) %>%
  select(rev_id, book_title, rev_text, coder) %>%
  group_split(coder, .keep = F)


lapply(1:7, function(num) {
  write.csv2(rvws_1[[num]],
             file = paste0("../data/block_1_", num,".csv"),
             row.names = F)
})



# ---- !BREAK! - New sample with more belletristic - 20.10. ----

# Problem with sampling, code doesn't reproduce same samples
# -> therefore, use just used reveiws for exclusion

rvws_0 <- lapply(1:7, function(x) {
  read.csv(paste0("../server/user_data/block_0_", x, ".csv"))
})

rvws_1 <- lapply(1:7, function(x) {
  read.csv(paste0("../server/user_data/block_1_", x, ".csv"))
})

rvws_used_0_1 <- bind_rows(rvws_0, rvws_1) %>%
  group_by(rev_id) %>%
  filter(row_number() == 1)


# load and filter all reviews
load("../../PT_all/data/pt_all_reviews.Rdata")
load("../../DNB/data/dnb_pt_all.Rdata")

books_dnb_pt <- select(books_dnb_pt, !starts_with("book_")) %>% select(!url_book)

rvws_all_new <- pt_reviews_df %>%

  # delete translations
  filter(book_revs_n > 0 & !str_detect(book_text, "Aus dem \\w* von")) %>%

  # delete Sachbücher
  filter(!str_detect(book_tags, "Sachb[uü]ch") | !str_detect(book_tpcs, "Sachb[uü]ch")) %>%

  mutate(rev_id = row_number()) %>%

  # get rid of already used reviews
  anti_join(rvws_used_0_1, by = "rev_id") %>%

  # add DNB data for DDC classification
  left_join(books_dnb_pt, by = c("book_isbn" = "dnb_isbn")) %>%

  # create dummy for german belletristic
  mutate(ddc = ifelse(is.na(dnb_ddc), dnb_subject, dnb_ddc),
         bell = str_detect(ddc, "830"),
         bell = ifelse(is.na(bell), FALSE, bell)) %>%

  # if no german belletristic, keep just half sample
  group_by(url_book) %>%
  mutate(grindex = cur_group_id(),
         grindex = ifelse(bell, 0, grindex)) %>%
  filter(grindex %% 2 == 0)


# nearly 50/50 of german belletristic and other literature after clean up
table(rvws_all_new$bell)


# extract urls from books from external list for sample creation
rvws_pps_new <- read.xlsx("../misc/bookcollection_purposefull.xlsx") %>%
  rename(book_author = autor) %>%
  mutate_if(is.character, str_squish) %>%
  inner_join(rvws_all_new, by = c("book_author", "book_title"))

urls_pps_new <- filter(rvws_pps_new, !is.na(url_book)) %>%
  select(url_book) %>%
  unique() %>%
  pull()


# get shuffled sample of random books (without purposeful selected)
'%ni%' <- Negate('%in%')

urls_rand_new <- sample(unique(rvws_all_new$url_book[rvws_all_new$url_book %ni% urls_pps_new]), 2500) %>%
  as.data.frame() %>%
  rename(url_book = ".") %>%

  left_join(rvws_all_new, by = "url_book") %>%

  # create max row number of related reviews
  mutate(row_n = row_number()) %>%
  group_by(url_book) %>%
  mutate(row_book = row_n + (book_revs_n - row_number())) %>%
  ungroup() %>%

  # filter max number of reviews
  filter(row_book <= (4000 - nrow(rvws_pps_new))) %>%

  pull(url_book) %>%
  unique()


# ---- combine and shuffle both samples (random and purposeful) ----
urls_smpl_new <- sample(c(urls_pps_new, urls_rand_new))

rvws_smpl_new2 <- data.frame(url_book = urls_smpl_new) %>%
  left_join(rvws_all_new, by = "url_book") %>%

  # create max row number of related reviews
  mutate(row_n = row_number()) %>%
  group_by(url_book) %>%
  mutate(row_book = row_n + (book_revs_n - row_number())) %>%
  ungroup() %>%

  # create code for blocks
  mutate(block = cut(row_book, breaks = c(0, 2000, 4000)) %>%
           as.numeric() %>% +1) %>%

  select(rev_id, book_title, rev_text, block, book_revs_n, row_book, url_book)


#save(rvws_smpl, file = "data/sample_20211020.Rdata")
load("../data/sample_20211020.Rdata")



# ---- Block 2 - 25.10. - 07.11. ----
rvws_2 <- filter(rvws_smpl_new, block == 2) %>%

  mutate(coder = as.numeric(cut(row_book, breaks = 7))) %>%

  # create max row number of related reviews (per coder)
  group_by(coder) %>%
  mutate(row_n = row_number()) %>%
  group_by(coder, url_book) %>%
  mutate(row_book = row_n + (book_revs_n - row_number())) %>%
  ungroup(url_book) %>%

  # create sub-sample of reviews to double-check per coder
  mutate(coder_2 = as.numeric(cut(row_book, breaks = c(0, 10, 20, 30), right = F)) + coder,
         coder_2 = ifelse(coder_2 > 7, coder_2 - 7, coder_2),
         coder = ifelse(is.na(coder_2), coder,
                        paste0(coder, ";", coder_2))) %>%
  tidyr::separate_rows(coder, sep = ";")  %>%

  select(rev_id, book_title, rev_text, coder) %>%
  group_split(coder, .keep = F)


lapply(1:7, function(num) {
  write.csv2(rvws_2[[num]],
             file = paste0("../data/block_2_", num,".csv"),
             row.names = F)
})



# ---- Block 3 - 08.10. - 21.10. ----
rvws_3 <- filter(rvws_smpl_new, block == 3) %>%

  mutate(coder = as.numeric(cut(row_book, breaks = 7))) %>%

  # create max row number of related reviews (per coder)
  group_by(coder) %>%
  mutate(row_n = row_number()) %>%
  group_by(coder, url_book) %>%
  mutate(row_book = row_n + (book_revs_n - row_number())) %>%
  ungroup(url_book) %>%

  # create sub-sample of reviews to double-check per coder
  mutate(coder_2 = as.numeric(cut(row_book, breaks = c(0, 10, 20, 30), right = F)) + coder,
         coder_2 = ifelse(coder_2 > 7, coder_2 - 7, coder_2),
         coder_2 = coder_2 + 3, # choose next three coders -> Revs 2 & 3: all cross coder validation
         coder_2 = ifelse(coder_2 > 7, coder_2 - 7, coder_2),
         coder = ifelse(is.na(coder_2), coder,
                        paste0(coder, ";", coder_2))) %>%
  tidyr::separate_rows(coder, sep = ";")  %>%

  select(rev_id, book_title, rev_text, coder) %>%
  group_split(coder, .keep = F)


lapply(1:7, function(num) {
  write.csv2(rvws_3[[num]],
             file = paste0("../data/block_3_", num,".csv"),
             row.names = F)
})



# ---- TEST OF SAMPLING PROCESS ----
rvws_test <- lapply(list(rvws_0, rvws_1, rvws_2, rvws_3),
                    function(l) bind_rows(l, .id = "coder")) %>%
  bind_rows(.id = "block") %>%
  mutate(block = as.numeric(block) - 1) %>%
  group_by(rev_id) %>%
  mutate(rev_n = n())

# table of reviews per coder per block
addmargins(table(rvws_test$block, rvws_test$coder))

# reviews per block: single or double-checked
group_by(rvws_test, block, rev_n) %>%
  summarize(reviews = n())

# reviews per block and coder: single or double-checked
test <- group_by(rvws_test, block, coder, rev_n) %>%
  summarize(reviews = n())
