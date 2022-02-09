
load("../data/pt_all_authors.Rdata")
pt_all_reviews <- readRDS("../data/pt_all_reviews.RDS")
pt_all_books <- readRDS("../data/pt_all_books.RDS")


# ---- 1. unique authors, books, reviews ----
length(unique(pt_authors_df$url_author)) #[pt_authors_df$author_books > 0]
length(unique(pt_all_books$url_book))
length(unique(pt_all_reviews$rev_text[!is.na(pt_all_reviews$rev_text)]))

# ---- 2. summary statistics of reviews
length(unique(pt_all_books$url_book[pt_all_books$book_revs_n > 0]))
mean(pt_all_reviews$book_revs_n)
sd(pt_all_reviews$book_revs_n)

pt_all_reviews$rev_text[!is.na(pt_all_reviews$rev_text)] %>% 
  unique() %>% 
  str_count("\\S+") %>% 
  summary()

hist(pt_all_reviews$rev_date, breaks = 23)
summary(pt_all_reviews$rev_date)
