
# 1. scrape list of all authors from perlentaucher
source("1_pt_all_1_scp_authors.R", print.eval = T, encoding = "utf-8")

# 2. scrape books for all authors from scraped author list
source("1_pt_all_2_scp_books.R", print.eval = T, encoding = "utf-8")

# 3. scrape reviews for all books from scraped book list
source("1_pt_all_3_scp_reviews.R", print.eval = T, encoding = "utf-8")

# 4. get names of reviewers from reviews
source("1_pt_all_4_get_names.R", print.eval = T, encoding = "utf-8")

# 5. create 3 data sets from raw: authors, books, reviews
source("1_pt_all_5_rec_raw.R", print.eval = T, encoding = "utf-8")

# 6. descriptives for data section
source("1_pt_all_6_desc.R", print.eval = T, encoding = "utf-8")
