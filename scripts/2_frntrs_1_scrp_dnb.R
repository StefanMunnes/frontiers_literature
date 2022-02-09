if(!require("pacman")) install.packages("pacman")
pacman::p_load("rdnb", "dplyr", "tidyr", "stringr")

# load modified functions from dnb-package to get also (co-)author id and byears
source("code_dnb/gets.R")
source("code_dnb/utils.R")


# DNB token for api
save_token(readLines("../data/token.txt"))


# ---- 1. scrape book information from DNB via ISBN ----
load("../../PT_all/data/pt_all_books.Rdata")

isbns <- pt_books_df$book_isbn

dnb_books_isbns <- lapply(isbns, function(isbn) {
  print(isbn)
  try(dnb_advanced_sm(isbn, limit = "all", clean = T))
})

names(dnb_books_isbns) <- isbns

save(dnb_books_isbns, file = "../data/dnb_pt_all_raw.Rdata")



# ---- 2. merge with PT book data + fill & clean ----
load("../data/dnb_pt_all_raw.Rdata")

books_dnb_pt <- bind_rows(purrr::keep(dnb_books_isbns, is.list)) %>%

  # select and create dnb variables
  select(author, contrib, title, subtitle, publisher, year, language, langorig,
         price, pages, format, isbn, ddc, subject, genre, topic, keyword, link) %>%
  rename_with(~ paste0("dnb_", .x)) %>%

  # merge with PT book data
  right_join(pt_books_df, by = c("dnb_isbn" = "book_isbn")) %>%
  select(starts_with("book_"), url_book, starts_with("dnb_")) %>%

  # clean up date data
  mutate(dnb_year = case_when(between(dnb_year, 0, 22) ~ dnb_year + 2000,
                              between(dnb_year, 23, 999) ~ dnb_year + 1000,
                              TRUE ~ dnb_year)) %>%

  #   if multiple books: fill information about DDC/subject and keep just correct year
  group_by(dnb_isbn) %>%
  fill(dnb_ddc, dnb_subject, .direction = "downup") %>%
  filter(n() == 1 | (n() > 1 & book_year == dnb_year)) %>%
  filter(row_number() == 1) %>%
  ungroup()


save(books_dnb_pt, file = "data/dnb_pt_all.Rdata")

################################################################################

ddc <- books_dnb_pt %>%
  mutate(ddc = ifelse(is.na(dnb_ddc), dnb_subject, dnb_ddc),
         ddc = ifelse(is.na(ddc), "Missing", ddc),
         B = str_detect(ddc, "B|8[0-9]{2,2}|59"),
         B830 = str_detect(ddc, "830"),
         K = str_detect(ddc, "K|07"))

table(ddc$B)
table(ddc$B830)
table(ddc$K)
