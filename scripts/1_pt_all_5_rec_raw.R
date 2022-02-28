
# ---- 1. Datensatz mit Autor:in als Beobachtungseinheit ----
load("../data/pt_all_raw_books.RDS")

pt_authors_df <- bind_rows(lapply(pt_books_df_ls, bind_rows)) %>%
  group_by(url_author) %>%
  mutate(url_book = paste(url_book, collapse = " | ")) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  select(author, author_text, author_books, url_author, url_book) %>%
  mutate(across(where(is.character), str_trim)) %>%
  mutate(author = case_when(author == "Gottschalk, Ingrid" ~ "Ingrid Gottschalk",
                            author == "Pöllinger, Sigrid" ~ "Sigrid Pöllinger",
                            TRUE ~ author),
         author = ifelse(str_detect(author, "(Di Cesare)|(von Franckenstein)|(de Smet)|(de Mello)"),
                         str_remove(author, ","), author))

saveRDS(pt_authors_df, file = "../data/pt_all_authors.RDS")


# ---- 2. Datensatz mit Rezenssionen als Beobachtungseinheit ----
load("../data/pt_all_raw_reviews.Rdata")
pt_reviews_names <- readRDS("../data/pt_all_reviews_names.RDS") %>%
  select(rev_id, rev_name)

pt_reviews <- pt_revs_df_raw %>%
  distinct(url_book, rev_text, .keep_all = T) %>%

  mutate(rev_id = row_number()) %>%

  full_join(pt_reviews_names, by = "rev_id") %>%

# manuell fehlerhafte einträge entfernen
  filter(
    !(url_book == "/buch/gernot-griksch/ghetto-bitch.html" &
        !str_detect(rev_text, "Von Poppenbüttel nach Steilshoop"))
  ) %>%

# Buch Informationen
  mutate(
    book_author_n = str_count(book_author, ";") + 1,

    book_pub   = str_extract(book_infos, "^.*?,") %>% str_remove(","),
    book_place = str_extract(book_infos, ",.*?[1-2]") %>%
      str_remove_all("[ ,0-9]"),
    book_year  = str_extract(book_infos, "[0-9]{2,4}"),

    book_isbn = str_extract(book_infos, "[0-9]{8,}"),
    book_type = str_extract(book_infos, "[0-9]{8,}[ A-z]*") %>% str_remove("[0-9 ]*"),
    book_page = str_extract(book_infos, "[0-9]+\\sSeiten") %>% str_remove("Seiten"),
    book_euro = str_extract(book_infos, "[0-9]+,[0-9]+")
  ) %>%

# Rezenssion Informationen
  separate(rev_news, c("rev_news", "rev_date"), sep = ", ",
           extra = "merge", fill = "right") %>%
  mutate(
    rev_news = str_remove(rev_news, "Rezensionsnotiz zu"),
    rev_news = str_replace(rev_news, "Im (Perlentaucher)", "\\1"),
    rev_news = ifelse(str_detect(rev_news, "Im Perlentaucher"), "Perlentaucher", rev_news),
    rev_date = as.Date(rev_date, "%d.%m.%Y")
  ) %>%

# Character clean-Up
  mutate(across(where(is.character), trimws)) %>%
  mutate_if(is.character, list(~na_if(., ""))) %>%

  mutate(book_euro = str_replace(book_euro, ",", ".")) %>%
  mutate(across(c(book_year, book_page, book_euro), as.numeric)) %>%

# Länge der Rezenssionen bestimmen
  mutate(rev_len = str_length(rev_text)) %>%

# Stichwörter und Themen aufbereiten und zählen
  mutate(
    book_tags_n = ifelse(is.na(book_tags), 0, str_count(book_tags, "\\|") + 1),
    book_tpcs_n = ifelse(is.na(book_tpcs), 0, str_count(book_tpcs, "\\|") + 1)
  ) %>%

# Rezenssionen zählen und Länge betimmen
  group_by(url_book) %>%
  filter(!(n() > 1 & is.na(rev_text))) %>% # wenn mehrere Reviews, leere löschen
  mutate(book_revs_n = ifelse(n() == 1 & is.na(rev_text), 0, n())) %>%
  ungroup() %>%

  select(book_author, book_author_n,
         book_title, book_subti, book_year, book_pub, book_place,
         book_isbn, book_type, book_page, book_euro, book_text,
         book_tags, book_tags_n, book_tpcs, book_tpcs_n, book_revs_n,
         rev_news, rev_date, rev_text, rev_len, rev_name,
         url_book)

saveRDS(pt_reviews, file = "../data/pt_all_reviews.RDS")


# ---- 3. Datensatz mit Buch als Beobachtungseinheit ----
pt_books <- pt_reviews %>%
  select(-starts_with("rev")) %>%
  distinct()

saveRDS(pt_books, file = "../data/pt_all_books.RDS")
