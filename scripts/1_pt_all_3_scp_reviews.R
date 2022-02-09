# Script:
# scrape informations about book and reviews per book-url
# nested in author-url and save for each letter
# load and append letter combined scraped informations in the end

# Objects:
# pt_revs_df_a_ls  = list of data.frames per author
# pt_revs_df_b_ls  = list of data.frames per book
# pt_revs_df_raw_l = raw data.frame (for each letter, save seperately)
# pt_revs_df_raw   = final raw data.frame (append all letter raw data.frame)


# create function to simplify code: scrapted html to text via specific node
html2text <- function(html, node) {
  html_nodes(html, node) %>% html_text(trim = T) %>%
    stri_paste(collapse = " | ") %>%
    ifelse(is.null(.), NA, .)
}


load("data/pt_all_raw_books.Rdata")


# ---- 1. scrape header and text of reviews from Perlentaucher by book link ----
for (letter in names(pt_books_df_ls)[23:26]) {

  print(letter)

  pt_revs_df_a_ls <- lapply(pt_books_df_ls[[letter]], function(author_url) {

    Sys.sleep(0.5)

    pt_revs_df_b_ls <- lapply(author_url$url_book, function(book_url) {

      print(book_url)

      book_rev_data <- NULL
      counter <- 1

      # Bei Fehler: immer wieder wiederholen (vor allem Verbindungsproblem)
      while (is.null(book_rev_data) & counter < 4) {

        book_rev_data <- tryCatch({

          html <- read_html(paste0("https://www.perlentaucher.de", book_url))

          author <- html_nodes(html, ".bookauthor") %>% html_children() %>%
            html_text() %>% stri_paste(collapse = "; ")
          title  <- html2text(html, ".booktitle")
          subti  <- html2text(html, ".bookdata .smaller")
          infos  <- html2text(html, ".gray")
          blurb  <- html2text(html, "h4+ .smaller")
          tags   <- html2text(html, ".kw .kw")
          tpcs   <- html2text(html, "h4+ ul li")
          news   <- html2text(html, "h3.newspaper")
          text   <- html2text(html, "#col_middle .paragraph")

          data.frame(book_author = author, book_title = title,
                     book_subti = subti, book_infos = infos, book_text = blurb,
                     book_tags = tags, book_tpcs = tpcs,
                     rev_news = news, rev_text = text,
                     url_book = book_url) %>%
            separate_rows(rev_news, rev_text, sep = " \\| ")

        }, error = function(e) {

          message(e)

          Sys.sleep(20)

          return(NULL)
        })

        if (is.null(book_rev_data)) counter <- counter + 1
      }

      book_rev_data
    })

    bind_rows(pt_revs_df_b_ls)
  })

  pt_revs_df_raw_l <- bind_rows(pt_revs_df_a_ls)

  save(pt_revs_df_raw_l, file = paste0("data/tmp/pt_all_raw_reviews_", letter, ".Rdata"))
}


# ---- 2. load and append single scraped list of data.frames by letter ----
pt_revs_df_raw <- lapply(letters, function(letter) {
  load(paste0("data/tmp/pt_all_raw_reviews_", letter, ".Rdata"))
  pt_revs_df_raw_l
}) %>%
  bind_rows()

save(pt_revs_df_raw, file = "data/pt_all_raw_reviews.Rdata")
