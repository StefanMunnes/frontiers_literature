
load("data/data/pt_all_urls_authors.Rdata")


# ---- 1. scrape book URLs from PT with scraped author URL list ----
for (letter in names(pt_authors_url)) {

  print(letter)

  pt_books_df_ls <- lapply(pt_authors_url[[letter]], function(author_url) {

    Sys.sleep(0.5)

    print(author_url)

    books_data <- NULL
    counter <- 1

    # Bei Fehler: immer wieder wiederholen (vor allem Verbindungsproblem)
    while (is.null(books_data) & counter < 4) {

      books_data <- tryCatch({

        html <- read_html(paste0("https://www.perlentaucher.de", author_url))

        author   <- html_nodes(html, "#col_middle .buch-autor") %>% html_text()
        author_t <- html_nodes(html, ".authordata .paragraph") %>% html_text()

        url_book <- html_nodes(html, "div > h3 > a") %>% html_attr("href") %>%
          stri_paste(collapse = ";") %>%
          ifelse(is.null(.), NA, .)

        data.frame(author = author, author_text = author_t, url_book = url_book,
                   stringAsFactor = F)

      }, error = function(e) {
        message(e)
        Sys.sleep(20)
        return(NULL)
      })

      # add 1 to counter, just try 3 times to scrape
      if (is.null(books_data)) counter <- counter + 1
    }


    # for authors with more than 40 Books, go to next Page and append data
    if (str_count(books_data$url_book, ";") == 39) {

      books_data2 <- try({

        html <- read_html(paste0("https://www.perlentaucher.de", author_url, "?p=1&q="))

        author   <- html_nodes(html, "#col_middle .buch-autor") %>% html_text()
        author_t <- html_nodes(html, ".authordata .paragraph") %>% html_text()

        url_book <- html_nodes(html, "div > h3 > a") %>% html_attr("href") %>%
          stri_paste(collapse = ";") %>%
          ifelse(is.null(.), NA, .)

        data.frame(author = author, author_text = author_t, url_book = url_book,
                   stringAsFactor = F)
      })

      books_data <- bind_rows(books_data, books_data2)
    }


    # data.frame zurück geben; vorher URLs aufspliten und Anzahl der Bücher zählen
    books_data %>%
      separate_rows(url_book, sep = ";") %>%
      mutate(url_book = ifelse(url_book == "", NA, url_book),
             author_books = sum(!is.na(url_book)),
             url_author = author_url)
  })

  names(pt_books_df_ls) <- pt_authors_url[[letter]]

  save(pt_books_df_ls,
       file = paste0("data/tmp/pt_all_books_", letter, ".Rdata"))
}


# ---- 2. load and append single scraped url files by letter ----
pt_books_df_ls <- lapply(letters, function(letter) {
  load(paste0("data/tmp/pt_all_books_", letter, ".Rdata"))
  pt_books_df_ls
})

names(pt_books_df_ls) <- letters

save(pt_books_df_ls, file = "../data/pt_all_raw_books.RDS")
