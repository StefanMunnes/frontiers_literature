
pt_authors_url_ls <- lapply(letters, function(letter) {

  print(letter)

  Sys.sleep(0.5)

  url  <- paste0("https://www.perlentaucher.de/autoren/", letter, ".html")
  urls <- read_html(url) %>%
      html_nodes(".btn-block a") %>%
      html_attr("href")

  lapply(urls, function(url2) {

    print(url2)
    Sys.sleep(1)

    read_html(paste0("https://www.perlentaucher.de", url2)) %>%
      html_nodes("b a") %>% html_attr("href")
  })
})


pt_authors_url_ls <- lapply(pt_authors_url_ls, unlist)

names(pt_authors_url_ls) <- letters


save(pt_authors_url_ls, file = "data/pt_all_urls_authors.Rdata")
