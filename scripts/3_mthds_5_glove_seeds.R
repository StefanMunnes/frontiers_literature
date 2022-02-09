
# ---- create list of seeds from different sources ----

# ---- 1. hand coded ----
pos_neg <- c("pos", "neg")

# transform spelling of words to minimal pre-processing
seeds_hc_min <- lapply(pos_neg, function(source) {
  read.xlsx(paste0("../data/seeds/seeds_", source, ".xlsx")) %>%
    pull(Segment) %>%
    na.omit() %>%
    unique()
})

names(seeds_hc_min) <- pos_neg

length(seeds_hc_min[[1]]) # pos: 295
length(seeds_hc_min[[2]]) # neg: 102


# transform spelling of words to maximal pre-processing
seeds_hc_max <- lapply(pos_neg, function(source) {
  read.xlsx(paste0("../data/seeds/seeds_", source, ".xlsx")) %>%
    pull(Segment) %>%
    na.omit() %>%
    dict_max()
})

names(seeds_hc_max) <- pos_neg

length(seeds_hc_max[[1]]) # pos: 219
length(seeds_hc_max[[2]]) # neg: 85



### rize and zorn
seeds_rz_min <- list(pos = c("hervorragend", "brillant", "großartig", "grandios",
                             "wunderbar", "prächtig", "gut", "fantastisch",
                             "ausgezeichnet", "genießen", "zustimmen", "richtig",
                             "vorteilhaft", "perfekt", "intelligent", "übereinstimmend",
                             "getreu", "weise", "konsequent", "überzeugt"),
                     neg = c("schlecht", "abscheulich", "schlimm", "entsetzlich",
                             "dumm", "miserabel", "falsch", "furchtbar",
                             "armselig", "inkorrekt", "böse", "verkehrt", "irren",
                             "scheitern", "Fehler", "unnötig", "ungestützt",
                             "unhaltbar", "ablehnen", "irrtümlich"))

seeds_rz_max <- lapply(seeds_rz_min, dict_max)

# seeds_rz_max <- lapply(seeds_rz_min, function(seeds_rz) {
#   wordStem(seeds_rz, language = "de")  %>%
#     stri_trans_general("de-ASCII") %>%
#     tolower()
# })

length(seeds_rz_min[[1]]) # 20
length(seeds_rz_min[[2]]) # 20

length(seeds_rz_max[[1]]) # 20
length(seeds_rz_max[[2]]) # 20


save(seeds_hc_min, seeds_hc_max, seeds_rz_min, seeds_rz_max,
     file = "../data/glove_seeds.Rdata")
