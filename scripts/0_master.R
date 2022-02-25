if(!require("pacman")) install.packages("pacman")
pacman::p_load(
  "bibtex",
  "devtools",
  "dplyr",
  "ggplot2",
  "ggrepel",
  "irr",
  "irrNA",
  "magrittr",
  "openxlsx",
  "parallel",
  "pbapply",
  "quanteda",
  "quanteda.sentiment",
  "quanteda.textmodels",
  "quanteda.textplots",
  "rdnb",
  "readxl",
  "scales",
  "SnowballC",
  "stopwords",
  "stringi",
  "stringr",
  "text2vec",
  "tibble",
  "tictoc",
  "tidyr",
  "vroom",
  "word2vec"
)

# add citation of packages to bibtex file
write.bib(.packages(), file = "../write_up/frontiers_lit.bib", append = TRUE)



# 1. scrape all books and additional informations from Perlentaucher
source("1_pt_all_0_master.R", print.eval = T, encoding = "utf-8")

# 2. create sample for and pre-process final hand coded reviews
source("2_frntrs_0_master.R", print.eval = T, encoding = "utf-8")

# 3. run all different methods
source("3_mthds_0_master.R", print.eval = T, encoding = "utf-8")
