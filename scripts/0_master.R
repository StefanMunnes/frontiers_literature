if(!require("pacman")) install.packages("pacman")
pacman::p_load("dplyr", "tidyr", "stringr", "stringi", "rdnb", "openxlsx",
               "irr", "irrNA", "scales", "parallel", "pbapply", "quanteda",
              "quanteda.sentiment", "SnowballC", "text2vec", "word2vec",
               "vroom","readxl","stopwords","devtools", "quanteda.dictionaries","magrittr")


# 1. scrape all books and additional informations from Perlentaucher
source("1_pt_all_0_master.R", print.eval = T, encoding = "utf-8")

# 2. create sample for and pre-process final hand coded reviews
source("2_frntrs_0_master.R", print.eval = T, encoding = "utf-8")

# 3. run all different methods
source("3_mthds_0_master.R", print.eval = T, encoding = "utf-8")
