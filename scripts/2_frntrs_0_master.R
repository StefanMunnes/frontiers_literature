# 1. scrape additional information about DDC from Deutsche Nationalbibliothek
source("2_frntrs_1_scp_dnb.R", print.eval = T, encoding = "utf-8")

# 2. sample reviews for human-coding: purposive and random for 7 coders
source("2_frntrs_2_revs_sample.R", print.eval = T, encoding = "utf-8")

# 3. create dataset from human coded reviews and calculate ICC
source("2_frntrs_3_revs_coded.R", print.eval = T, encoding = "utf-8")

# 4. as corpus and pre-process hand-coded and pt_all texts
source("2_frntrs_4_revs_prep.R", print.eval = T, encoding = "utf-8")
