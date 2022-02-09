
# ---- 1. load and prepare list of forenames from heise.de ----
# https://www.heise.de/ct/ftp/07/17/182/ ---

# namezip = "../data/misc/names_heise.zip"
# if (!file.exists(namezip)) {
#  download.file("ftp://ftp.heise.de/pub/ct/listings/0717-182.zip",
#                destfile = namezip)
#  unzip(namezip, exdir = "../data/misc/names_heise")
# }

names.raw <- read.delim("../data/misc/nam_dict.txt",
                        header = F, skip = 362)
names.raw$V1     <- substr(names.raw$V1, 1, 86)
names.raw$gender <- trimws(substr(names.raw$V1, 1, 3))
names.raw$name   <- trimws(substr(names.raw$V1, 4, 28)) #%>% stri_trans_general("latin-ascii")
names.raw$laute  <- substr(names.raw$V1, 29, 30)
names.raw$nums   <- gsub(" ", "", substr(names.raw$V1, 31, 86))
names.raw$freq   <- nchar(names.raw$nums)

# calculate digitsum (Quersumme) to filter common names
sum <- sapply(names.raw$nums, function(x) {
  gsub("[ABCD]", "9" , x) %>%
    as.character() %>%
    strsplit(split = "") %>%
    unlist() %>%
    as.numeric() %>%
    sum()
})

names.raw <- cbind(names.raw, sum)
rm(sum)

# Namen mit Sonderzeichen (<...>) Umlauten, daher doppelt (-), sowie "äquivalente Namen" löschen
names.short <- names.raw[(!grepl("<", names.raw$name) &
                          !grepl("-", names.raw$laute)), ]

# nur Namen behalten, die wenigstens in 2 Ländern vorkommen oder eine summierte Relevanz von 4 haben
names.short <- names.short[( names.short$freq > 1 | names.short$sum > 3), 2:3]

# Namen mit "+" berenigen, entweder Bindestrich oder ohne Leerzeichen
names.dash <- names.short[grepl("\\+", names.short$name),]
names.dash$name <- gsub("\\+", "-", names.dash$name)

names.short <- separate(names.short, name, into = c("one", "two"), sep = "\\+") %>%
  mutate(name = ifelse(is.na(two), one, paste0(one, tolower(two)))) %>%
  bind_rows(names.dash) %>%
  select(name, gender)

# doppelte Namen mit unterschiedlichem Geschlecht: zusammenspielen, beide behalten
names.heise <- names.short %>%

  group_by(name) %>%
  filter(n() == 1) %>%
  filter(str_length(name) > 2) %>%
  select(name)


rm(names.raw, names.dash, names.short)

saveRDS(names.heise, file = "../data/misc/names_heise.RDS")
names_heise <- readRDS("../data/misc/names_heise.RDS")



# ---- 2. prepare and clean reviews (remove author name(s) & "Roman") ----
load("../data/pt_all_raw_reviews.Rdata")

reviews_rm_author <- pt_revs_df_raw %>%
  distinct(url_book, rev_text, .keep_all = T) %>%

  mutate(rev_id = row_number()) %>%

  mutate(across(book_author:rev_text, ~ stri_trans_general(.x, "de-ASCII")),
    name = str_remove_all(book_author, "\\s*\\(.*\\)"),
    name = str_replace_all(name, ";\\s", ")|("),
    name = paste0("(", name, ")"),

    forename = str_replace_all(name, "\\(([A-Za-z-]{2,}).+?\\)", "\\1"),
    forename = str_replace_all(forename, "(\\|+(\\(.+\\)))|((\\(.+\\))\\|+)|(\\(.+\\))", ""), # get rid of full copied names bc. no replace in front/end/between names in parantheses
    forename = ifelse(forename == "", NA, forename),
    lastname = str_replace_all(name, "\\(.+?([A-Za-z-]+)\\)", "\\1"),

    rev_text = str_remove_all(rev_text, paste0(name, ".")),
    rev_text = str_remove_all(rev_text, paste0(lastname, ".")),

    rev_text = str_replace_all(rev_text, "Roman(.[^A-Z])", "\\1"),
    rev_text = str_squish(rev_text)) %>% 
  
  select(rev_id, book_title, book_author, name, forename, lastname, rev_text)



# ---- 3. extract reviewer name after "Rezensent/in" ----
regex_reviewer <- "Rezensent[sien]*\\s"
regex_revname <- "[A-Z][A-z-]+\\s([Vv][oa]n\\s)*([A-Z]\\.\\s)*[A-Z][A-z-]+(\\s[A-Z][A-z-]+)*?"

reviews_names1 <- reviews_rm_author %>%
  mutate(row = row_number(),
    rev_name1 = str_extract(rev_text, paste0(regex_reviewer, regex_revname)),
    rev_name1 = str_remove(rev_name1, paste0("(", regex_reviewer, ")"))) %>% 

  # group by first two name parts and take just shortest one (get rid of names with unwanted attachments)
  mutate(rev_name1.2 = word(rev_name1, 1, 2)) %>%
  group_by(rev_name1.2) %>%
  arrange(str_length(rev_name1)) %>%
  mutate(rev_name1 = ifelse(!is.na(rev_name1), first(rev_name1), NA)) %>%
  ungroup() %>% 

  # group by last two name parts and take just shortest one
  mutate(rev_name1.2 = word(rev_name1, -2, -1)) %>%
  group_by(rev_name1.2) %>%
  arrange(str_length(rev_name1)) %>%
  mutate(rev_name1 = ifelse(!is.na(rev_name1), first(rev_name1), NA)) %>%
  select(!rev_name1.2) %>%
  ungroup() %>%

  mutate(rev_name1 = str_replace_all(rev_name1,
                                     c("^Peter Kunisch" = "Hans-Peter Kunisch",
                                       "Isenschmid$" = "Isenschmidt")),
         rev_name1 = str_remove(rev_name1, "(Biografie Willy Brandts)|(Roland M. Beschreibung)|(Walter Benjamins Diktum)|(Eifersuechteleien Walter Benjamin)|(Zeugnisse von Wegbegleitern)|(Richtung Fantasy Fiction)|(Vorgaenge von Recycling)|(Portraet von Praesident Emmanuel Macron)"),
         rev_name1 = str_remove(rev_name1, "(s*\\s(Georg Geschichte|Buch|Urteil|Studie|Neuuebersetzung|Versuch|Referat|Aufsatz|Erinnerungen|Jugendroman|Arbeiterclub|Computerspiele|Darstellung|Kurzgeschichtenband|Maskenspiel|Untersuchung|Fussball-Buch|Ankuendigung|Appetit|Appell|Biografie|Beitrag|Buch|Meinung|Sendung|Haltung|Amuesement|Abhandlung|Mitleid|Auswahl|Thesen|Bilderbuch|Vorfreude))$"),
         rev_forename1 = str_extract(rev_name1, "^[A-Za-z-]{2,}")) %>%

  arrange(row)


# get list of unique reviewer names
names_reviews <- reviews_names1 %>%
  distinct(rev_name1, rev_forename1) %>%
  na.omit()


# add names/gender from reviews to list of names/gender from heise
names_all <- select(names_reviews, rev_forename1) %>%
  rename(name = rev_forename1) %>%
  bind_rows(names_heise) %>%
  mutate(name = stri_trans_general(name, "de-ASCII")) %>%
  distinct(name, .keep_all = T) %>%
  na.omit()


save(names_all, names_reviews, names_heise, file = "../data/names.Rdata")
load("../data/names.Rdata")



# ---- 4. extract names from reviews by list of reviewer names ----

# check for each reviewer name if part of review text -> to character vector of (row)number
reviewers_list1 <- pbsapply(names_reviews$rev_name1,
                            grep,
                            reviews_names1$rev_text) %>%
  lapply(function(x) paste(as.character(x), collapse = ";"))

saveRDS(reviewers_list1, "../data/reviewers_list.RDS")
reviewers_list1 <- readRDS("../data/reviewers_list.RDS")


# transform list to df with (row)number as observation and join with reviews_name1 df
reviews_names2 <- reshape2::melt(reviewers_list1, value.name = "row") %>%
  rename(rev_name2 = L1) %>%
  
  filter(row != "") %>%
  
  separate_rows(row, sep = ";", convert = T) %>%
  group_by(row) %>%
  summarise(rev_name2 = paste(rev_name2, collapse = ";")) %>%
  ungroup() %>%
  
  full_join(reviews_names1, by = "row") %>% 
  
  select(row, rev_id, rev_text,rev_name1, rev_forename1, rev_name2, book_author, name, forename, lastname)



# ---- 5. extract names by first occurrence of forename from long name list ----
reviewers_list2 <- pbsapply(reviews_names2$rev_text, function(text) {
  forenames <- stri_match_all_regex(text, '(?=([A-Z][a-z-]+\\s([Vv][oa]n|[A-Z])))')[[1]][,2]
  forenames <- str_extract_all(forenames, "[A-Z][a-z-]+") %>% unlist()
  forenames_pos <- match(forenames, names_all$name)
  forenames_valid <- forenames[which(!is.na(forenames_pos))]
  paste(forenames_valid, collapse = "|")
})

reviews_names3 <- bind_cols(reviews_names2, reviewers_list2)
names(reviews_names3)[ncol(reviews_names3)] <- "rev_forename3"


# combine reviewer name3 approach with with name1 & name2
regex_lastname <- "\\s([Vv][oa]n\\s)*([A-Z]\\.\\s)*[A-Z][A-z-]+(\\s[A-Z][A-z-]+)*"

reviews_names <- reviews_names3 %>%
  mutate(rev_forename3 = ifelse(rev_forename3 == "", NA, rev_forename3),

         # get full names out of rev text from forenames
         rev_name3 = str_extract(rev_text, paste0("(", rev_forename3, ")",
                                              regex_lastname)),

         # group by first two nameparts and take just short one (get rid of names with unwanted attachments)
         rev_name3.2 = word(rev_name3, 1, 2),
         rev_name3.2 = str_remove(rev_name3, "(s|')$")) %>%
  group_by(rev_name3.2) %>%
  arrange(str_length(rev_name3)) %>%
  mutate(rev_name3 = ifelse(!is.na(rev_name3), first(rev_name3), NA)) %>%
  ungroup() %>%

  mutate(rev_name = ifelse(is.na(rev_name1), rev_name2, rev_name1),
         rev_name = ifelse(is.na(rev_name), rev_name3, rev_name),
         rev_forename = str_extract(rev_name, "^[A-Z][A-z-]+"),
         same12 = rev_name1 == rev_name2,
         same13 = rev_name1 == rev_name3,
         same23 = rev_name2 == rev_name3) %>% 

  select(rev_id, rev_text, book_title, book_author, rev_name, rev_name1, rev_name2, rev_name3,
         rev_forename, rev_forename3, starts_with("same"))

saveRDS(reviews_names, "../data/pt_all_reviews_names.RDS")


# ---- 6. test quality of different approaches ----
qualitytest <- reviews_names %>%
  select(starts_with("rev_name"), starts_with("same")) %>%
  mutate_at(vars(starts_with("rev")), funs(!is.na(.))) %>%
  summarise_all(funs("per" = round(mean(.,na.rm = T) * 100, 2),
                     sum(., na.rm = T))) %>%
  gather("var", "value") %>%
  mutate(var = str_replace(var, "_([per|sum])", ".\\1")) %>%
  separate(var, c("var", "typ"), "\\.") %>%
  spread(typ, value)


library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library(scales)

plot1 <- ggplot(data = qualitytest[1:4,], aes(x = var, y = per, fill = var)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(qualitytest$per[1:4], "%"), vjust = -.5), size = 3) +
  geom_text(aes(label = sum, vjust = 1.5), size = 3) +
  #geom_hline(yintercept = nrow(reviews_names), linetype="dashed") +
  scale_y_continuous(labels = percent_format(scale = 1), breaks = seq(0, 100, 10), limits = c(0, 100)) +
  scale_fill_manual(values = brewer.pal(8, "Set2")[1:4]) +
  xlab("") + ylab("") +
  labs(title = "Namensfunde") +
  theme_classic() +
  theme(legend.position = "none",
        panel.grid.major.y = element_line(colour = "Gray90"))

plot2 <- ggplot(data = qualitytest[5:7,], aes(x = var, y = per, fill = var)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(qualitytest$per[5:7], "%"), vjust = -.5), size = 3) +
  geom_text(aes(label = sum, vjust = 1.5), size = 3) +
  #geom_hline(yintercept = nrow(reviews_names), linetype = "dashed") +
  scale_y_continuous(labels = percent_format(scale = 1), breaks = seq(0, 100, 10), limits = c(0, 100)) +
  scale_fill_manual(values = brewer.pal(8, "Set2")[5:7]) +
  xlab("") + ylab("") +
  labs(title = "Übereinstimmung") +
  theme_classic() +
  theme(legend.position = "none",
        panel.grid.major.y = element_line(colour = "Gray90"),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())

plot <- grid.arrange(plot1, plot2, nrow = 1)
ggsave("output/graphs/revs_names.png", plot)


### data and variable overview
# names.heise     list of forenames and gender from heise.com
# names.reviewers list of forenames and gender from reviews (by Rezensent/in)
# names           combined list of forenames and gender
# reviews_df      full review text data, pre-processt (drop authors names, ...)
# reviews_name1/3 reviews with each extracted names by different approach
# reviews_name    final reviews and all name approaches, similarity

# review          original review
# text            remove special characters & (last)name of author(s)
# text2           remove also "Roman", quotes, lowercase
# rev_name1       reviewer name: by "Resenzent/in"
# rev_name2       reviewer name: by extracted reviewer names list
# rev_name3       reviewer name: by general name list (first occurence)
# same12          reviewer name: rev_name1 == rev_name2
# same13          reviewer name: rev_name1 == rev_name3
# same23          reviewer name: rev_name2 == rev_name3
# rev_gndr1       gender of reviewer by extracted forename and namelist
# rev_gndr2       gender of reviewer by "Rezensent/in"
# same_gndr       gender: rev_gndr1 == rev_gndr2
