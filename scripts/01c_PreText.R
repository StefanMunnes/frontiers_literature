#Makes use of the preprocessing test tool Pretext, to decide what preprocessing steps should be taken.
#18.01.2022
#Corinna
# Quelle: http://www.mjdenny.com/getting_started_with_preText.html


install.packages("devtools")
write('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', file = "~/.Renviron", append = TRUE)

devtools::install_github("matthewjdenny/preText")

library(preText)
library(quanteda)


#load corpus
corpus <- readRDS("./data/frontiers_corp.rds")
preprocessed_documents <- factorial_preprocessing(
  corpus,
  use_ngrams = TRUE,
  verbose = TRUE)


preText_results <- preText(
  preprocessed_documents,
  dataset_name = "Corpus_reviews",
  distance_method = "cosine",
  num_comparisons = 50,
  verbose = TRUE)

#load PreText Data
preText_results <- readRDS("data/preText_results.rds")
pdf("graphs/PreText_plot1.pdf")
preText_score_plot(preText_results)
remove_intercept = TRUE)
dev.off()

pdf("graphs/PreText_plot2.pdf")
regression_coefficient_plot(preText_results)
dev.off()