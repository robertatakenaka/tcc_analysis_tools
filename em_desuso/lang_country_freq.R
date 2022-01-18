############################################################################
#
#  Identifica as frequência de idiomas do texto nas coleções dos países
#
############################################################################

library("tidyverse")
library("stringi")

options(digits = 2)
options(scipen = 999)
# specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))

read_ds <- function(dataset_path) {
  print(dataset_path)
  ds <- read.csv(dataset_path, header=TRUE, sep=",") %>% distinct()
  return (ds)
}

freq_lang_and_country <- function(dataset) {
  ds <- dataset %>%
  group_by(lang, collection) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n), freq_perc = paste0(round(100 * n/sum(n), 0), '%'))
  return (ds)
}


readRenviron(".appenv")
CSV_FOLDER_PATH <- Sys.getenv("LANG_COL_CSV_FOLDER_PATH")
ANALYSIS_FOLDER_PATH <- Sys.getenv("LANG_COL_ANALYSIS_FOLDER_PATH")

FILENAMES <- list("abstracts")
for(filename in FILENAMES){
  ds_path = file.path(CSV_FOLDER_PATH, filename)
  ds_path_csv <- paste(ds_path, "csv", sep=".")
  ds <- read_ds(ds_path_csv)
  print(head(ds))
  ds_lang_and_country <- freq_lang_and_country(ds)
  new_name = paste(filename, "_lang_and_country.csv", sep="_")
  new_file <- file.path(ANALYSIS_FOLDER_PATH, new_name)
  print(new_file)
  write.csv(ds_lang_and_country, new_file)
}

