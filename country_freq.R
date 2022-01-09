############################################################################
#
#  Identifica as frequência de artigos nas coleções (países)
#
############################################################################

library("tidyverse")

options(digits = 2)
options(scipen = 999)
# specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))

evaluate_collections <- function(dataset_path) {
  print(dataset_path)
  ds <- read.csv(dataset_path, header=TRUE, sep=",") %>% distinct()
  collections <- data.frame(table(ds$collection))
  total_obs = sum(collections$Freq)
  return (mutate(collections, perc=Freq*100/total_obs) %>% arrange(desc(Freq)))
}

readRenviron(".appenv")
CSV_FOLDER_PATH <- Sys.getenv("ARTICLES_CSV_FOLDER_PATH")
ANALYSIS_FOLDER_PATH <- Sys.getenv("ARTICLES_ANALYSIS_FOLDER_PATH")

FILENAMES <- list("articles", "articles_langs")
for(filename in FILENAMES){
  ds_path = file.path(CSV_FOLDER_PATH, filename)
  ds_path_csv <- paste(ds_path, "csv", sep=".")
  ds_collections <- evaluate_collections(ds_path_csv)
  print(head(ds_collections))
  new_name = paste(filename, "collections.csv", sep="_")
  new_file <- file.path(ANALYSIS_FOLDER_PATH, new_name)
  print(new_file)
  write.csv(ds_collections, new_file)
}
