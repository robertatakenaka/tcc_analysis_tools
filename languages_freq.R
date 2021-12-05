library("tidyverse")

options(digits = 2)
options(scipen = 999)
# specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))

evaluate_langs <- function(dataset_path) {
  print(dataset_path)
  ds <- read.csv(dataset_path, header=TRUE, sep=",") %>% distinct()
  langs <- data.frame(table(ds$lang))
  total_obs = sum(langs$Freq)
  return (mutate(langs, perc=Freq*100/total_obs) %>% arrange(desc(Freq)))
}

readRenviron(".appenv")
 # CSV_FOLDER_PATH = "/PATH/csv/"
CSV_FOLDER_PATH <- Sys.getenv("CSV_FOLDER_PATH")
ANALYSIS_LANGS_FOLDER_PATH <- Sys.getenv("ANALYSIS_LANGS_FOLDER_PATH")

FILENAMES <- list("abstracts", "article_titles", "keywords")
for(filename in FILENAMES){
  ds_path = file.path(CSV_FOLDER_PATH, filename)
  ds_path_csv <- paste(ds_path, "csv", sep=".")
  ds_langs <- evaluate_langs(ds_path_csv)
  print(head(ds_langs))
  new_name = paste(filename, "langs.csv", sep="_")
  new_file <- file.path(ANALYSIS_LANGS_FOLDER_PATH, new_name)
  print(new_file)
  write.csv(ds_langs, new_file)
}
