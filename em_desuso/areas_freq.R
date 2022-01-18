############################################################################
#
#  Identifica as frequência de periódicos por área, categorias, e temas
#
############################################################################

library("tidyverse")
library("stringi")

options(digits = 2)
options(scipen = 999)
# specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))


evaluate_classifications <- function(dataset_path) {
  print(dataset_path)
  ds <- read.csv(dataset_path, header=TRUE, sep=",") %>% 
    distinct()
  ds$value <- stri_trans_general(str = ds$value, id = "Latin-ASCII")
  classifications <- data.frame(table(ds$value))
  total_obs = sum(classifications$Freq)
  return (mutate(classifications, perc=Freq*100/total_obs) %>% arrange(desc(Freq)))
}

readRenviron(".appenv")
CSV_FOLDER_PATH <- Sys.getenv("JOURNALS_CSV_FOLDER_PATH")
ANALYSIS_FOLDER_PATH <- Sys.getenv("JOURNALS_ANALYSIS_FOLDER_PATH")

FILENAMES <- list("title_study_areas", "title_subj_categories", "title_subj_description")
for(filename in FILENAMES){
  ds_path = file.path(CSV_FOLDER_PATH, filename)
  ds_path_csv <- paste(ds_path, "csv", sep=".")
  ds_classifications <- evaluate_classifications(ds_path_csv)
  print(head(ds_classifications))
  new_name = paste(filename, "_classifications.csv", sep="_")
  new_file <- file.path(ANALYSIS_FOLDER_PATH, new_name)
  print(new_file)
  write.csv(ds_classifications, new_file)
}

