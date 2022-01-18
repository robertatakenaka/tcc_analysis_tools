library("tidyverse")
library("stringi")
library("fastDummies")
library("reshape2")

options(digits = 2)
options(scipen = 999)
# specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))


read_csv_file <- function(dataset_path) {
  print(dataset_path)
  ds <- read.csv(dataset_path, header=TRUE, sep=",") %>% 
    distinct()
  return (ds)
}



add_subject_area_and_citer_journal_id_to_refs <- function(ds_refs, ds_only_one_subject_area_journals, file_path) {
  # cria a coluna citer_journal_id com o ID do periódico (ISSN ID) que cita a referencia
  ds_refs <- mutate(ds_refs, issn_id=substr(ds_refs$pid, 2, 10))

  # obtém o subjconjunto de referências de periódicos que tem apenas 1 area
  ds <- inner_join(ds_refs, ds_only_one_subject_area_journals, by="issn_id")

  write.csv(ds, file_path)
  return (ds)
}



readRenviron(".appenv")

# 
JOURNAL_ONLY_ONE_SUBJECT_AREA_FILE_PATH <- Sys.getenv("JOURNAL_ONLY_ONE_SUBJECT_AREA_FILE_PATH")
REFERENCES_SOURCE_FILE_PATH <- Sys.getenv("REFERENCES_SOURCE_FILE_PATH")
REFERENCES_ANALYSIS_FOLDER_PATH <- Sys.getenv("REFERENCES_ANALYSIS_FOLDER_PATH")
REFERENCES_WITH_SUBJECT_AREA_AND_ISSN_ID_FILE_PATH <- Sys.getenv("REFERENCES_WITH_SUBJECT_AREA_AND_ISSN_ID_FILE_PATH")

ds_only_one_subject_area_journals <- read_csv_file(JOURNAL_ONLY_ONE_SUBJECT_AREA_FILE_PATH)

print("Reading REFERENCES_SOURCE_FILE_PATH")
ds_refs <- read_csv_file(REFERENCES_SOURCE_FILE_PATH)

# obtém um subconjunto de referencias, apenas aquelas cujo periódico tem 1 area
print("Labeling references")
ds_labeled_refs <- add_subject_area_and_citer_journal_id_to_refs(ds_refs, ds_only_one_subject_area_journals, REFERENCES_WITH_SUBJECT_AREA_AND_ISSN_ID_FILE_PATH)
print(head(ds_labeled_refs))
