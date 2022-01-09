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


add_subject_area_and_citer_journal_id_to_abstracts <- function(ds_abstracts, ds_only_one_subject_area_journals, file_path) {
  # cria a coluna citer_journal_id com o ID do periódico (ISSN ID) que cita a referencia
  ds_abstracts <- mutate(ds_abstracts, issn_id=substr(ds_abstracts$pid, 2, 10))

  # obtém o subjconjunto de referências de periódicos que tem apenas 1 area
  ds <- inner_join(ds_abstracts, ds_only_one_subject_area_journals, by="issn_id")

  write.csv(ds, file_path)
  return (ds)
}


get_ds_abstracts_selected_by_area <- function(ds_only_one_subject_area_abstracts, selected_area, file_path) {
    ds <- filter(ds_only_one_subject_area_abstracts, area==selected_area)
    write.csv(ds, file_path)
    return (ds)
}


classify_abstracts_by_suject_area <- function(ds_abstracts_labeled_with_area, ds_only_one_subject_area_journals, folder_path) {
  areas_freq <- data.frame(table(ds_only_one_subject_area_journals$area))

  # dir.create(folder_path)

  for (area in areas_freq$Var1) {
      file_path <- file.path(folder_path, paste(area, "csv", sep="."))
      print(file_path)
      print(area)
      ds <- get_ds_abstracts_selected_by_area(ds_abstracts_labeled_with_area, area, file_path)
  }
  return (ds)
}



readRenviron(".appenv")

# 
JOURNAL_ONLY_ONE_SUBJECT_AREA_FILE_PATH <- Sys.getenv("JOURNAL_ONLY_ONE_SUBJECT_AREA_FILE_PATH")
ABSTRACTS_SOURCE_FILE_PATH <- Sys.getenv("ABSTRACTS_SOURCE_FILE_PATH")
ABSTRACTS_ANALYSIS_FOLDER_PATH <- Sys.getenv("ABSTRACTS_ANALYSIS_FOLDER_PATH")
LABELED_ABSTRACTS_FILE_PATH <- Sys.getenv("LABELED_ABSTRACTS_FILE_PATH")

ds_only_one_subject_area_journals <- read_csv_file(JOURNAL_ONLY_ONE_SUBJECT_AREA_FILE_PATH)

print("Reading ABSTRACTS_SOURCE_FILE_PATH")
ds_abstracts <- read_csv_file(ABSTRACTS_SOURCE_FILE_PATH)

# obtém um subconjunto de referencias, apenas aquelas cujo periódico tem 1 area
print("Labeling abstracts")
ds_labeled_abstracts <- add_subject_area_and_citer_journal_id_to_abstracts(ds_abstracts, ds_only_one_subject_area_journals, LABELED_ABSTRACTS_FILE_PATH)
print(head(ds_labeled_abstracts))

print("Classifying abstracts")
classify_abstracts_by_suject_area(ds_labeled_abstracts, ds_only_one_subject_area_journals, ABSTRACTS_ANALYSIS_FOLDER_PATH)



