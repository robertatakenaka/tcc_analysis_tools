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


get_ds_references_selected_by_area <- function(ds_only_one_subject_area_refs, selected_area, file_path) {
    ds <- filter(ds_only_one_subject_area_refs, area==selected_area) %>%
          write.csv(file_path)
    return (ds)
}


split_references_by_suject_area <- function(ds_refs_labeled_with_area, ds_only_one_subject_area_journals, folder_path) {
  areas_freq <- data.frame(table(ds_only_one_subject_area_journals$area))

  # dir.create(folder_path)

  for (area in areas_freq$Var1) {
      file_path <- file.path(folder_path, paste(area, "csv", sep="."))
      print(file_path)
      print(area)
      get_ds_references_selected_by_area(ds_refs_labeled_with_area, area, file_path)
  }
}



readRenviron(".appenv")

# 
JOURNAL_ONLY_ONE_SUBJECT_AREA_FILE_PATH <- Sys.getenv("JOURNAL_ONLY_ONE_SUBJECT_AREA_FILE_PATH")
REFERENCES_ANALYSIS_FOLDER_PATH <- Sys.getenv("REFERENCES_ANALYSIS_FOLDER_PATH")
REFERENCES_WITH_SUBJECT_AREA_AND_ISSN_ID_FILE_PATH <- Sys.getenv("REFERENCES_WITH_SUBJECT_AREA_AND_ISSN_ID_FILE_PATH")

ds_only_one_subject_area_journals <- read_csv_file(JOURNAL_ONLY_ONE_SUBJECT_AREA_FILE_PATH)
ds_refs_with_subj_area_and_issn_id <- read_csv_file(REFERENCES_WITH_SUBJECT_AREA_AND_ISSN_ID_FILE_PATH)

print("Classifying references")
split_references_by_suject_area(ds_refs_with_subj_area_and_issn_id, ds_only_one_subject_area_journals, REFERENCES_ANALYSIS_FOLDER_PATH)



