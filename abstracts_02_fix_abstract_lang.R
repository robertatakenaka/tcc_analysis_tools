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


rollback_lang_dataset <- function(abstracts_ds, file_path) {
    
    print(file_path)
    fixed_ds <- mutate(abstracts_ds, lang=old_lang) %>%
                mutate(fixed=FALSE) %>%
                write.csv(file_path)
}


rollback_lang_datasets <- function(ds_only_one_subject_area_journals, folder_path) {
  areas_freq <- data.frame(table(ds_only_one_subject_area_journals$area))

  # dir.create(folder_path)

  for (area in areas_freq$Var1) {
      file_path <- file.path(folder_path, paste(area, "csv", sep="."))
      ds_abstracts <- read_csv_file(file_path)

      rollback_lang_dataset(ds_abstracts, file_path)
  }

}


fix_lang <- function(lang, text) {
    if (str_detect(str_to_lower(text), " the ") == FALSE) {
        new_lang <- lang
    } else {
        new_lang <- "en"
    }
    return (new_lang)
}


fix_abstracts_lang_ds <- function(abstracts_ds, file_path) {
    
    print(file_path)
    fixed_ds <- mutate(abstracts_ds, old_lang=lang) %>%
                mutate(new_lang=case_when(
                    (str_detect(str_to_lower(text), " the ") == FALSE) ~ lang,
                    (str_detect(str_to_lower(text), " the ") == TRUE) ~ "en",
                )) %>%
                mutate(fixed=(new_lang != old_lang)) %>%
                write.csv(file_path)
}


fix_abstracts_lang_datasets <- function(ds_only_one_subject_area_journals, folder_path) {
  areas_freq <- data.frame(table(ds_only_one_subject_area_journals$area))

  # dir.create(folder_path)

  for (area in areas_freq$Var1) {
      file_path <- file.path(folder_path, paste(area, "csv", sep="."))
      ds_abstracts <- read_csv_file(file_path) %>% 
                      fix_abstracts_lang_ds(file_path)
  }
}

readRenviron(".appenv")

# 
JOURNAL_ONLY_ONE_SUBJECT_AREA_FILE_PATH <- Sys.getenv("JOURNAL_ONLY_ONE_SUBJECT_AREA_FILE_PATH")
ABSTRACTS_ANALYSIS_FOLDER_PATH <- Sys.getenv("ABSTRACTS_ANALYSIS_FOLDER_PATH")

ds_only_one_subject_area_journals <- read_csv_file(JOURNAL_ONLY_ONE_SUBJECT_AREA_FILE_PATH)

print("Classifying abstracts")
fix_abstracts_lang_datasets(ds_only_one_subject_area_journals, ABSTRACTS_ANALYSIS_FOLDER_PATH)

##rollback_lang_datasets(ds_only_one_subject_area_journals, ABSTRACTS_ANALYSIS_FOLDER_PATH)


