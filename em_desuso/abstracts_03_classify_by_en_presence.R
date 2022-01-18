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


get_freq_ds <- function(table_ds_column, file_path='', rename_freq='') {
  ds_col <- data.frame(table_ds_column)
  total_obs = sum(ds_col$Freq)
  ds_col <- (mutate(ds_col, total=total_obs, freq_perc=Freq/total_obs) %>% arrange(desc(Freq)))

  ds_col <- rename(ds_col, "pid"="Var1")
  if (rename_freq != '') {
    ds_col <- rename(ds_col, "not_english_freq"="Freq")
  }
  if (file_path != '') {
    write.csv(ds_col, file_path)
  }
  
  return (ds_col)
}


get_pids_which_have_no_english_abstract_ds <- function(abstracts_ds, pids_freq_ds, file_path) {
  # remove as observacoes que tem new_lang = 'en'
  not_english_ds <- filter(abstracts_ds, new_lang != 'en')
  
  # conta a frequencia de pids, após ter excluído as linhas do idioma ingles
  not_english_pids_freq_ds <- get_freq_ds(table(not_english_ds$pid), '', "not_english_freq")
  print(head(not_english_pids_freq_ds))
  
  print(head(pids_freq_ds))
  
  # cria um novo dataset juntando pids_freq_ds e not_english_pids_freq_ds,
  # mantendo somente os PIDs que não tiveram a freq modificada,
  # pois significa que eles não tinham ingles originalmente
  pids_which_have_not_english_abstract <- inner_join(pids_freq_ds, not_english_pids_freq_ds, by="pid")
  print(head(pids_which_have_not_english_abstract))
  
  filtered <- filter(pids_which_have_not_english_abstract, Freq == not_english_freq)

  print(head(filtered))
  ds <- inner_join(abstracts_ds, filtered, by="pid")

  # write file
  write.csv(ds, file_path)

  return (ds)
}


get_pids_which_have_english_abstracts_ds <- function(abstracts_ds, pids_freq_ds) {
  english_ds <- filter(abstracts_ds, new_lang == 'en')
  pids_which_have_english_abstracts <- inner_join(english_ds, pids_freq_ds, by="pid")
  return (pids_which_have_english_abstracts)
}


get_pids_which_have_only_english_abstract_ds <- function(abstracts_ds, pids_which_have_english_abstracts, file_path) {
  filtered <- filter(pids_which_have_english_abstracts, Freq==1)

  ds <- inner_join(abstracts_ds, filtered, by="pid")

  # write file
  write.csv(ds, file_path)

  return (ds)
}


get_pids_which_have_abstracts_and_one_is_in_english_ds <- function(abstracts_ds, pids_which_have_english_abstracts, file_path) {
  filtered <- filter(pids_which_have_english_abstracts, Freq>1)

  ds <- inner_join(abstracts_ds, filtered, by="pid")

  # write file
  write.csv(ds, file_path)

  return (ds)
}


get_real_not_english_abstracts_ds <- function(ds) {
    ds <- filter(ds, str_detect(text, "the") == FALSE)
    return (ds)
}


classify_abstracts_ds_by_en_presence <- function(abstracts_ds, area, folder) {
    
    print(area)
    # obtenção da freq de pids em resumos
    pids_freq_ds <- get_freq_ds(table(abstracts_ds$pid))

    # arquivo com pids q não tem resumo em ingles
    file_path <- file.path(folder, paste(area, "abstracts_not_english.csv", sep="."))
    get_pids_which_have_no_english_abstract_ds(abstracts_ds, pids_freq_ds, file_path)

    # pids que tem resumo em ingles
    english_abstract_ds <- get_pids_which_have_english_abstracts_ds(abstracts_ds, pids_freq_ds)

    # pids que tem somente resumo em ingles
    file_path <- file.path(folder, paste(area, "abstracts_only_in_english.csv", sep="."))
    get_pids_which_have_only_english_abstract_ds(abstracts_ds, english_abstract_ds, file_path)

    # pids que tem resumos e um deles é em ingles
    file_path <- file.path(folder, paste(area, "abstracts_and_one_is_in_english.csv", sep="."))
    get_pids_which_have_abstracts_and_one_is_in_english_ds(abstracts_ds, english_abstract_ds, file_path)

}

classify_abstracts_datasets_by_en_presence <- function(ds_only_one_subject_area_journals, folder_path) {
  areas_freq <- data.frame(table(ds_only_one_subject_area_journals$area))

  # dir.create(folder_path)

  for (area in areas_freq$Var1) {
      file_path <- file.path(folder_path, paste(area, "csv", sep="."))
      ds_abstracts <- read_csv_file(file_path)

      classify_abstracts_ds_by_en_presence(ds_abstracts, area, folder_path)
  }

}

readRenviron(".appenv")

# 
JOURNAL_ONLY_ONE_SUBJECT_AREA_FILE_PATH <- Sys.getenv("JOURNAL_ONLY_ONE_SUBJECT_AREA_FILE_PATH")
ABSTRACTS_ANALYSIS_FOLDER_PATH <- Sys.getenv("ABSTRACTS_ANALYSIS_FOLDER_PATH")

ds_only_one_subject_area_journals <- read_csv_file(JOURNAL_ONLY_ONE_SUBJECT_AREA_FILE_PATH)

print("Classifying abstracts")
classify_abstracts_datasets_by_en_presence(ds_only_one_subject_area_journals, ABSTRACTS_ANALYSIS_FOLDER_PATH)



