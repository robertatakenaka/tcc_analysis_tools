library("tidyverse")
library("fastDummies")
library("stringi")
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


add_subject_area_and_citer_journal_id_to_refs <- function(ds_abstracts, ds_only_one_subject_area_journals, file_path) {
  # cria a coluna citer_journal_id com o ID do periódico (ISSN ID) que cita a referencia
  ds_abstracts <- mutate(ds_abstracts, issn_id=substr(ds_abstracts$pid, 2, 10))

  # obtém o subjconjunto de referências de periódicos que tem apenas 1 area
  ds <- inner_join(ds_abstracts, ds_only_one_subject_area_journals, by="issn_id")

  write.csv(ds, file_path)
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
  # remove as observacoes que tem lang = 'en'
  not_english_ds <- filter(abstracts_ds, lang != 'en')
  
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
  english_ds <- filter(abstracts_ds, lang == 'en')
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


identify_journal_area <- function(ds, journal_areas_ds, file_path) {
  ds <- mutate(ds, journal_id=substr(ds$pid, 2, 10))
  journal_areas_ds <- rename(journal_areas_ds, "journal_id"="key", "journal_area"="value")
  with_journal_area_ds <- inner_join(ds, journal_areas_ds, by="journal_id")

  write.csv(with_journal_area_ds, file_path)
  return (with_journal_area_ds)
}


get_real_not_english_abstracts_ds <- function(ds) {
    ds <- filter(ds, str_detect(text, "the") == FALSE)
    return (ds)
}


add_keywords <- function(abstract_ds, keywords_ds, file_path) {
  keywords_ds <- rename(keywords_ds, "keyword"="text")
  with_keywords_ds <- inner_join(abstract_ds, keywords_ds, by="pid")

  write.csv(with_keywords_ds, file_path)
  return (with_keywords_ds)
}


readRenviron(".appenv")
KEYWORDS_SOURCE_CSV_PATH <- Sys.getenv("KEYWORDS_SOURCE_CSV_PATH")
ABSTRACTS_SOURCE_CSV_PATH <- Sys.getenv("ABSTRACTS_SOURCE_CSV_PATH")
ABSTRACTS_ANALYSIS_FOLDER_PATH <- Sys.getenv("ABSTRACTS_ANALYSIS_FOLDER_PATH")
JOURNAL_ONLY_ONE_SUBJECT_AREA_FILE_PATH <- Sys.getenv("JOURNAL_ONLY_ONE_SUBJECT_AREA_FILE_PATH")

abstracts_ds <- read_csv_file(ABSTRACTS_SOURCE_CSV_PATH)
ds_only_one_subject_area_journals <- read_csv_file(JOURNAL_ONLY_ONE_SUBJECT_AREA_FILE_PATH)
keywords_ds <- read_csv_file(KEYWORDS_SOURCE_CSV_PATH)


# alguns resumos estão marcados com idioma errado
## abstracts_ds <- fix_lang(abstracts_ds)


ds_abstracts_with_subject_area_and_citer_journal_id <- add_subject_area_and_citer_journal_id_to_refs(ds_abstracts, ds_only_one_subject_area_journals, file_path)

# obtenção da freq de pids em resumos
pids_freq_ds <- get_freq_ds(table(abstracts_ds$pid))

# arquivo com pids q não tem resumo em ingles
file_path <- file.path(ABSTRACTS_ANALYSIS_FOLDER_PATH, "abstracts_not_english.csv")
not_english_ds <- get_pids_which_have_no_english_abstract_ds(abstracts_ds, pids_freq_ds, file_path)


# pids que tem resumo em ingles
english_abstract_ds <- get_pids_which_have_english_abstracts_ds(abstracts_ds, pids_freq_ds)

# pids que tem somente resumo em ingles
file_path <- file.path(ABSTRACTS_ANALYSIS_FOLDER_PATH, "abstracts_only_in_english.csv")
only_english_abstract_ds <- get_pids_which_have_only_english_abstract_ds(abstracts_ds, english_abstract_ds, file_path)

# pids que tem resumos e um deles é em ingles
file_path <- file.path(ABSTRACTS_ANALYSIS_FOLDER_PATH, "abstracts_and_one_is_in_english.csv")
abstracts_and_one_is_in_english_ds <- get_pids_which_have_abstracts_and_one_is_in_english_ds(abstracts_ds, english_abstract_ds, file_path)

