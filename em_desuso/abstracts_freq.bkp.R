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


get_abstracts_with_langs <- function(dataset, langs) {
    # print(head(dataset)) 
    # langs_dummies <- dummy_columns(.data = dataset,
    #                                 select_columns = c("lang"),
    #                                 remove_selected_columns = T,
    #                                 remove_first_dummy = F)
    # print(head(langs_dummies))
    
    grouped <- dcast(dataset, pid ~ lang)
    print(head(grouped))

    # print("----")
    #return (grouped)
}


get_freq_ds <- function(table_ds_column, file_path) {
  ds_col <- data.frame(table_ds_column)
  total_obs = sum(ds_col$Freq)
  ds_col <- (mutate(ds_col, total=total_obs, freq_perc=Freq/total_obs) %>% arrange(desc(Freq)))
  write.csv(ds_col, file_path)
  
  return (ds_col)
}


get_freq_1_ds <- function(ds) {
  filtered_ds <- filter(ds, Freq == '1')
  return (filtered_ds)
}


add_freq_1_to_ds <- function(ds, ds_freq_1) {
  ds_freq_1 <- rename(ds_freq_1, "pid"="Var1")
  print(ds_freq_1$total)
  joined <- inner_join(ds, ds_freq_1)
  print(joined$total)
  return (joined)
}


get_not_english_ds <- function(ds, file_path) {
  filtered_ds <- filter(ds, lang != 'en')
  print(filtered_ds$total)
  write.csv(filtered_ds, file_path)
  return (filtered_ds)
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
JOURNAL_AREAS_FILE_PATH <- Sys.getenv("JOURNAL_AREAS_FILE_PATH")

ds_abstracts = read_csv_file(ABSTRACTS_SOURCE_CSV_PATH)

abstracts_with_langs <- get_abstracts_with_langs(ds_abstracts)


# arquivo com resultado da freq de pids em resumos
pids_freq_ds_file_path <- file.path(ABSTRACTS_ANALYSIS_FOLDER_PATH, "abstracts_pids_freq.csv")
# obtenção da freq de pids em resumos sem correção
pids_freq_ds <- get_freq_ds(table(ds_abstracts$pid), pids_freq_ds_file_path)

# arquivo com pid q não tem resumo em ingles
not_english_file_path <- file.path(ABSTRACTS_ANALYSIS_FOLDER_PATH, "abstracts_not_english_freq.csv")
freq_1_ds <- get_freq_1_ds(original_freq_ds)
original_with_freq_1_ds <- add_freq_1_to_ds(ds_abstracts, freq_1_ds)
not_english_ds <- get_not_english_ds(original_with_freq_1_ds, not_english_file_path)

real_not_english_abstracts_ds <- get_real_not_english_abstracts_ds(not_english_ds)

# resumo, com 1 idioma não inglês, com área
not_english_with_journal_area_file_path <- file.path(ABSTRACTS_ANALYSIS_FOLDER_PATH, "abstracts_not_english_with_journal_area.csv")
ds_journal_areas <- read_csv_file(JOURNAL_AREAS_FILE_PATH)
with_journal_areas_ds <- identify_journal_area(real_not_english_abstracts_ds, ds_journal_areas, not_english_with_journal_area_file_path)


# frequencia das áreas em dataset artigo com apenas 1 idioma e não é inglês
not_english_with_journal_area_freq_file_path <- file.path(ABSTRACTS_ANALYSIS_FOLDER_PATH, "abstracts_not_english_with_journal_area_freq.csv")
journal_area_freq_ds <- get_freq_ds(table(with_journal_areas_ds$journal_area), not_english_with_journal_area_freq_file_path)
View(journal_area_freq_ds)
print(not_english_with_journal_area_freq_file_path)

# frequencia de idiomas em dataset artigo com apenas 1 idioma e não é inglês
not_english_with_lang_freq_file_path <- file.path(ABSTRACTS_ANALYSIS_FOLDER_PATH, "abstracts_not_english_with_lang_freq.csv")
lang_freq_ds <- get_freq_ds(table(with_journal_areas_ds$lang), not_english_with_lang_freq_file_path)
View(lang_freq_ds)
print(not_english_with_lang_freq_file_path)


# frequencia de idiomas e areas em dataset artigo com apenas 1 idioma e não é inglês
not_english_with_lang_and_jarea_freq_file_path <- file.path(ABSTRACTS_ANALYSIS_FOLDER_PATH, "abstracts_not_english_with_lang_and_jarea_freq.csv")
lang_and_jarea_freq_ds <- get_freq_ds(table(with_journal_areas_ds$lang, with_journal_areas_ds$journal_area), not_english_with_lang_and_jarea_freq_file_path)
View(lang_and_jarea_freq_ds)
print(not_english_with_lang_and_jarea_freq_file_path)

# add keywords to abstracts
keywords_ds <- read_csv_file(KEYWORDS_SOURCE_CSV_PATH)
file_path <- file.path(ABSTRACTS_ANALYSIS_FOLDER_PATH, "abstracts_not_english_with_lang_and_jarea_and_kwds.csv")
add_keywords(real_not_english_abstracts_ds, keywords_ds, file_path)
