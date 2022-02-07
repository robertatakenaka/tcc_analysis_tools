# ==================================================
# 
# Análise da presença do idioma inglês de resumos
# 
# ==================================================


library(dplyr)
library(tidyverse)
library(stringi)
source("my_functions_files.R")
source("my_functions_translated_codes.R")
source("my_functions_graphics.R")

options(digits = 2)
options(scipen = 999)


readRenviron("envs/abstracts__english_presence__multilingue_abst.env")


ABSTRACT_WITH_ENGLISH_PRESENCE_CSV_FOLDER_PATH <- Sys.getenv("ABSTRACT_WITH_ENGLISH_PRESENCE_CSV_FOLDER_PATH")
ABSTRACTS_WITH_LESS_COMMON_LANGS_FILE_PATH <- Sys.getenv("ABSTRACTS_WITH_LESS_COMMON_LANGS_FILE_PATH")
PIDS_FILE_PATH <- Sys.getenv("PIDS_FILE_PATH")


if (file.exists(ABSTRACTS_WITH_LESS_COMMON_LANGS_FILE_PATH)) {
    ds_less_common_langs_abstracts <- read_csv_file(ABSTRACTS_WITH_LESS_COMMON_LANGS_FILE_PATH)
} else {
    ds <- read_csv_file(ABSTRACT_WITH_ENGLISH_PRESENCE_CSV_FOLDER_PATH) %>%
        filter(lang != 'zz')

    # select the articles which abstract languages is not in en, pt, es
    # the resulting dataset contains only the pid column, distinct
    ds_less_common_langs <- ds %>%
        filter(!(lang %in% c('en', 'es', 'pt'))) %>%
        select(pid) %>%
        distinct() 
    print(nrow(ds_less_common_langs))

    # create a dataset which contains all the abstracts of the articles
    # which have abstracts in less common langs
    ds_less_common_langs_abstracts <- inner_join(ds_less_common_langs, ds, by='pid') %>%
        select(pid, lang, subject_area) %>%
        group_by(pid, subject_area) %>%
        mutate(n_abstracts=n()) %>%
        ungroup() %>%
        filter(n_abstracts > 1) %>%
        write_csv_file(ABSTRACTS_WITH_LESS_COMMON_LANGS_FILE_PATH)
}

ds_n_abstracts <- ds_less_common_langs_abstracts %>%
    group_by(n_abstracts) %>%
    summarise(n=n())
print(ds_n_abstracts)

ds_years <- ds_less_common_langs_abstracts %>%
    mutate(year=as.numeric(substr(pid, 11, 14))) %>%
    group_by(year) %>%
    summarise(n=n())
print(ds_years)


ds_subset_less_common_langs_abstracts <- ds_less_common_langs_abstracts %>%
    mutate(year=as.numeric(substr(pid, 11, 14))) %>%
    filter(n_abstracts > 2 & year < 2017)

langs <- ds_subset_less_common_langs_abstracts %>%
    select(lang) %>%
    distinct()
print(langs)

ds_pid_and_lang <- ds_subset_less_common_langs_abstracts %>%
    select(pid) %>%
    distinct() %>%
    write_csv_file(PIDS_FILE_PATH)
     
print(nrow(ds_pid_and_lang))









