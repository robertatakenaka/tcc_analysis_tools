# ==================================================
# 
# Select sample3: papers which abstracts is in less common langs
# For this sample, each abstract will be considered as 1 different paper,
# in order to check if abstracts from same paper will be very similar
# 
# ==================================================


library(dplyr)
library(tidyverse)
library(stringi)
source("libs/my_functions_files.R")
source("libs/my_functions_translated_codes.R")
source("libs/my_functions_graphics.R")

options(digits = 2)
options(scipen = 999)


readRenviron("envs/abstracts__sample_less_common_langs.env")


ARTICLES_LESS_COMMON_LANGS_FILE_PATH <- Sys.getenv("ARTICLES_LESS_COMMON_LANGS_FILE_PATH")
ABSTRACT_WITH_ENGLISH_PRESENCE_CSV_FOLDER_PATH <- Sys.getenv("ABSTRACT_WITH_ENGLISH_PRESENCE_CSV_FOLDER_PATH")
ABSTRACTS_WITH_LESS_COMMON_LANGS_FILE_PATH <- Sys.getenv("ABSTRACTS_WITH_LESS_COMMON_LANGS_FILE_PATH")
PIDS_FILE_PATH <- Sys.getenv("PIDS_FILE_PATH")


if (file.exists(ARTICLES_LESS_COMMON_LANGS_FILE_PATH)) {
    ds_papers_less_common_langs <- read_csv_file(ARTICLES_LESS_COMMON_LANGS_FILE_PATH)
} else {
    ds <- read_csv_file(ABSTRACT_WITH_ENGLISH_PRESENCE_CSV_FOLDER_PATH) %>%
        filter(lang != 'zz')

    # select the articles which abstract languages is not in en, pt, es
    # the resulting dataset contains only the pid column, distinct
    ds_less_common_langs <- ds %>%
        filter(!(lang %in% c('en', 'es', 'pt', 'fr'))) %>%
        select(pid) %>%
        distinct() 
    print(nrow(ds_less_common_langs))

    # create a dataset which contains all the abstracts of the articles
    # which have abstracts in less common langs
    ds_papers_less_common_langs <- inner_join(ds_less_common_langs, ds, by='pid') %>%
        write_csv_file(ARTICLES_LESS_COMMON_LANGS_FILE_PATH)
}

langs <- ds_papers_less_common_langs %>%
    select(lang) %>% distinct()

print(nrow(langs))
# 13115

pids <- ds_papers_less_common_langs %>%
    select(pid) %>% distinct() %>%
    write_csv_file(PIDS_FILE_PATH)


print(nrow(pids))
# 3887
