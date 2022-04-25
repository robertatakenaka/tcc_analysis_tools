# ==================================================
# 
# Missing Abstract Text
# 
# ==================================================


library(dplyr)
library(tidyverse)
library(stringi)
source("libs/my_functions_files.R")

options(digits = 2)
options(scipen = 999)


readRenviron("envs/abstracts__lang.env")

SOURCE_WITH_LANG_TEXT_AND_SUBJECT_AREA_CSV_FILE_PATH <- Sys.getenv("SOURCE_WITH_LANG_TEXT_AND_SUBJECT_AREA_CSV_FILE_PATH")

ds_sample <- read_csv_file(SOURCE_WITH_LANG_TEXT_AND_SUBJECT_AREA_CSV_FILE_PATH)


ds_zh <- ds_sample %>%
    filter(lang == "zh" | lang == "ru") %>%
    write_csv_file("zh_ru.csv")
print(nrow(ds_zh))
print(".........")

ds_missing <- ds_sample %>%
    filter(text == "") %>%
    write_csv_file("missing_abstracts.csv")
