# ==================================================
# 
# Prep
# 
# ==================================================


library(dplyr)
# library(tidyverse)
# library(stringi)
source("libs/my_functions_files.R")
# source("my_functions_translated_codes.R")


readRenviron("envs/results_pares.env")

PIDS_FILE_PATH <- Sys.getenv("PIDS_FILE_PATH")
ds_pids <- read_csv_file(PIDS_FILE_PATH) %>%
    mutate(issn_id=substr(pid, 2, 10)) %>%
    left_join(read_csv_file("journals_areas_pt.csv")) %>%
    write_csv_file("pids_with_subject_area.csv")
