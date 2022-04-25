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


filtered <- function(ds_pids, ds_refs, refs_filtered_by_pid_path) {
    ds_pids %>%
        left_join(ds_refs, by="pid") %>%
        write_csv_file(filtered_refs_path)
}


add_ref_pid <- function(src_refs_path, refs_with_pid_path) {
    if (file.exists(refs_with_pid_path)) {
        ds <- read_csv_file(refs_with_pid_path)
    } else {
        ds <- read_csv_file(src_refs_path) %>%
            mutate(ref_pid=pid, pid=substr(ref_pid, 1, 23)) %>%
            write_csv_file(refs_with_pid_path)
    }
    return (ds)
}

readRenviron("envs/results_pares.env")

PIDS_FILE_PATH <- Sys.getenv("PIDS_FILE_PATH")
REFS1_WITH_PID_AND_REF_PID_FILE_PATH <- Sys.getenv("REFS1_WITH_PID_AND_REF_PID_FILE_PATH")
REFS_SRC1_FILE_PATH <- Sys.getenv("REFS_SRC1_FILE_PATH")


ds_refs <- add_ref_pid(REFS_SRC1_FILE_PATH, REFS1_WITH_PID_AND_REF_PID_FILE_PATH)
ds_pids <- read_csv_file(PIDS_FILE_PATH)
filtered(ds_pids, ds_refs)

    

# ABSTRACTS_FILE_PATH <- Sys.getenv("ABSTRACTS_FILE_PATH")
# ds_all <- read_csv_file(ABSTRACTS_FILE_PATH)
# ds_pids %>%
#     left_join(ds_all) %>%
#     write_csv_file("filtered_abstracts.csv")

