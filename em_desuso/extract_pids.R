# ==================================================
# 
# Análise do conjunto de dados que tem texto e idioma
# 
# ==================================================

source("texts_and_langs_functions.R")

readRenviron(".appenv")

EXTRACT_PIDS__ABSTRACTS_SOURCE_CSV_FILE_PATH <- Sys.getenv("EXTRACT_PIDS__ABSTRACTS_SOURCE_CSV_FILE_PATH")

EXTRACT_PIDS__OUTPUT_PIDS_FILE_PATH <- Sys.getenv("EXTRACT_PIDS__OUTPUT_PIDS_FILE_PATH")

EXTRACT_PIDS__PIDS_ENGLISH_ABSENT_FILE_PATH <- Sys.getenv("EXTRACT_PIDS__PIDS_ENGLISH_ABSENT_FILE_PATH")


ds_abstracts_pids <- read_csv_file(EXTRACT_PIDS__ABSTRACTS_SOURCE_CSV_FILE_PATH) %>%
    select(pid)


ds_pids_input <- read_csv_file(EXTRACT_PIDS__PIDS_ENGLISH_ABSENT_FILE_PATH)

output_dataset <- ds_abstracts_pids %>%
  bind_rows(ds_pids_input) %>%
    write_csv_file(EXTRACT_PIDS__OUTPUT_PIDS_FILE_PATH)

