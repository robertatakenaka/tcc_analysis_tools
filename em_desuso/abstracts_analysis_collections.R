# ==================================================
# 
# Análise do conjunto de dados que tem texto e idioma
# 
# ==================================================

source("my_functions_files.R")

readRenviron("envs/collections.env")

INPUT <- Sys.getenv("INPUT")
OUTPUT_FILE_PATH <- Sys.getenv("OUTPUT_FILE_PATH")


ds <- read_csv_file(INPUT) %>%
        select(collection) %>%
        write_csv_file(OUTPUT_FILE_PATH)
