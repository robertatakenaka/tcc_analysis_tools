# ==================================================
# 
# Análise do conjunto de dados que tem texto e idioma
# 
# ==================================================

source("my_functions_files.R")
library(tools)
library("stringr")

# inibe notação científica
options(digits = 2)
options(scipen = 999)


add_numbers <- function(ds_abstracts) {
    ds <- ds_abstracts %>%
        group_by(subject_area, time_range, lang_text) %>%
        mutate(n_resumos_area_ano_idioma=n()) %>%
        mutate(total_area_ano=sum(n_resumos_area_ano_idioma)) %>%
        mutate(display_numbers=sprintf("%1.0f (%0.2f%%)", n_resumos_area_ano_idioma, (100*n_resumos_area_ano_idioma)/total_area_ano)) %>%
        ungroup()
    return (ds)
}


save_datasets_splitted_by_area <- function(ds_file_path) {
    DS_SUBJECT_AREAS <- read_csv_file('subject_areas_pt.csv')
    dir = dirname(ds_file_path)
    fname = file_path_sans_ext(basename(ds_file_path))

    ds_complete <- read_csv_file(ds_file_path)

    for (area in DS_SUBJECT_AREAS$subject_area_pt) {

        area_fname <- str_replace(str_replace(area, " ", "_"), ",", "_")
        filename <- paste(paste(fname, area, sep="__"), "csv", sep=".")
        file_path <- file.path(dir, filename)

        ds_area <- ds_complete %>%
                filter(subject_area==area) %>%
                add_numbers() %>%
                write_csv_file(file_path)
    }
}


readRenviron("envs/abstracts__area_lang_year.env")

SOURCE_CSV_FILE_PATH <- Sys.getenv("SOURCE_CSV_FILE_PATH")
save_datasets_splitted_by_area(SOURCE_CSV_FILE_PATH)
