# ==================================================
# 
# Análise do conjunto de dados que tem texto e idioma
# 
# ==================================================

# install.packages(hrbrthemes)
# library(hrbrthemes)

library(tools)
library(stringr)
library("ggplot2")
library("dplyr")
library("viridis")
library("forcats")


source("my_functions_files.R")
source("my_functions_graphics.R")


get_graphic <- function(ds_g, title, file_path) {
    
    g <- ds_g %>%
      # arrange(n_resumos_area_ano_idioma) %>%
      #mutate(lang_text=fct_reorder(lang_text, n_resumos_area_ano_idioma)) %>%

    ggplot(aes(time_range, lang_text, fill=n_resumos_area_ano_idioma)) + 
      geom_tile() +
      scale_fill_viridis(discrete=FALSE)

    save_g(g, paste(file_path, "pdf", sep="."))
    save_g(g, paste(file_path, "svg", sep="."))
}


readRenviron("envs/abstracts__area_lang_year.env")
SOURCE_CSV_FILE_PATH <- Sys.getenv("SOURCE_CSV_FILE_PATH")
GRAPHIC_FILENAME <- Sys.getenv("GRAPHIC_FILENAME3")

DS_SUBJECT_AREAS <- read_csv_file('subject_areas_pt.csv')

dir = dirname(SOURCE_CSV_FILE_PATH)
fname = file_path_sans_ext(basename(SOURCE_CSV_FILE_PATH))
OUTPUT_FOLDER_PATH = dirname(SOURCE_CSV_FILE_PATH)


for (area in DS_SUBJECT_AREAS$subject_area_pt) {

    area_fname <- str_replace(str_replace(area, " ", "_"), ",", "_")
    filename <- paste(paste(fname, area, sep="__"), "csv", sep=".")
    file_path <- file.path(dir, filename)

    g_file_path <- file.path(OUTPUT_FOLDER_PATH, paste(GRAPHIC_FILENAME, area_fname, sep="__"))

    ds_area <- read_csv_file(file_path) %>%
        get_graphic(area, g_file_path)
}
