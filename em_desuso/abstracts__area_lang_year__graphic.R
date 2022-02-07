# ==================================================
# 
# Análise do conjunto de dados que tem texto e idioma
# 
# ==================================================

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
        mutate(lang_text=fct_reorder(lang_text, n_resumos_area_ano_idioma)) %>%
        ggplot() +
        geom_bar(
            width=0.1,
            aes(x=lang_text, y=n_resumos_area_ano_idioma+100, fill=lang_text),
            stat="identity", 
            position=position_dodge(width = 0.5),
        ) +
        geom_text(
            aes(x=lang_text, y=n_resumos_area_ano_idioma+100, label=display_numbers),
            hjust = -0.1, size = 0.8,
            position=position_dodge(width = 0.5)
        ) +
        # scale_fill_manual(
        #     name=NULL,
        #     breaks=g_lang_text,
        #     values=PALETTE
        # ) +
        scale_fill_viridis(discrete = TRUE) +
        coord_flip() +
        facet_grid(time_range ~ .) +
        xlab("") +
        ylab(title) + 
        theme_linedraw()
        # expand_limits(y = c(-10, max(ds_g$n_resumos_area_ano_idioma+100)*1.4)) +
        # theme(panel.grid.minor.y = element_line(color = "red",
        #                                       size = 2,
        #                                       linetype = 2))
    
    
    save_g(g, paste(file_path, "png", sep="."))
    save_g(g, paste(file_path, "pdf", sep="."))
    save_g(g, paste(file_path, "svg", sep="."))
}


readRenviron("envs/abstracts__area_lang_year.env")
SOURCE_CSV_FILE_PATH <- Sys.getenv("SOURCE_CSV_FILE_PATH")
GRAPHIC_FILENAME <- Sys.getenv("GRAPHIC_FILENAME")

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
