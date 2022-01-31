# ==================================================
# 
# Análise do conjunto de dados que tem texto e idioma
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


draw_graphic <- function(ds_g, title, file_path) {
    
    # Create data (could be way easier but it's late)
    # value1 <- abs(rnorm(6))*2
    don <- data.frame(
      x=ds_g$lang_text, 
      val=ds_g$perc_n_langs_in_area,
      grp=ds_g$subject_area,
      display=ds_g$display_numbers
      ) %>%
      arrange(val)

    # With a bit more style
    g <- don %>%
        ggplot() +
        geom_segment( aes(x=x, xend=x, y=0, yend=val), color="black") +
        geom_point( aes(x=x, y=val, color=x), size=3 ) +
        geom_text(
            aes(x=x, y=val, label=display),
            hjust = -0.2, size = 2.5,
            position=position_dodge(width = 1)
        ) +
        coord_flip()+
        expand_limits(y = c(-2, max(don$val)+10)) +
        theme(
            legend.position = "none",
            panel.border = element_blank(),
            panel.spacing = unit(0.1, "lines"),
            strip.text.x = element_text(size = 8)
        ) +
        xlab("Idiomas dos resumos") +
        ylab("Porcentagem dos idiomas dos resumos") +
        #theme_void() +
        facet_wrap(~grp, ncol=2, scale="free_x")
    
    save_g(g, paste(file_path, "pdf", sep="."))
    # save_g(g, paste(file_path, "svg", sep="."))
    save_g(g, paste(file_path, "svg", sep="."))
}

readRenviron("envs/sample_covid.env")

SOURCE_CSV_FILE_PATH <- Sys.getenv("SOURCE_CSV_FILE_PATH")
SOURCE_WITH_LANG_TEXT_AND_SUBJECT_AREA_CSV_FILE_PATH <- Sys.getenv("SOURCE_WITH_LANG_TEXT_AND_SUBJECT_AREA_CSV_FILE_PATH")
GRAPHIC_CSV_FILE_PATH <- Sys.getenv("GRAPHIC_CSV_FILE_PATH")
GRAPHIC_TMP_CSV_FILE_PATH <- Sys.getenv("GRAPHIC_TMP_CSV_FILE_PATH")
GRAPHIC_FILE_PATH <- Sys.getenv("GRAPHIC_FILE_PATH")


if (file.exists(GRAPHIC_CSV_FILE_PATH)) {
    ds_g <- read_csv_file(GRAPHIC_CSV_FILE_PATH)
} else {
    if (file.exists(SOURCE_WITH_LANG_TEXT_AND_SUBJECT_AREA_CSV_FILE_PATH)) {
        ds_sample <- read_csv_file(SOURCE_WITH_LANG_TEXT_AND_SUBJECT_AREA_CSV_FILE_PATH)
    } else {
        ds_sample <- read_csv_file(SOURCE_CSV_FILE_PATH) %>%
            add_lang_text() %>%
            add_subject_area() %>%
            write_csv_file(SOURCE_WITH_LANG_TEXT_AND_SUBJECT_AREA_CSV_FILE_PATH)
    }
    ds_g <- ds_sample %>%
        mutate(total=n()) %>%
        group_by(lang_text) %>%
        mutate(n_langs=n()) %>%
        mutate(perc_n_langs_in_total=n_langs*100/total) %>%
        ungroup() %>%
        group_by(subject_area) %>%
        mutate(total_in_area=n()) %>%
        ungroup() %>%
        group_by(subject_area, lang_text) %>%
        mutate(n_langs_in_area=n()) %>%
        mutate(perc_n_langs_in_area=as.numeric(n_langs_in_area * 100 / total_in_area)) %>%
        mutate(display_numbers=sprintf("%1.0f (%0.2f%%)", n_langs_in_area, perc_n_langs_in_area)) %>%
            write_csv_file(GRAPHIC_TMP_CSV_FILE_PATH)
    ds_g <- ds_g %>%
        select(subject_area, lang_text, perc_n_langs_in_area, display_numbers) %>%
            write_csv_file(GRAPHIC_CSV_FILE_PATH)
}
print("numbers!!!!")
print(head(ds_g))

DS_SUBJECT_AREAS <- read_csv_file('subject_areas_pt.csv')
for (area in DS_SUBJECT_AREAS$subject_area_pt) {
    file_path = paste(
        GRAPHIC_FILE_PATH,
        str_replace(str_replace(area, ' ', '_'), ',', '_'), sep=""
    )
    ds_partial <- ds_g %>%
        filter(subject_area==area) %>%

        draw_graphic("title", file_path)

}

