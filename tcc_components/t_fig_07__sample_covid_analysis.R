# ==================================================
# 
# Figure 07 - COVID sample analysis
# 
# ==================================================
library(dplyr)
library(tidyverse)
library(stringi)
library(gridExtra)

source("libs/my_functions_files.R")
source("libs/my_functions_translated_codes.R")
source("libs/my_functions_graphics.R")

options(digits = 2)
options(scipen = 999)


draw_graphic <- function(ds_g, title, file_path, aspect_ratio, width, height) {
    
    don <- data.frame(
        x=ds_g$lang_text, 
        val=ds_g$perc_n_langs_in_area,
        grp=ds_g$subject_area,
        display=ds_g$display_numbers
        ) %>%
        arrange(val) %>%
        mutate(x = fct_reorder(x, val))

    g <- don %>%
        ggplot() +
        geom_segment( aes(x=x, xend=x, y=0, yend=val), color="black") +
        geom_point( aes(x=x, y=val, color=x), size=3 ) +
        geom_text(
            aes(x=x, y=val, label=display),
            hjust = -0.2, size = 5,
            position=position_dodge(width = 1)
        ) +
        coord_flip()+
        expand_limits(y = c(-0.5, max(don$val)+51)) +
        theme_bw() +
        theme(
            aspect.ratio=aspect_ratio,
            legend.position = "none",
            plot.margin=margin(0,0,0,0),
            panel.border = element_blank(),
            panel.spacing = unit(1, "lines"),
            text = element_text(size =18)
        ) +
        xlab("") +
        ylab("Porcentagem dos idiomas dos resumos") +
        facet_wrap(~grp, ncol=3)
    
    g1 <- save_g(g, paste(file_path, "svg", sep="."))

    g <- grid.arrange(g1, nrow = 1) %>%
        graphics(paste(GRAPHIC_FILE_PATH, "jpg", sep="."), width=width, height=height)


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
        select(subject_area, lang_text, pid) %>%
        group_by(subject_area) %>%
        mutate(total_in_area=n()) %>%
        ungroup() %>%
        group_by(subject_area, lang_text) %>%
        mutate(n_langs_in_area=n()) %>%
        write_csv_file(GRAPHIC_TMP_CSV_FILE_PATH)
    ds_g <- ds_g %>%
        mutate(perc_n_langs_in_area=as.numeric(n_langs_in_area * 100 / total_in_area)) %>%
        mutate(display_numbers=sprintf("%1.0f (%0.2f%%)", n_langs_in_area, perc_n_langs_in_area)) %>%
        select(subject_area, lang_text, perc_n_langs_in_area, display_numbers) %>%
        write_csv_file(GRAPHIC_CSV_FILE_PATH)
    print("numbers!!!!")
    print(head(ds_g))
}
print("numbers!!!!")
print(head(ds_g))
draw_graphic(
    ds_g, "title", GRAPHIC_FILE_PATH, 
    aspect_ratio=2.1/4, width=27, height=18
)

