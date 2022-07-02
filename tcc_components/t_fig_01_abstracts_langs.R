# ==================================================
# 
# Figura 1. Quantidade de resumos por idioma
# 
# ==================================================



library(cowplot)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(hrbrthemes)
library(stringi)
library(tidyverse)
library(viridis)

source("libs/my_functions_files.R")
source("libs/my_functions_translated_codes.R")
source("libs/my_functions_graphics.R")

options(digits = 2)
options(scipen = 999)


draw_graphic <- function(ds_g, title, file_path, d) {
    
    # Create data (could be way easier but it's late)
    # value1 <- abs(rnorm(6))*2
    don <- data.frame(
        x=ds_g$lang_text, 
        val=ds_g$perc_n_langs_in_total,
        display=ds_g$display_numbers
      ) %>%
      arrange(val) %>%
      mutate(x = fct_reorder(x, val))

    # With a bit more style
    g <- don %>%
        ggplot() +
        #scale_y_continuous(trans='log10') +
        geom_segment( aes(x=x, xend=x, y=0, yend=val), color="black") +
        geom_point( aes(x=x, y=val, color=x), size=1 ) +
        geom_text(
            aes(x=x, y=val, label=display),
            size=3.5,
            hjust = -0.2, size = 2.5,
            position=position_dodge(width = 1)
        ) +
        coord_flip()+
        expand_limits(y = c(-0.1, don$val+d)) +
        theme_bw() +
        theme(
            legend.position = "none",
            aspect.ratio=2/3,
            plot.margin=margin(2,2,2,2),
            panel.border = element_blank(),
            panel.spacing = unit(0.1, "lines"),
            #strip.text.x = element_text(size = 12),
            axis.ticks = element_line(colour = "grey70", size = 0.1),
            text = element_text(size = 12)
        ) +
        xlab("") +
        ylab("Porcentagem dos idiomas dos resumos")
    
    #save_g(g, paste(file_path, "pdf", sep="."))
    save_g(g, paste(file_path, "svg", sep="."))
    return (g)
    
}

readRenviron("envs/abstracts__lang.env")

SOURCE_CSV_FILE_PATH <- Sys.getenv("SOURCE_CSV_FILE_PATH")
SOURCE_WITH_LANG_TEXT_CSV_FILE_PATH <- Sys.getenv("SOURCE_WITH_LANG_TEXT_CSV_FILE_PATH")
GRAPHIC_CSV_FILE_PATH <- Sys.getenv("GRAPHIC_CSV_FILE_PATH")
GRAPHIC_TMP_CSV_FILE_PATH <- Sys.getenv("GRAPHIC_TMP_CSV_FILE_PATH")
GRAPHIC_FILE_PATH <- Sys.getenv("GRAPHIC_FILE_PATH")
G1_FILE_PATH <- Sys.getenv("G1_FILE_PATH")
G2_FILE_PATH <- Sys.getenv("G2_FILE_PATH")


if (file.exists(GRAPHIC_CSV_FILE_PATH)) {
    ds_g <- read_csv_file(GRAPHIC_CSV_FILE_PATH)
} else {
    if (file.exists(SOURCE_WITH_LANG_TEXT_CSV_FILE_PATH)) {
        ds_with_lang_text <- read_csv_file(SOURCE_WITH_LANG_TEXT_CSV_FILE_PATH)
    } else {
        ds_with_lang_text <- read_csv_file(SOURCE_CSV_FILE_PATH) %>%
            add_lang_text() %>%
            write_csv_file(SOURCE_WITH_LANG_TEXT_CSV_FILE_PATH)
    }
    ds_pid_and_lang_text <- ds_with_lang_text %>%
        select(pid, lang_text) %>%
        unique()
    print("ds_pid_and_lang_text")
    print(nrow(ds_pid_and_lang_text))

    ds_pid <- ds_pid_and_lang_text %>%
        select(pid) %>%
        unique()
    print("ds_pid")
    print(nrow(ds_pid))

    ds_g <- ds_pid_and_lang_text %>%
        mutate(total=n()) %>%
        group_by(lang_text) %>%
        mutate(n_langs=n()) %>%
        mutate(perc_n_langs_in_total=n_langs*100/total) %>%
        ungroup() %>%
        mutate(display_numbers=sprintf("%1.0f (%0.2f%%)", n_langs, perc_n_langs_in_total)) %>%
            write_csv_file(GRAPHIC_TMP_CSV_FILE_PATH)

    ds_g <- ds_g %>%
        select(lang_text, n_langs, perc_n_langs_in_total, display_numbers) %>%
        mutate(total=n())
    ds_g <- ds_g[order(ds_g$perc_n_langs_in_total),]
    write_csv_file(ds_g, GRAPHIC_CSV_FILE_PATH)
}


# draw_graphic(ds_g, "title", GRAPHIC_FILE_PATH)

print(ds_g)
g1 <- ds_g %>% 
    mutate(total = n()) %>%
    filter(X < total/2) %>%
    draw_graphic("title", G1_FILE_PATH, 2)
g2 <- ds_g %>% 
    mutate(total = n()) %>%
    filter(X >= total/2) %>%
    draw_graphic("title", G2_FILE_PATH, 40)
        
g <- grid.arrange(g2, g1, nrow = 1) %>%
    graphics(paste(GRAPHIC_FILE_PATH, "jpg", sep="."), width=20, height=7)


