# ==================================================
# 
# Figures 02-05 Areas and Langs
# 
# ==================================================
# source("libs/my_functions__abstracts__lang_area.R")

library(cowplot)
# library(dplyr)
# library(ggplot2)
library(gridExtra)
# library(hrbrthemes)
# library(stringi)
# library(tidyverse)
# library(viridis)

library(dplyr)
library(tidyverse)
library(stringi)
library(svglite)
library(patchwork)

source("libs/my_functions_files.R")
source("libs/my_functions_translated_codes.R")
source("libs/my_functions_graphics.R")

options(digits = 2)
options(scipen = 999)

# readRenviron("envs/abstracts__lang.env")
readRenviron("envs/abstracts__lang_area.env")

SOURCE_WITH_LANG_TEXT_CSV_FILE_PATH <- Sys.getenv("SOURCE_WITH_LANG_TEXT_CSV_FILE_PATH")
SOURCE_WITH_LANG_TEXT_AND_SUBJECT_AREA_CSV_FILE_PATH <- Sys.getenv("SOURCE_WITH_LANG_TEXT_AND_SUBJECT_AREA_CSV_FILE_PATH")
GRAPHIC_CSV_FILE_PATH <- Sys.getenv("GRAPHIC_CSV_FILE_PATH")
GRAPHIC_TMP_CSV_FILE_PATH <- Sys.getenv("GRAPHIC_TMP_CSV_FILE_PATH")
GRAPHIC_FILE_PATH <- Sys.getenv("GRAPHIC_FILE_PATH")


read_graphic_csv_file <- function() {
    if (file.exists(GRAPHIC_CSV_FILE_PATH)) {
        ds_g <- read_csv_file(GRAPHIC_CSV_FILE_PATH)
    } else {
        if (file.exists(SOURCE_WITH_LANG_TEXT_AND_SUBJECT_AREA_CSV_FILE_PATH)) {
            ds_with_lang_text_and_subject_area <- read_csv_file(SOURCE_WITH_LANG_TEXT_AND_SUBJECT_AREA_CSV_FILE_PATH)
        } else {
            ds_with_lang_text_and_subject_area <- read_csv_file(SOURCE_WITH_LANG_TEXT_CSV_FILE_PATH) %>%
                add_subject_area() %>%
                write_csv_file(SOURCE_WITH_LANG_TEXT_AND_SUBJECT_AREA_CSV_FILE_PATH)
        }
        ds_g <- ds_with_lang_text_and_subject_area %>%
            select(pid, subject_area, lang_text) %>%
            group_by(subject_area) %>%
            mutate(total_langs_in_area=n()) %>%
            ungroup() %>%
            group_by(subject_area, lang_text) %>%
            mutate(n_langs_in_area=n()) %>%
            ungroup() %>%
            select(subject_area, lang_text, total_langs_in_area, n_langs_in_area) %>%
            write_csv_file(GRAPHIC_TMP_CSV_FILE_PATH)
        ds_g <- ds_g %>%
            mutate(perc_n_langs_in_area=(n_langs_in_area * 100 / total_langs_in_area)) %>%
            mutate(display_numbers=sprintf("%1.0f (%0.2f%%)", n_langs_in_area, perc_n_langs_in_area)) %>%
            write_csv_file(GRAPHIC_CSV_FILE_PATH)
    }
    return (ds_g)
}


draw_graphic <- function(ds_g, title, file_path, nrow=1, arg_xlab="",
                         arg_ylab="", aspect_ratio=1,
                         width=10, height=10, file_type="jpg") {
    print(head(ds_g))
    don <- data.frame(
            langs=ds_g$lang_text, 
            val=as.numeric(ds_g$perc_n_langs_in_area),
            grp=ds_g$subject_area,
            display=ds_g$display_numbers
        ) %>%
        arrange(val) %>%
        mutate(langs=fct_reorder(langs, val))

    # With a bit more style
    g <- don %>%
  
        ggplot() +
        geom_segment( aes(x=langs, xend=langs, y=0, yend=val), color="black") +
        geom_point( aes(x=langs, y=val, color=langs), size=4 ) +
        geom_text(
            aes(x=langs, y=val, label=display),
            hjust = -0.2, size = 5,
            position=position_dodge(width = 1)
        ) +
        coord_flip() +
        expand_limits(y = c(-2, max(don$val)+30)) +
        xlab(arg_xlab) +
        ylab(arg_ylab) +
        theme_bw() +
        facet_wrap(~grp, nrow=nrow) +
        theme(
            aspect.ratio=aspect_ratio,
            legend.position = "none",
            panel.border = element_blank(),
            panel.spacing = unit(0.1, "lines"),
            # strip.text.x = element_text(size = 16),
            plot.margin=margin(0,0,0,0),
            text = element_text(size = 17)
        )
    # save_g(g, paste(file_path, "jpg", sep="."))
    #g <- grid.arrange(g, nrow = nrow) %>%
    if (file_type == 'svg') {
        print(paste(file_path, file_type, sep="."))
        save_g(g, paste(file_path, file_type, sep="."))

    } else {
        graphics(g, paste(file_path, file_type, sep="."), width=width, height=height)
    }
    
    
    return (g)
        
    # save_g(g, paste(file_path, "pdf", sep="."))
    # # save_g(g, paste(file_path, "svg", sep="."))
    
    # svg_file <- paste(file_path, "svg", sep=".")
    # svg(svg_file)
    # plot(g)
    # invisible(dev.off())
    # return (g)

}


get_filename <- function(area, ext="") {
    file_path = paste(
        GRAPHIC_FILE_PATH,
        str_replace(str_replace(area, ' ', '_'), ',', '_'), sep=""
    )
    if (ext != "") {
        file_path <- paste(file_path, ext, sep=".")
    }
    return (file_path)
}


get_subject_areas_graphic <- function(areas, nrow=1, arg_xlab="", arg_ylab="",
        aspect_ratio=1, width=20, height=10) {
    filename = str_c(areas, collapse = "_")
    partial_filename <- get_filename(filename)
    ds_g <- read_graphic_csv_file() %>%
        filter(subject_area %in% areas)

    if (length(areas) == 1) {
        print(head(ds_g))
        ds_g <- ds_g[order(as.numeric(ds_g$n_langs_in_area)),]
        print(ds_g)
        ds_g <- ds_g %>% 
            mutate(grp=case_when(
                        (n_langs_in_area < 20) ~ "2",
                        (n_langs_in_area >= 20) ~ "1"))

        g1 <- ds_g %>% 
            filter(grp == "1") %>%
            draw_graphic("title", paste(partial_filename, ds_g$grp[1], sep="_"),
                         nrow=nrow,
                         arg_xlab=arg_xlab, arg_ylab=arg_ylab,
                         aspect_ratio=aspect_ratio,
                         width=width,
                         height=height,
                         file_type="svg"
            )
        g2 <- ds_g %>% 
            filter(grp == "2") %>%
            draw_graphic("title", paste(partial_filename, ds_g$grp[1], sep="_"),
                         nrow=nrow,
                         arg_xlab=arg_xlab, arg_ylab=arg_ylab,
                         aspect_ratio=aspect_ratio,
                         width=width,
                         height=height,
                         file_type="svg"
            )
                
        g <- grid.arrange(g1, g2, nrow = 1) %>%
            graphics(paste(partial_filename, "jpg", sep="."), width, height)



    } else {
        ds_g <- ds_g %>%
            mutate(grp=subject_area) %>%
            draw_graphic("title", partial_filename, nrow=nrow,
                         arg_xlab=arg_xlab, arg_ylab=arg_ylab,
                         aspect_ratio=aspect_ratio,
                         width=width,
                         height=height
            )
    }
    return (ds_g)
}



# areas = c("Ciências Agrárias", "Ciências Exatas e da Terra", "Engenharias")
# g <- get_subject_areas_graphic(
#     areas,
#     nrow=2,
#     arg_xlab="Idiomas dos resumos",
#     arg_ylab="Porcentagem dos idiomas dos resumos",
#     aspect_ratio=1/4,
#     width=30, height=15
# )



# areas = c("Ciências Biológicas", "Ciências da Saúde")
# get_subject_areas_graphic(
#     areas,
#     nrow=1,
#     arg_xlab="Idiomas dos resumos",
#     arg_ylab="Porcentagem dos idiomas dos resumos",
#     aspect_ratio=1/2,
#     width=30, height=12
# )


areas = c("Ciências Humanas", "Ciências Sociais Aplicadas")
get_subject_areas_graphic(
    areas,
    nrow=1,
    arg_xlab="Idiomas dos resumos",
    arg_ylab="Porcentagem dos idiomas dos resumos",
    aspect_ratio=2.4/3,
    width=30, height=13

)


# areas = c("Linguística, Letras e Artes")
# get_subject_areas_graphic(
#     areas,
#     nrow=1,
#     arg_xlab="Idiomas dos resumos",
#     arg_ylab="Porcentagem dos idiomas dos resumos",
#     aspect_ratio=1/2,
#     width=28, height=9

# )
