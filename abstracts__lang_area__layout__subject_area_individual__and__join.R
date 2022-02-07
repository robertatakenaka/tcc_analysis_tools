# ==================================================
# 
# Gráficos de idiomas dos resumos
# 
# ==================================================

source("abstracts__lang_area__functions.R")


readRenviron("envs/abstracts__lang_area.env")

SOURCE_CSV_FILE_PATH <- Sys.getenv("SOURCE_CSV_FILE_PATH")
SOURCE_WITH_LANG_TEXT_AND_SUBJECT_AREA_CSV_FILE_PATH <- Sys.getenv("SOURCE_WITH_LANG_TEXT_AND_SUBJECT_AREA_CSV_FILE_PATH")
GRAPHIC_CSV_FILE_PATH <- Sys.getenv("GRAPHIC_CSV_FILE_PATH")
GRAPHIC_TMP_CSV_FILE_PATH <- Sys.getenv("GRAPHIC_TMP_CSV_FILE_PATH")
GRAPHIC_FILE_PATH <- Sys.getenv("GRAPHIC_FILE_PATH")


g1 <- get_subject_areas_graphic(
        c("Ciências Agrárias"),
        arg_xlab="Idiomas dos resumos")
g2 <- get_subject_areas_graphic(
        c("Ciências Exatas e da Terra"),
        arg_xlab="Idiomas dos resumos")
g3 <- get_subject_areas_graphic(
        c("Engenharias"),
        arg_xlab="Idiomas dos resumos",
        arg_ylab="Porcentagem dos idiomas dos resumos")

g4 <- get_subject_areas_graphic(
        c("Ciências Biológicas"),
        arg_xlab="Idiomas dos resumos")
g5 <- get_subject_areas_graphic(
        c("Ciências da Saúde"),
        arg_xlab="Idiomas dos resumos",
        arg_ylab="Porcentagem dos idiomas dos resumos")

g6 <- get_subject_areas_graphic(
        c("Ciências Humanas"),
        arg_xlab="Idiomas dos resumos")
g7 <- get_subject_areas_graphic(
        c("Ciências Sociais Aplicadas"),
        arg_xlab="Idiomas dos resumos")

g8 <- get_subject_areas_graphic(
        c("Linguística, Letras e Artes"),
        arg_xlab="Idiomas dos resumos", arg_ylab="Porcentagem dos idiomas dos resumos")


svg_file_path <- get_filename("g1_3", "svg")
svg(svg_file_path)
g1 + g2 + g3 + plot_layout(nrow = 3)
invisible(dev.off())

svg_file_path <- get_filename("g4_5", "svg")
svg(svg_file_path)
g4 + g5 + plot_layout(nrow = 2)
invisible(dev.off())

svg_file_path <- get_filename("g6_7", "svg")
svg(svg_file_path)
g6 + g7 + plot_layout(nrow = 2)
invisible(dev.off())

svg_file_path <- get_filename("g8", "svg")
svg(svg_file_path)
g8 + plot_layout(nrow = 2)
invisible(dev.off())
