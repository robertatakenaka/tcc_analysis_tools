# ==================================================
# 
# Figures 02-05 Areas and Langs
# 
# ==================================================
source("my_functions__abstracts__lang_area.R")


readRenviron("envs/abstracts__lang_area.env")

SOURCE_CSV_FILE_PATH <- Sys.getenv("SOURCE_CSV_FILE_PATH")
SOURCE_WITH_LANG_TEXT_AND_SUBJECT_AREA_CSV_FILE_PATH <- Sys.getenv("SOURCE_WITH_LANG_TEXT_AND_SUBJECT_AREA_CSV_FILE_PATH")
GRAPHIC_CSV_FILE_PATH <- Sys.getenv("GRAPHIC_CSV_FILE_PATH")
GRAPHIC_TMP_CSV_FILE_PATH <- Sys.getenv("GRAPHIC_TMP_CSV_FILE_PATH")
GRAPHIC_FILE_PATH <- Sys.getenv("GRAPHIC_FILE_PATH")


# areas = c("Ciências Agrárias", "Ciências Exatas e da Terra", "Engenharias")
# get_subject_areas_graphic(
#     areas,
#     nrow=2,
#     arg_xlab="Idiomas dos resumos",
#     arg_ylab="Porcentagem dos idiomas dos resumos",
#     aspect_ratio=1/2
# )


# areas = c("Ciências Biológicas", "Ciências da Saúde")
# get_subject_areas_graphic(
#     areas,
#     nrow=1,
#     arg_xlab="Idiomas dos resumos",
#     arg_ylab="Porcentagem dos idiomas dos resumos",
#     aspect_ratio=1/2
# )


# areas = c("Ciências Humanas", "Ciências Sociais Aplicadas")
# get_subject_areas_graphic(
#     areas,
#     nrow=1,
#     arg_xlab="Idiomas dos resumos",
#     arg_ylab="Porcentagem dos idiomas dos resumos",
#     aspect_ratio=2/3
# )


areas = c("Linguística, Letras e Artes")
get_subject_areas_graphic(
    areas,
    nrow=2,
    arg_xlab="Idiomas dos resumos",
    arg_ylab="Porcentagem dos idiomas dos resumos",
    aspect_ratio=1/3
)
