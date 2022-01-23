# ==================================================
# 
# Análise do conjunto de dados que tem texto e idioma
# 
# ==================================================

source("texts_and_langs_functions.R")
source("graphics.R")


readRenviron(".appenv")

ABSTRACTS_ANALYSIS_LANG_TRANSLATED_FILE_PATH <- Sys.getenv("ABSTRACTS_ANALYSIS_LANG_TRANSLATED_FILE_PATH")
ABSTRACTS_ANALYSIS_LANG_FILE_PATH <- Sys.getenv("ABSTRACTS_ANALYSIS_LANG_FILE_PATH")
ABSTRACTS_ANALYSIS_LANG_IN_AREA_FILE_PATH <- Sys.getenv("ABSTRACTS_ANALYSIS_LANG_IN_AREA_FILE_PATH")
ABSTRACTS_SOURCE_CSV_FILE_PATH <- Sys.getenv("ABSTRACTS_SOURCE_CSV_FILE_PATH")

ABSTRACTS_ANALYSIS_PIDS_FILE_PATH <- Sys.getenv("ABSTRACTS_ANALYSIS_PIDS_FILE_PATH")

ABSTRACTS_ANALYSIS_ENGLISH_PRESENCE_FILE_PATH <- Sys.getenv("ABSTRACTS_ANALYSIS_ENGLISH_PRESENCE_FILE_PATH")


ds_english_presence <- read_csv_file(ABSTRACTS_ANALYSIS_ENGLISH_PRESENCE_FILE_PATH)


ds_g <- ds_english_presence %>%
    mutate(g_texts=paste(subject_areas, english_presence, sep=" | ")) %>%
    group_by(subject_areas) %>% mutate(total_by_subject_area=sum(total)) %>%
    ungroup()
print(head(ds_g))

texts <- ds_g$g_texts
numbers <- ds_g$total
perc <- ds_g$total / ds_g$total_by_subject_area
source("graphics.R")
g <- sorted_hor_bar(ds_g, texts, numbers, perc, v_text="", h_text="", reorder_by_text=TRUE)
print(g)

