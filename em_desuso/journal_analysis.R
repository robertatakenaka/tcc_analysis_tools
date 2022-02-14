# ==================================================
# 
# Análise do conjunto de dados que tem texto e idioma
# 
# ==================================================

source("texts_and_langs_functions.R")


# add_subject_areas <- function(ds, ds_journal_with_subject_areas) {
#     # O valor do issn_id corresponde a substr(ds$pid, 2, 10)
#     ds <- ds %>%
#             mutate(issn_id=issn) %>%
#             inner_join(ds_journal_with_subject_areas, by="issn_id")
#     return (ds)
# }


readRenviron(".appenv")

# CSV que contém colunas: issn (key), collection, área temática (value)
JOURNAL_AREAS_FILE_PATH <- Sys.getenv("JOURNAL_AREAS_FILE_PATH")
ds_subject_areas <- load_journal_subject_areas_ds(JOURNAL_AREAS_FILE_PATH)

DS_SOURCE_CSV_FILE_PATH <- Sys.getenv("DS_SOURCE_CSV_FILE_PATH")
DS_OUT_CSV_FILE_PATH <- Sys.getenv("DS_OUT_CSV_FILE_PATH")


ds <- read_csv_file(DS_SOURCE_CSV_FILE_PATH) %>%
      add_subject_area(ds_subject_areas) %>%
      write_csv_file(DS_OUT_CSV_FILE_PATH) %>%
      select(pid) %>% distinct()

print(ds)


