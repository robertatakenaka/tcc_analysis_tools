# ==================================================
# 
# Análise do conjunto de dados que tem texto e idioma
# 
# ==================================================

#install.packages("dplyr")
library("dplyr")
library("stringr")


add_subject_area <- function(ds) {
    # O valor do issn_id corresponde a substr(ds$pid, 2, 10)
    ds_journal_with_subject_areas <- read_csv_file('./datasets/journals_areas_pt.csv')

    # Adiciona ao dataset as áreas temáticas vinculadas ao `issn_id`
    # Cada `issn_id` (periódico) pode estar associado a uma ou mais áreas temáticas
    ds <- ds %>%
        mutate(issn_id=substr(pid, 2, 10)) %>%
        inner_join(ds_journal_with_subject_areas, by="issn_id")
    ds[is.na(ds)] <- "UNKNOWN"
    print("Added subject areas")
    print(head(ds))
    return (ds)
}


add_lang_text <- function(ds) {
    translations <- read_csv_file('./datasets/LANGS.csv')
    ds <- inner_join(ds, translations, by="lang")
    ds[is.na(ds)] <- "UNKNOWN"
    print("Added lang texts")
    print(head(ds))
    return (ds)
}
