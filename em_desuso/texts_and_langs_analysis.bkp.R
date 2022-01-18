# ==================================================
# 
# Análise do conjunto de dados que tem texto e idioma
# 
# ==================================================


library("dplyr")

# inibe notação científica
options(digits = 2)
options(scipen = 999)


read_csv_file <- function(dataset_path) {
    # Lê os dados de um arquivo CSV
    print(dataset_path)
    ds <- read.csv(dataset_path, header=TRUE, sep=",")
    ds <- distinct(ds)
    return (ds)
}

write_csv_file <- function(dataset_path, ds) {
    print(dataset_path)
    path <- dirname(dataset_path)
    dir.create(path, recursive=TRUE, showWarnings=FALSE)
    write.csv(ds, dataset_path)
}


load_journal_subject_areas_ds <- function(ds_file_path) {
    # cria a coluna issn_id com o ID do periódico (ISSN ID) que cita a referencia

    # O valor do issn_id corresponde a substr(ds$pid, 2, 10)
    ds <- read_csv_file(ds_file_path) %>%
        rename("subject_area"="value", "issn_id"="key") %>%
        select(c("subject_area", "issn_id"))
    
    print(head(ds))
    return (ds)
}


add_subject_area <- function(ds, ds_journal_with_subject_areas) {
    # cria a coluna issn_id com o ID do periódico (ISSN ID) que cita a referencia

    # O valor do issn_id corresponde a substr(ds$pid, 2, 10)
    ds <- mutate(ds, issn_id=substr(ds$pid, 2, 10))

    # Adiciona ao dataset as áreas temáticas vinculadas ao `issn_id`
    # Cada `issn_id` (periódico) pode estar associado a um ou mais áreas temáticas
    ds <- inner_join(ds, ds_journal_with_subject_areas, by="issn_id")

    return (ds)
}

remove_empty_text <- function(ds) {
    print("re")
    ds <- filter(ds, !is.na(text))
    print("move")
    print(head(ds))
    return (ds)
}


evaluate_lang_freq <- function(ds) {
    print(head(ds))

    ds <- group_by(ds, lang) %>%
          summarise(n = n()) %>%
          mutate(freq = n/sum(n)) %>%
          ungroup() %>%
          arrange(desc(freq))
    return (ds)
}


evaluate_lang_freq_in_subject_area_by_year <- function(ds) {
    print(head(ds))

    ds <- group_by(ds, subject_area, pub_year, lang) %>%
          summarise(n = n()) %>%
          mutate(freq = n/sum(n)) %>%
          ungroup() %>%
          arrange(subject_area, pub_year, desc(freq))
    return (ds)
}


get_less_freq_than_x_percent <- function(ds, max_freq) {
    ds <- filter(ds, freq < max_freq) %>%
          mutate(freq = n/sum(n)) %>%
          arrange(desc(freq))
    return (ds)
}


create_freq_report <- function(input_file_path, ds_journal_with_subject_areas,
                               lang_freq_file_path,
                               lang_freq_in_subject_area_by_year_file_path,
                               less_freq_langs_file_path 
                               ) {
    ds <- read_csv_file(input_file_path) %>%
          # remove_empty_text() %>%
          add_subject_area(ds_journal_with_subject_areas)


    ds1 <- evaluate_lang_freq(ds)
    write_csv_file(lang_freq_file_path, ds1)

    ds2 <- get_less_freq_than_x_percent(ds1, 0.2)
    write_csv_file(less_freq_langs_file_path, ds2)

    ds3 <- evaluate_lang_freq_in_subject_area_by_year(ds)
    write_csv_file(lang_freq_in_subject_area_by_year_file_path, ds3)

    return (ds)
}

readRenviron(".appenv")

# CSV que contém colunas: issn (key), collection, área temática (value)
JOURNAL_AREAS_FILE_PATH <- Sys.getenv("JOURNAL_AREAS_FILE_PATH")
ds_journal_with_subject_areas <- load_journal_subject_areas_ds(JOURNAL_AREAS_FILE_PATH)

ABSTRACTS_SOURCE_CSV_FILE_PATH <- Sys.getenv("ABSTRACTS_SOURCE_CSV_FILE_PATH")
ABSTRACTS_ANALYSIS_LANG_FILE_PATH <- Sys.getenv("ABSTRACTS_ANALYSIS_LANG_FILE_PATH")
ABSTRACTS_ANALYSIS_LANG_IN_AREA_YEAR_FILE_PATH <- Sys.getenv("ABSTRACTS_ANALYSIS_LANG_IN_AREA_YEAR_FILE_PATH")
ABSTRACTS_ANALYSIS_LESS_FREQ_LANG_FILE_PATH <- Sys.getenv("ABSTRACTS_ANALYSIS_LESS_FREQ_LANG_FILE_PATH")

KEYWORDS_SOURCE_CSV_FILE_PATH <- Sys.getenv("KEYWORDS_SOURCE_CSV_FILE_PATH")
KEYWORDS_ANALYSIS_LANG_FILE_PATH <- Sys.getenv("KEYWORDS_ANALYSIS_LANG_FILE_PATH")
KEYWORDS_ANALYSIS_LANG_IN_AREA_YEAR_FILE_PATH <- Sys.getenv("KEYWORDS_ANALYSIS_LANG_IN_AREA_YEAR_FILE_PATH")
KEYWORDS_ANALYSIS_LESS_FREQ_LANG_FILE_PATH <- Sys.getenv("KEYWORDS_ANALYSIS_LESS_FREQ_LANG_FILE_PATH")

a <- create_freq_report(
    ABSTRACTS_SOURCE_CSV_FILE_PATH, ds_journal_with_subject_areas,
    ABSTRACTS_ANALYSIS_LANG_FILE_PATH,
    ABSTRACTS_ANALYSIS_LANG_IN_AREA_YEAR_FILE_PATH,
    ABSTRACTS_ANALYSIS_LESS_FREQ_LANG_FILE_PATH
)

k <- create_freq_report(
    KEYWORDS_SOURCE_CSV_FILE_PATH, ds_journal_with_subject_areas,
    KEYWORDS_ANALYSIS_LANG_FILE_PATH,
    KEYWORDS_ANALYSIS_LANG_IN_AREA_YEAR_FILE_PATH,
    KEYWORDS_ANALYSIS_LESS_FREQ_LANG_FILE_PATH
)

