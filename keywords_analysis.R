# ==================================================
# 
# Análise do conjunto de dados que tem texto e idioma
# 
# ==================================================


source("texts_and_langs_functions.R")


get_freq_keywords <- function(ds) {
    print("Get keywords freq")
    ds <- ds %>%
          group_by(text) %>%
          summarise(n = n()) %>%
          mutate(total=sum(n), freq = n/sum(n)) %>%
          ungroup() %>%
          arrange(desc(freq))
    return (ds)
}


get_freq_keywords_with_lang <- function(ds) {
    print("Get keywords freq by year")
    ds <- ds %>%
          group_by(text, lang) %>%
          summarise(n = n()) %>%
          #mutate(freq = n/sum(n)) %>%
          ungroup() %>%
          arrange(desc(n))
    return (ds)
}


get_freq_keywords_by_year <- function(ds) {
    print("Get keywords freq by year")
    ds <- ds %>%
          group_by(text, pub_year) %>%
          summarise(n = n()) %>%
          #mutate(freq = n/sum(n)) %>%
          ungroup() %>%
          arrange(desc(n))
    return (ds)
}


get_first_rows <- function(ds, first_n_items) {
    ds <- head(ds, n=first_n_items)
    return (ds)
}


get_rows_with_one_ocurrence <- function(ds) {
    ds <- ds %>%
          filter(n == 1)
    return (ds)
}



# get_freq_keywords_with_lang_by_year <- function(ds) {
#     print("Get keywords freq with lang by year")
#     ds <- ds %>%
#           group_by(pub_year, text, lang) %>%
#           summarise(n = n()) %>%
#           mutate(freq = n/sum(n)) %>%
#           ungroup() %>%
#           arrange(desc(freq))
#     return (ds)
# }


get_freq_keywords_in_subject_area_by_year <- function(ds) {
    print("Get keywords freq with lang by year by subject_area")
    ds <- ds %>%
          group_by(text, pub_year, subject_area) %>%
          summarise(n = n()) %>%
          ungroup() %>%
          arrange(desc(n))
    return (ds)
}


explore_keywords <- function(ds) {
    print("Explore keywords")
    ds <- ds %>%
          group_by(text, pub_year, subject_area) %>%
          summarise(n = n()) %>%
          ungroup() %>%
          arrange(desc(n))
    return (ds)
}

readRenviron(".appenv")

# CSV que contém colunas: issn (key), collection, área temática (value)
JOURNAL_AREAS_FILE_PATH <- Sys.getenv("JOURNAL_AREAS_FILE_PATH")
ds_journal_with_subject_areas <- load_journal_subject_areas_ds(JOURNAL_AREAS_FILE_PATH)

KEYWORDS_SOURCE_CSV_FILE_PATH <- Sys.getenv("KEYWORDS_SOURCE_CSV_FILE_PATH")
KEYWORDS_ANALYSIS_TEXT_FILE_PATH <- Sys.getenv("KEYWORDS_ANALYSIS_TEXT_FILE_PATH")
KEYWORDS_ANALYSIS_TEXT_MORE_FREQ_FILE_PATH <- Sys.getenv("KEYWORDS_ANALYSIS_TEXT_MORE_FREQ_FILE_PATH")
KEYWORDS_ANALYSIS_TEXT_ONCE_FILE_PATH <- Sys.getenv("KEYWORDS_ANALYSIS_TEXT_ONCE_FILE_PATH")

KEYWORDS_ANALYSIS_TEXT_IN_AREA_FILE_PATH <- Sys.getenv("KEYWORDS_ANALYSIS_TEXT_IN_AREA_FILE_PATH")

KEYWORDS_ANALYSIS_TEXT_XXX_FILE_PATH <- Sys.getenv("KEYWORDS_ANALYSIS_TEXT_XXX_FILE_PATH")
KEYWORDS_ANALYSIS_TEXT_XXXX_FILE_PATH <- Sys.getenv("KEYWORDS_ANALYSIS_TEXT_XXXX_FILE_PATH")


ds <- read_csv_file(KEYWORDS_SOURCE_CSV_FILE_PATH)



ds6 <- ds %>%
      get_freq_keywords_with_lang() %>%
      write_csv_file(KEYWORDS_ANALYSIS_TEXT_XXXX_FILE_PATH)


# ds_with_subject_areas <- ds %>%
#     add_subject_area(ds_journal_with_subject_areas)

# ds1 <- ds_with_subject_areas %>%
#       get_freq_keywords_in_subject_area_by_year() %>%
#       write_csv_file(KEYWORDS_ANALYSIS_TEXT_IN_AREA_FILE_PATH)

# ds2 <- ds %>%
#       get_freq_keywords() %>%
#       write_csv_file(KEYWORDS_ANALYSIS_TEXT_FILE_PATH)

# ds3 <- ds2 %>%
#       get_first_rows(2000) %>%
#       write_csv_file(KEYWORDS_ANALYSIS_TEXT_MORE_FREQ_FILE_PATH)

# ds4 <- ds2 %>%
#       get_rows_with_one_ocurrence() %>%
#       write_csv_file(KEYWORDS_ANALYSIS_TEXT_ONCE_FILE_PATH)



# ds5 <- ds_with_subject_areas %>%
#       explore_keywords() %>%
#       write_csv_file(KEYWORDS_ANALYSIS_TEXT_XXX_FILE_PATH)
