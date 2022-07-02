# ==================================================
# 
# Análise do conjunto de dados que tem texto e idioma
# 
# ==================================================

#install.packages("dplyr")
library("dplyr")
library("stringr")

# inibe notação científica
options(digits = 2)
options(scipen = 999)


fix_langs <- function(ds) {
    print("Fixing langs")
    ds$original_lang <- ds$lang
    fixed_ds <- ds %>%
                mutate(lang=case_when(
                    (str_detect(str_to_lower(text), " the ") == TRUE) ~ "en",
                    (str_detect(str_to_lower(text), " the ") == FALSE) ~ original_lang,
                )) %>%
                mutate(fixed=(original_lang != lang))
    print(head(fixed_ds))
    ds <- fixed_ds
    return (ds)
}


evaluate_lang_freq <- function(ds) {
    print(">>> evaluate_lang_freq()")
    print(head(ds))
    ds <- ds %>%
          group_by(lang) %>%
          summarise(n = n()) %>%
          mutate(freq = n/sum(n)) %>%
          ungroup() %>%
          arrange(desc(freq))
    return (ds)
}


evaluate_lang_freq_in_subject_area <- function(ds) {
    print(">>> evaluate_lang_freq_in_subject_area()")
    print(head(ds))
    ds <- group_by(ds, subject_area, lang) %>%
          summarise(n = n()) %>%
          mutate(freq = n/sum(n)) %>%
          ungroup() %>%
          arrange(subject_area, desc(freq))
    return (ds)
}



mark_english_presence <- function(ds) {   
    print(">>> mark_english_presence()")

    ds <- ds %>%
        mutate(english=case_when(lang == 'en' ~ 1,
                                        TRUE ~ 0)) %>%
        mutate(not_english=case_when(lang != 'en' ~ 1,
                                        TRUE ~ 0))
    print(head(ds))
    return (ds)
}


classify_docs <- function(ds) {   
    print(">>> classify_docs()")

    ds <- ds %>%
        group_by(pid) %>%
        mutate(total_not_english=sum(not_english),
                total_english=sum(english)) %>%
        mutate(type=case_when(
            total_english > 0 & total_not_english == 0 ~ "en",
            total_english > 0 & total_not_english > 0 ~ "en_xx",
            total_english == 0 & total_not_english > 0 ~ "xx",
            TRUE ~ "unknown"
        )) %>%
        ungroup()
    print(head(ds))
    return (ds)
}



add_lang_text <- function(ds) {
    translations <- read_csv_file('./datasets/LANGS.csv')
    ds <- inner_join(ds, translations, by="lang")
    ds[is.na(ds)] <- 0
    return (ds)
}


