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


read_csv_file <- function(dataset_path) {
    # Lê os dados de um arquivo CSV
    print("Reading ...")
    print(dataset_path)
    ds <- read.csv(dataset_path, header=TRUE, sep=",") %>%
            distinct()
    print("done")
    return (ds)
}


write_csv_file <- function(ds, dataset_path) {
    print(dataset_path)
    path <- dirname(dataset_path)
    dir.create(path, recursive=TRUE, showWarnings=FALSE)
    write.csv(ds, dataset_path)
    return (ds)
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
    # O valor do issn_id corresponde a substr(ds$pid, 2, 10)
    ds <- mutate(ds, issn_id=substr(ds$pid, 2, 10))

    # Adiciona ao dataset as áreas temáticas vinculadas ao `issn_id`
    # Cada `issn_id` (periódico) pode estar associado a um ou mais áreas temáticas
    ds <- inner_join(ds, ds_journal_with_subject_areas, by="issn_id")

    return (ds)
}


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


add_filename_use_type <- function(ds) {
    print(">>> add_filename_use_type()")

    ds <- ds %>%
        mutate(filename=paste(type, 'csv', sep='.'))

    print(head(ds))
    return (ds)
}


add_filename_use_type_and_subj_area <- function(ds) {
    print(">>> add_filename_use_type_and_subj_area()")

    ds <- ds %>%
        mutate(filename=paste(subject_area, type, 'csv', sep='.'))

    print(head(ds))
    return (ds)
}


create_file <- function(ds, file_path, name) {
    print("")
    print("")
    print(">>> create_file()")

    filenames = c(name, "_")
    items <- ds %>%
             filter(filename %in% filenames) %>%
             select('pid') %>%
             distinct() %>%
             write_csv_file(file_path)
    return (items)
}

create_files <- function(ds, folder_path) {
    print(">>> create_files()")

    print(head(ds))
    ds_names <- ds %>%
             select(c('filename')) %>%
             distinct()
    
    print(head(ds_names))

    for (filename in ds_names$filename) {
        file_path <- file.path(folder_path, filename)
        print("-> ds")
        print(head(ds))
        create_file(ds, file_path, filename)
    }
    return (TRUE)
}


add_lang_text <- function(ds) {
    translations <- read_csv_file('LANGS.csv')
    ds <- inner_join(ds, translations, by="lang")
    ds[is.na(ds)] <- 0
    return (ds)
}


create_pid_list <- function(ds, file_path="") {
    pids <- ds %>%
            mutate(correct=case_when(str_length(pid) == 23 ~ TRUE,
                                         TRUE ~ FALSE)) %>%
            filter(correct == TRUE) %>%
            select(pid) %>%
            distinct()
    if (file_path != "") {
        write_csv_file(pids, file_path)
    }
    print(paste("Total ", nrow(pids)))
    return (pids)
}
