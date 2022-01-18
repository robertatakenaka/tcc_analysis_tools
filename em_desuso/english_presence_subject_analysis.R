# ==================================================
# 
# Análise da presença do idioma inglês de resumos
# 
# ==================================================

library("dplyr")
library(stringr)

# inibe notação científica
options(digits = 2)
options(scipen = 999)


read_csv_file <- function(dataset_path) {
    # Lê os dados de um arquivo CSV
    print("Reading")
    print(dataset_path)
    ds <- read.csv(dataset_path, header=TRUE, sep=",") %>%
          distinct()
    return (ds)
}

write_csv_file <- function(ds, dataset_path) {
    print("Writing")
    print(dataset_path)
    path <- dirname(dataset_path)
    dir.create(path, recursive=TRUE, showWarnings=FALSE)
    write.csv(ds, dataset_path)
    return (ds)
}


fix_langs <- function(ds) {
    TMP_FILE_PATH = "tmp_fix_langs.csv"
    if (file.exists(TMP_FILE_PATH)) {
        ds <- (read_csv_file(TMP_FILE_PATH))
    } else {
        print("Fixing langs")
        ds$original_lang <- ds$lang
        fixed_ds <- ds %>%
                    mutate(lang=case_when(
                        (str_detect(str_to_lower(text), " the ") == TRUE) ~ "en",
                        (str_detect(str_to_lower(text), " the ") == FALSE) ~ original_lang,
                    )) %>%
                    mutate(fixed=(original_lang != lang))
        print(head(fixed_ds))
        write_csv_file(fixed_ds, TMP_FILE_PATH)
        ds <- fixed_ds
    }
    return (ds)
}

load_journal_subject_areas_ds <- function(ds_file_path) {
    # cria a coluna issn_id com o ID do periódico (ISSN ID) que cita a referencia

    TMP_FILE_PATH = "tmp_load_journal_subject_areas_ds.csv"
    if (file.exists(TMP_FILE_PATH)) {
        return (read_csv_file(TMP_FILE_PATH))
    } else {
        # O valor do issn_id corresponde a substr(ds$pid, 2, 10)
        ds <- read_csv_file(ds_file_path) %>%
            rename("subject_area"="value", "issn_id"="key") %>%
            select(c("subject_area", "issn_id"))
        
        print(head(ds))
        write_csv_file(ds, TMP_FILE_PATH)
        return (ds)
    }
}


add_subject_area <- function(ds, ds_journal_with_subject_areas) {
    # cria a coluna issn_id com o ID do periódico (ISSN ID) que cita a referencia
    TMP_FILE_PATH = "tmp_add_subject_area.csv"
    if (file.exists(TMP_FILE_PATH)) {
        ds <- read_csv_file(TMP_FILE_PATH)
    } else {

        # O valor do issn_id corresponde a substr(ds$pid, 2, 10)
        ds <- mutate(ds, issn_id=substr(ds$pid, 2, 10))

        # Adiciona ao dataset as áreas temáticas vinculadas ao `issn_id`
        # Cada `issn_id` (periódico) pode estar associado a um ou mais áreas temáticas
        ds <- inner_join(ds, ds_journal_with_subject_areas, by="issn_id")

        write_csv_file(ds, TMP_FILE_PATH)
    }
    print(head(ds))
    return (ds)
}


mark_english_presence <- function(ds) {   
    TMP_FILE_PATH = "tmp_mark_english_presence.csv"
    if (file.exists(TMP_FILE_PATH)) {
        ds <- read_csv_file(TMP_FILE_PATH)
    } else {
        ds <- mutate(ds, english=case_when(lang == 'en' ~ 1,
                                            TRUE ~ 0)) %>%
            mutate(not_english=case_when(lang != 'en' ~ 1,
                                            TRUE ~ 0))
        write_csv_file(ds, TMP_FILE_PATH)
    }
    print(head(ds))
    return (ds)
}


classify_docs <- function(ds) {   

    TMP_FILE_PATH = "tmp_group_by_pid.csv"
    if (file.exists(TMP_FILE_PATH)) {
        ds <- read_csv_file(TMP_FILE_PATH)
    } else {
        ds <- ds %>%
          group_by(pid, subject_area)
        write_csv_file(ds, TMP_FILE_PATH)
    }
    print(head(ds))
    
    TMP_FILE_PATH = "tmp_summarise_total_not_english.csv"
    if (file.exists(TMP_FILE_PATH)) {
        ds <- read_csv_file(TMP_FILE_PATH)
    } else {
        ds <- ds %>%
          mutate(total_not_english=sum(not_english),
                    total_english=sum(english))
        write_csv_file(ds, TMP_FILE_PATH)
    }
    print(head(ds))

    # TMP_FILE_PATH = "tmp_ungroup.csv"
    # if (file.exists(TMP_FILE_PATH)) {
    #     ds <- read_csv_file(TMP_FILE_PATH)
    # } else {
    #     ds <- ds %>%
    #       ungroup()
    #     write_csv_file(ds, TMP_FILE_PATH)
    # }
    # print(head(ds))

    TMP_FILE_PATH = "tmp_type.csv"
    if (file.exists(TMP_FILE_PATH)) {
        ds <- read_csv_file(TMP_FILE_PATH)
    } else {
        ds <- ds %>%
          mutate(type=case_when(
              total_english > 0 & total_not_english == 0 ~ "english_only",
              total_english > 0 & total_not_english > 0 ~ "multilingue_with_english",
              total_english == 0 & total_not_english > 0 ~ "no_english",
              TRUE ~ "unknown"
          ))
        write_csv_file(ds, TMP_FILE_PATH)
    }
    print(head(ds))

    TMP_FILE_PATH = "tmp_select_pid_type_subject_area_pub_year.csv"
    if (file.exists(TMP_FILE_PATH)) {
        ds <- read_csv_file(TMP_FILE_PATH)
    } else {
        ds <- ds %>%
          select(c("pid", "type", "subject_area", "pub_year"))
        write_csv_file(ds, TMP_FILE_PATH)
    }      
    print(head(ds))
    return (ds)
}


# create_files <- function(ds, folder_path) {
#     TMP_FILE_PATH = "tmp_create_files.csv"
#     if (file.exists(TMP_FILE_PATH)) {
#         ds <- read_csv_file(TMP_FILE_PATH)
#     } else {
#         ds <- group_by(ds, subject_area, pub_year, type) %>%
#               mutate(name=paste(paste(subject_area, pub_year, type, sep="_"), "csv", sep="."))
#         write_csv_file(ds, TMP_FILE_PATH)
#     }      
#     print(head(ds))

#     ds_names <- ds %>%
#              select(c('name')) %>%
#              distinct()

#     for (name in ds_names$name) {
#         file_path <- file.path(folder_path, name)
#         print(file_path)
#         items <- ds
#         items <- items %>%
#                  filter(name==name) %>%
#                  select(c("pid")) %>%
#                  distinct() %>%
#                  write_csv_file(file_path)
#         print(name)
#         print(head(items))
#     }
#     return (TRUE)
# }

create_files <- function(ds, folder_path) {
    TMP_FILE_PATH = "tmp_xcreate_files.csv"
    if (file.exists(TMP_FILE_PATH)) {
        ds <- read_csv_file(TMP_FILE_PATH)
    } else {
        ds <- group_by(ds, subject_area, pub_year, type) %>%
              mutate(name=paste(paste(subject_area, pub_year, type, sep="_"), "csv", sep="."))
        write_csv_file(ds, TMP_FILE_PATH)
    }      
    print(head(ds))

    ds_names <- ds %>%
             select(c('name')) %>%
             distinct()

    for (name in ds_names$name) {
        file_path <- file.path(folder_path, name)
        print(file_path)
        items <- ds
        items <- items %>%
                 filter(name==name) %>%
                 select(c("pid")) %>%
                 distinct() %>%
                 write_csv_file(file_path)
        print(name)
        print(head(items))
    }
    return (TRUE)
}
create_reports <- function(input_file_path, ds_journal_with_subject_areas, folder_path) {
    ds <- read_csv_file(input_file_path) %>%
          # remove_empty_text() %>%
          fix_langs()
    print("del original")
    # ds$original <- NULL
    # print("del text")
    ds$text <- NULL
    print("add_subject_area")
    # ds <- add_subject_area(ds, ds_journal_with_subject_areas) %>%
    #       mark_english_presence()  %>%
    #       classify_docs()

    ds <- mark_english_presence(ds)  %>%
          classify_docs()
    create_files(ds, folder_path)
    return (TRUE)
}

readRenviron(".appenv")

# CSV que contém colunas: issn (key), collection, área temática (value)
JOURNAL_AREAS_FILE_PATH <- Sys.getenv("JOURNAL_AREAS_FILE_PATH")
ds_journal_with_subject_areas <- load_journal_subject_areas_ds(JOURNAL_AREAS_FILE_PATH)

ABSTRACTS_SOURCE_CSV_FILE_PATH <- Sys.getenv("ABSTRACTS_SOURCE_CSV_FILE_PATH")
ANALYSIS_ENGLISH_PRESENCE_FOLDER_PATH <- Sys.getenv("ANALYSIS_ENGLISH_PRESENCE_FOLDER_PATH")

print(ABSTRACTS_SOURCE_CSV_FILE_PATH)
a <- create_reports(
    ABSTRACTS_SOURCE_CSV_FILE_PATH, ds_journal_with_subject_areas,
    ANALYSIS_ENGLISH_PRESENCE_FOLDER_PATH
)




