# ==================================================
# 
# Análise da presença do idioma inglês de resumos
# 
# ==================================================


source("abstracts__lang_area__functions.R")


get_ds_marked_with_english_presence <- function() {
    if (file.exists(ARTICLE_WITH_ENGLISH_PRESENCE_CSV_FILE_PATH)) {
        ds_article_with_english_presence_mark <- read_csv_file(ARTICLE_WITH_ENGLISH_PRESENCE_CSV_FILE_PATH)
    } else {
        if (file.exists(SOURCE_WITH_LANG_TEXT_AND_SUBJECT_AREA_CSV_FILE_PATH)) {
            ds <- read_csv_file(SOURCE_WITH_LANG_TEXT_AND_SUBJECT_AREA_CSV_FILE_PATH)
        } else {
            ds <- read_csv_file(SOURCE_CSV_FILE_PATH) %>%
                add_lang_text() %>%
                add_subject_area() %>%
                write_csv_file(SOURCE_WITH_LANG_TEXT_AND_SUBJECT_AREA_CSV_FILE_PATH)
        }
        ds_article_with_english_presence_mark <- ds %>%
            select(pid, subject_area, lang) %>%
            mark_english_presence_in_abstract() %>%
            mark_english_presence_in_article()
    }
    return (ds_article_with_english_presence_mark)
}


mark_english_presence_in_abstract <- function(ds) {   
    if (file.exists(SOURCE_WITH_ENGLISH_PRESENCE_CSV_FILE_PATH)) {
        ds <- read_csv_file(SOURCE_WITH_ENGLISH_PRESENCE_CSV_FILE_PATH)
    } else {
        ds <- ds %>%
            mutate(english=case_when(lang == 'en' ~ 1, TRUE ~ 0)) %>%
            mutate(not_english=case_when(lang != 'en' ~ 1, TRUE ~ 0)) %>%
            write_csv_file(SOURCE_WITH_ENGLISH_PRESENCE_CSV_FILE_PATH)
    }
    return (ds)
}


mark_english_presence_in_article <- function(ds) {   
    if (file.exists(ARTICLE_WITH_ENGLISH_PRESENCE_CSV_FILE_PATH)) {
        ds <- read_csv_file(ARTICLE_WITH_ENGLISH_PRESENCE_CSV_FILE_PATH)
    } else {
        ds <- ds %>%
            group_by(pid, subject_area) %>%
            summarise(
                total_not_english=sum(not_english),
                total_english=sum(english)) %>%
            mutate(classification=case_when(
                total_english == 1 & total_not_english == 0 ~ "english_only",
                total_english == 0 & total_not_english > 0 ~ "no_english",
                TRUE ~ "multilingue_with_english"
            )) %>%
            write_csv_file(ARTICLE_WITH_ENGLISH_PRESENCE_CSV_FILE_PATH)
    }
    return (ds)
}


create_area_and_classification_files <- function(ds) {        
    folder_path <- dirname(ARTICLE_WITH_ENGLISH_PRESENCE_CSV_FILE_PATH)
    DS_SUBJECT_AREAS <- read_csv_file('subject_areas_pt.csv')
    
    for (area in DS_SUBJECT_AREAS$subject_area_pt) {
        for (x in c('english_only', 'no_english', 'multilingue_with_english')) {
            file_path <- file.path(folder_path, paste(paste(area, x, sep="_"), "csv", sep="."))
            if (!file.exists(file_path)) {
                ds_partial <- ds %>%
                    dplyr::filter(ds$classification==x & ds$subject_area==area) %>%
                    write_csv_file(file_path)
            }
        }
    }
}


draw_graphic <- function(ds_g, title, file_path) {
    
    don <- data.frame(
      x=ds_g$classification, 
      val=ds_g$perc*100,
      grp=ds_g$subject_area,
      display=ds_g$display
      ) %>%
      arrange(val) %>%
      mutate(x = fct_reorder(x, val))

    g <- don %>%
        ggplot() +
        geom_segment( aes(x=x, xend=x, y=0, yend=val), color="black") +
        geom_point( aes(x=x, y=val, color=x), size=3 ) +
        geom_text(
            aes(x=x, y=val, label=display),
            hjust = -0.2, size = 2.5,
            position=position_dodge(width = 1)
        ) +
        coord_flip()+
        expand_limits(y = c(-0.5, don$val+50)) +
        theme_bw() +
        xlab("") +
        ylab("Porcentagem de artigos") +
        facet_wrap(~grp, ncol=2) +
        theme(
            aspect.ratio=1/3,
            legend.position = "none",
            plot.margin=margin(2,2,2,2),
            panel.border = element_blank(),
            panel.spacing = unit(1, "lines"),
            strip.text.x = element_text(size = 9)
        )
    
    #save_g(g, paste(file_path, "pdf", sep="."))
    save_g(g, paste(file_path, "svg", sep="."))
    
}

readRenviron("envs/abstracts__english_presence.env")


SOURCE_CSV_FILE_PATH <- Sys.getenv("SOURCE_CSV_FILE_PATH")
SOURCE_WITH_LANG_TEXT_AND_SUBJECT_AREA_CSV_FILE_PATH <- Sys.getenv("SOURCE_WITH_LANG_TEXT_AND_SUBJECT_AREA_CSV_FILE_PATH")
SOURCE_WITH_ENGLISH_PRESENCE_CSV_FILE_PATH <- Sys.getenv("SOURCE_WITH_ENGLISH_PRESENCE_CSV_FILE_PATH")
ARTICLE_WITH_ENGLISH_PRESENCE_CSV_FILE_PATH <- Sys.getenv("ARTICLE_WITH_ENGLISH_PRESENCE_CSV_FILE_PATH")

GRAPHIC_CSV_FILE_PATH <- Sys.getenv("GRAPHIC_CSV_FILE_PATH")
NO_ENGLISH <- Sys.getenv("NO_ENGLISH")
GRAPHIC_FILE_PATH <- Sys.getenv("GRAPHIC_FILE_PATH")


if (file.exists(GRAPHIC_CSV_FILE_PATH)) {
    ds_graphic <- read_csv_file(GRAPHIC_CSV_FILE_PATH)
} else {
    ds <- get_ds_marked_with_english_presence()

    create_area_and_classification_files(ds)
    ds %>%
        dplyr::filter(classification=='no_english') %>%
        write_csv_file(NO_ENGLISH)
    ds_graphic <- ds %>%
        select(subject_area, pid, classification) %>%
        group_by(subject_area, classification) %>%
        summarise(n=n()) %>%
        mutate(total=sum(n)) %>%
        mutate(perc=n / total) %>%
        mutate(display=sprintf("%1.0f (%0.2f%%)", n, n * 100 / total)) %>%
        mutate(classification=case_when(
            classification == 'no_english' ~ 'resumo em inglês ausente',
            classification == 'english_only' ~ 'somente resumo em inglês',
            classification == 'multilingue_with_english' ~ 'resumo em inglês + outro(s)',
        )) %>%
        write_csv_file(GRAPHIC_CSV_FILE_PATH)
}
draw_graphic(ds_graphic, "", GRAPHIC_FILE_PATH)




