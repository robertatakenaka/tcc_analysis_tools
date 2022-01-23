# ==================================================
# 
# Análise do conjunto de dados que tem texto e idioma
# 
# ==================================================

source("texts_and_langs_functions.R")
source("graphics.R")


translate_data <- function(langs_freq_file_path, langs_with_codes_translated_file_path) {
    output_folder_path = dirname(langs_with_codes_translated_file_path)
    dir.create(output_folder_path, recursive=TRUE, showWarnings=FALSE)

    if (file.exists(langs_with_codes_translated_file_path)) {
        ds_langs_freq <- read_csv_file(langs_with_codes_translated_file_path)
    } else {
        # lê o conjunto de dados de resumos, contém a freq de idioma em resumos
        ds_langs_freq <- read_csv_file(langs_freq_file_path)  %>%
                         add_lang_text() %>%
                         write_csv_file(langs_with_codes_translated_file_path)
    }
    return (ds_langs_freq)
}


draw_graphic <- function(ds_g, title, file_path) {
    
    g <- ds_g %>%
        mutate(lang_text = fct_reorder(lang_text, n)) %>%
        ggplot() +
        geom_bar(
            width=0.5,
            aes(x=lang_text, y=n+5999, fill=lang_text),
            stat="identity", 
            position=position_dodge(width = 1),
        ) +
        geom_text(
            aes(x=lang_text, y=n+5999, label=display_numbers),
            hjust = -0.1, size = 2.8,
            position=position_dodge(width = 1)
        ) +
        # scale_fill_manual(
        #     name=NULL,
        #     breaks=g_lang_text,
        #     values=PALETTE
        # ) +
        scale_fill_viridis(discrete = TRUE) +
        coord_flip() +
        xlab("") +
        ylab(title) + 
        theme_linedraw() +
        expand_limits(y = c(-10, max(ds_g$n+5999)*1.4)) +
        theme(panel.grid.minor.y = element_line(color = "red",
                                              size = 0.5,
                                              linetype = 2))
    
    
    save_g(g, paste(file_path, "pdf", sep="."))
    save_g(g, paste(file_path, "svg", sep="."))
}

readRenviron(".appenv")

ABSTRACTS_ANALYSIS_LANG_IN_AREA_TRANSLATED_FILE_PATH <- Sys.getenv("ABSTRACTS_ANALYSIS_LANG_IN_AREA_TRANSLATED_FILE_PATH")
ABSTRACTS_ANALYSIS_LANG_IN_AREA_FILE_PATH <- Sys.getenv("ABSTRACTS_ANALYSIS_LANG_IN_AREA_FILE_PATH")

# ABSTRACTS_SOURCE_CSV_FILE_PATH <- Sys.getenv("ABSTRACTS_SOURCE_CSV_FILE_PATH")

# ABSTRACTS_ANALYSIS_PIDS_FILE_PATH <- Sys.getenv("ABSTRACTS_ANALYSIS_PIDS_FILE_PATH")

# ABSTRACTS_ANALYSIS_ENGLISH_PRESENCE_FILE_PATH <- Sys.getenv("ABSTRACTS_ANALYSIS_ENGLISH_PRESENCE_FILE_PATH")


output_folder_path = dirname(ABSTRACTS_ANALYSIS_LANG_IN_AREA_FILE_PATH)
filename_prefix = "abstracts__langs_freq_area_gxxx"
ds <- translate_data(
    ABSTRACTS_ANALYSIS_LANG_IN_AREA_FILE_PATH,
    ABSTRACTS_ANALYSIS_LANG_IN_AREA_TRANSLATED_FILE_PATH
)

ds_g <- ds %>%
    mutate(display_numbers = sprintf("%1.0f (%0.2f%%)", n, freq*100))

g_subject_areas <- ds_g %>%
                   select(subject_area) %>%
                   distinct()

for (area in g_subject_areas$subject_area) {
    print(area)
    ds_subject_area <- ds_g %>%
        filter(subject_area==area)
    print(head(ds_subject_area))
    area_name <- str_replace(str_replace(area, " ", "_"), ",", "_")
    print(area_name)
    file_path <- file.path(output_folder_path, paste(filename_prefix, area_name, sep="_"))
    print(file_path)
    draw_graphic(ds_subject_area, area, file_path)
}



