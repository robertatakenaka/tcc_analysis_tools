# ==================================================
# 
# Análise do conjunto de dados que tem texto e idioma
# 
# ==================================================

source("texts_and_langs_functions.R")
source("graphics.R")


abstracts_lang_freq_graphic <- function(langs_freq_file_path, langs_with_codes_translated_file_path, output_folder_path, filename_prefix) {
    dir.create(output_folder_path, recursive=TRUE, showWarnings=FALSE)

    if (file.exists(langs_with_codes_translated_file_path)) {
        ds_langs_freq <- read_csv_file(langs_with_codes_translated_file_path)
    } else {
        # lê o conjunto de dados de resumos, contém a freq de idioma em resumos
        ds_langs_freq <- read_csv_file(langs_freq_file_path)  %>%
                         add_lang_text() %>%
                         write_csv_file(langs_with_codes_translated_file_path)
    }
    ds_langs_freq$total = sum(ds_langs_freq$n)
    text = paste(
        "Quantidade de resumos por idioma | Total de", ds_langs_freq$total[1], "resumos"
    )
    # gera um gráfico de barras com a quantidade de obs em cada idioma
    g <- sorted_hor_bar(ds_langs_freq, ds_langs_freq$lang_text,
                        ds_langs_freq$n, ds_langs_freq$freq,
                        h_text=text) %>%
        save_g(file.path(output_folder_path, paste(filename_prefix, "png", sep="."))) %>%
        save_g(file.path(output_folder_path, paste(filename_prefix, "svg", sep="."))) %>%
        save_g(file.path(output_folder_path, paste(filename_prefix, "pdf", sep=".")))
    return (g)
}

readRenviron(".appenv")

ABSTRACTS_ANALYSIS_LANG_TRANSLATED_FILE_PATH <- Sys.getenv("ABSTRACTS_ANALYSIS_LANG_TRANSLATED_FILE_PATH")
ABSTRACTS_ANALYSIS_LANG_FILE_PATH <- Sys.getenv("ABSTRACTS_ANALYSIS_LANG_FILE_PATH")
ABSTRACTS_ANALYSIS_LANG_IN_AREA_FILE_PATH <- Sys.getenv("ABSTRACTS_ANALYSIS_LANG_IN_AREA_FILE_PATH")
ABSTRACTS_SOURCE_CSV_FILE_PATH <- Sys.getenv("ABSTRACTS_SOURCE_CSV_FILE_PATH")

ABSTRACTS_ANALYSIS_PIDS_FILE_PATH <- Sys.getenv("ABSTRACTS_ANALYSIS_PIDS_FILE_PATH")

ABSTRACTS_ANALYSIS_ENGLISH_PRESENCE_FILE_PATH <- Sys.getenv("ABSTRACTS_ANALYSIS_ENGLISH_PRESENCE_FILE_PATH")


output_folder_path = dirname(ABSTRACTS_ANALYSIS_LANG_FILE_PATH)
filename_prefix = "abstracts__langs_freq_g"
abstracts_lang_freq_graphic(
    ABSTRACTS_ANALYSIS_LANG_FILE_PATH,
    ABSTRACTS_ANALYSIS_LANG_TRANSLATED_FILE_PATH,
    output_folder_path,
    filename_prefix
)

