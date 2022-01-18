# ==================================================
# 
# Análise do conjunto de dados que tem texto e idioma
# 
# ==================================================

source("texts_and_langs_functions.R")
source("graphics.R")


abstracts_lang_freq_graphic <- function(
                                langs_freq_file_path,
                                langs_with_codes_translated_file_path,
                                output_folder_path,
                                filename_prefix
                                ) {
    dir.create(output_folder_path, recursive=TRUE, showWarnings=FALSE)

    if (file.exists(langs_with_codes_translated_file_path)) {
        ds_langs_freq <- read_csv_file(langs_with_codes_translated_file_path)
    } else {
        # carrega a tabela de código e texto de idiomas
        ds_lang_codes_translated <- get_lang_code_translated()

        # lê o conjunto de dados de resumos, contém a freq de idioma em resumos
        ds_langs_freq <- read_csv_file(langs_freq_file_path)

        # adiciona a tradução do código do idioma
        ds <- add_lang_text(ds_langs_freq, ds_lang_codes_translated) %>%
            write_csv_file(file.path(output_folder_path, "abstracts_langs_freq_lang_codes_translated.csv"))

    }
    
    # gera um gráfico de barras com a quantidade de obs em cada idioma
    g <- sorted_hor_bar(ds, ds$lang_text, ds$n, ds$freq)
    #print(g)
    save_g("bla.pdf", g)
    save_g("bla.png", g)
    save_g("bla.svg", g)
}

readRenviron(".appenv")

ABSTRACTS_ANALYSIS_LANG_TRANSLATED_FILE_PATH <- Sys.getenv("ABSTRACTS_ANALYSIS_LANG_TRANSLATED_FILE_PATH")
ABSTRACTS_ANALYSIS_LANG_FILE_PATH <- Sys.getenv("ABSTRACTS_ANALYSIS_LANG_FILE_PATH")
ABSTRACTS_ANALYSIS_LANG_IN_AREA_FILE_PATH <- Sys.getenv("ABSTRACTS_ANALYSIS_LANG_IN_AREA_FILE_PATH")
ABSTRACTS_SOURCE_FILE_PATH <- Sys.getenv("ABSTRACTS_SOURCE_FILE_PATH")


ABSTRACTS_ANALYSIS_LANG_TRANSLATED_FILE_PATH


