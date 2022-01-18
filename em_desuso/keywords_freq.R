############################################################################
#
#  Identifica as frequência das palavras-chaves do dataset, com e sem
#  os idiomas
#
############################################################################

library("tidyverse")
library("stringi")

options(digits = 2)
options(scipen = 999)
# specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))


read_csv_file <- function(dataset_path) {
  print(dataset_path)
  ds <- read.csv(dataset_path, header=TRUE, sep=",") %>% 
    distinct()
  return (ds)
}

get_freq_ds <- function(table_ds_column, file_path) {
  ds_col <- data.frame(table_ds_column)
  total_obs = sum(ds_col$Freq)
  ds_col <- (mutate(ds_col, total=total_obs, freq_perc=Freq/total_obs) %>% arrange(desc(Freq)))
  print(head(ds_col))
  write.csv(ds_col, file_path)
  
  return (ds_col)
}


readRenviron(".appenv")
KEYWORDS_SOURCE_CSV_PATH <- Sys.getenv("KEYWORDS_SOURCE_CSV_PATH")
KEYWORDS_ANALYSIS_FOLDER_PATH <- Sys.getenv("KEYWORDS_ANALYSIS_FOLDER_PATH")

ds_keywords = read_csv_file(KEYWORDS_SOURCE_CSV_PATH)

# arquivo com resultado da freq de palavras-chaves, sem correção
original_freq_ds_file_path <- file.path(KEYWORDS_ANALYSIS_FOLDER_PATH, "keywords_original_freq.csv")
# obtenção da freq das palavras-chaves sem correção
original_freq_ds <- get_freq_ds(table(ds_keywords$original), original_freq_ds_file_path)

# arquivo com resultado da freq da freq de palavras-chaves, sem correção
freq_original_freq_ds_file_path <- file.path(KEYWORDS_ANALYSIS_FOLDER_PATH, "freq_keywords_original_freq.csv")
# obtenção da freq de freq das palavras-chaves sem correção
freq_original_freq_ds <- get_freq_ds(table(original_freq_ds$Freq), freq_original_freq_ds_file_path)


# arquivo com resultado da freq de palavras-chaves corrigidas
fixed_freq_ds_file_path <- file.path(KEYWORDS_ANALYSIS_FOLDER_PATH, "keywords_fixed_freq.csv")
# obtenção da freq das palavras-chaves corrigidas
fixed_freq_ds <- get_freq_ds(table(ds_keywords$text), fixed_freq_ds_file_path)

# arquivo com resultado da freq da freq de palavras-chaves corrigidas
freq_fixed_freq_ds_file_path <- file.path(KEYWORDS_ANALYSIS_FOLDER_PATH, "freq_keywords_fixed_freq.csv")
# obtenção da freq de freq das palavras-chaves corrigidas
freq_fixed_freq_ds <- get_freq_ds(table(fixed_freq_ds$Freq), freq_fixed_freq_ds_file_path)


# arquivo com resultado da freq de palavras-chaves corrigidas
fixed_freq_ds_file_path <- file.path(KEYWORDS_ANALYSIS_FOLDER_PATH, "keywords_fixed_and_lang_freq.csv")
# obtenção da freq das palavras-chaves corrigidas
fixed_freq_ds <- get_freq_ds(table(ds_keywords$lang, ds_keywords$text), fixed_freq_ds_file_path)

# arquivo com resultado da freq da freq de palavras-chaves corrigidas
freq_fixed_freq_ds_file_path <- file.path(KEYWORDS_ANALYSIS_FOLDER_PATH, "freq_keywords_fixed_and_lang_freq.csv")
# obtenção da freq de freq das palavras-chaves corrigidas
freq_fixed_freq_ds <- get_freq_ds(table(fixed_freq_ds$Freq), freq_fixed_freq_ds_file_path)

