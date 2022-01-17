############################################################################
#
#  Identifica a frequência dos idiomas em resumos, títulos e palavras-chaves
#
############################################################################

library("tidyverse")

options(digits = 2)
options(scipen = 999)
# specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))


read_ds <- function(dataset_path) {
    print(dataset_path)
    ds <- read.csv(dataset_path, header=TRUE, sep=",") %>% distinct()
    return (ds)
}

evaluate_freq <- function(ds) {
    ds <- group_by(ds, year, lang) %>%
          summarise(n = n()) %>%
          mutate(freq = n/sum(n)) %>%
          arrange(year, desc(freq))
    return (ds)
}


# evaluate_freq <- function(ds) {
#     ds_grouped_by_year <- group_by(ds, year)
    
#     ds_with_freq <- ds_grouped_by_year
#     print(head(ds_grouped_by_year))

#     print("freq de idioma")
#     ds_with_freq <- data.frame(table(ds_grouped_by_year$lang))
#     print(head(ds_with_freq))
    
#     print("remove freq=0")
#     ds_with_freq <- filter(ds_with_freq, Freq>0)
#     print(head(ds_with_freq))
    
#     print("add total_obs")
#     ds_with_freq <- mutate(ds_with_freq, total_obs=sum(Freq))
#     print(head(ds_with_freq))
    
#     print("add perc")
#     ds_with_freq <- mutate(ds_with_freq, perc=Freq/total_obs)
#     print(head(ds_with_freq))
    
#     print("ungroup")
#     ds_with_freq <- ungroup(ds_with_freq) 
#     print(head(ds_with_freq))
    
#     print("arrange")
#     ds_with_freq <- arrange(ds_with_freq, year)
#     print("arrange")
#     ds_with_freq <- arrange(ds_with_freq, desc(Freq))
#     print(head(ds_with_freq))
#     return (ds_with_freq)
# }


# evaluate_freq <- function(ds) {
#   ds_with_freq <- data.frame(table(ds$year, ds$lang))
#   head(ds_with_freq)
#   total_obs <- sum(ds_with_freq$Freq)
#   ds_with_freq <- mutate(ds_with_freq, perc=Freq*100/total_obs) %>%
#                   arrange(desc(Var1, Freq)) %>% filter(Freq>0)
#   return (ds_with_freq)
# }

get_enough_data <- function(ds) {
  ds <- mutate(ds, year=substr(ds$pid, 11, 14))
  ds$text <- NULL
  ds$collection <- NULL
  ds$pid <- NULL

  return (ds)
}


readRenviron(".appenv")
 # CSV_FOLDER_PATH = "/PATH/csv/"
CSV_FOLDER_PATH <- Sys.getenv("CSV_FOLDER_PATH")
ANALYSIS_LANGS_FOLDER_PATH <- Sys.getenv("ANALYSIS_LANGS_FOLDER_PATH")

FILENAMES <- list("abstracts", "article_titles", "keywords")
for(filename in FILENAMES){
    ds_path = file.path(CSV_FOLDER_PATH, filename)
    ds_path_csv <- paste(ds_path, "csv", sep=".")

    ds <- read_ds(ds_path_csv)

    ds_year <- get_enough_data(ds)

    ds_langs <- evaluate_freq(ds_year)

    new_name = paste(filename, "_year_lang_freq.csv", sep="_")
    new_file <- file.path(ANALYSIS_LANGS_FOLDER_PATH, new_name)
    print(new_file)
    write.csv(ds_langs, new_file)

}

