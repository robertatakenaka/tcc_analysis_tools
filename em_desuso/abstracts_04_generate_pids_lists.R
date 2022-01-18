library("dplyr")

read_csv_file <- function(dataset_path) {
  print(dataset_path)
  ds <- read.csv(dataset_path, header=TRUE, sep=",") %>% 
    distinct()
  return (ds)
}


create_pids_list_file <- function(ds, folder_path, filename) {
    ds <- select(ds, c('pid'))
    dir.create(folder_path, recursive=TRUE, showWarnings=FALSE)
    file_path = file.path(folder_path, paste("pid", filename, sep="_"))
    write.csv(ds, file_path)
    return (ds)
}


readRenviron(".appenv")
ABSTRACTS_ANALYSIS_FOLDER_PATH <- Sys.getenv("ABSTRACTS_ANALYSIS_FOLDER_PATH")
RS_INPUT_LISTS_PATH <- Sys.getenv("RS_INPUT_LISTS_PATH")
files = c(
    'Agricultural Sciences.abstracts_and_one_is_in_english.csv',
    'Agricultural Sciences.abstracts_not_english.csv',
    'Agricultural Sciences.abstracts_only_in_english.csv',
    'Applied Social Sciences.abstracts_and_one_is_in_english.csv',
    'Applied Social Sciences.abstracts_not_english.csv',
    'Applied Social Sciences.abstracts_only_in_english.csv',
    'Biological Sciences.abstracts_and_one_is_in_english.csv',
    'Biological Sciences.abstracts_not_english.csv',
    'Biological Sciences.abstracts_only_in_english.csv',
    'Engineering.abstracts_and_one_is_in_english.csv',
    'Engineering.abstracts_not_english.csv',
    'Engineering.abstracts_only_in_english.csv',
    'Exact and Earth Sciences.abstracts_and_one_is_in_english.csv',
    'Exact and Earth Sciences.abstracts_not_english.csv',
    'Exact and Earth Sciences.abstracts_only_in_english.csv',
    'Health Sciences.abstracts_and_one_is_in_english.csv',
    'Health Sciences.abstracts_not_english.csv',
    'Health Sciences.abstracts_only_in_english.csv',
    'Human Sciences.abstracts_and_one_is_in_english.csv',
    'Human Sciences.abstracts_not_english.csv',
    'Human Sciences.abstracts_only_in_english.csv',
    'Linguistics, Letters and Arts.abstracts_and_one_is_in_english.csv',
    'Linguistics, Letters and Arts.abstracts_not_english.csv',
    'Linguistics, Letters and Arts.abstracts_only_in_english.csv'
)

for (filename in files) {
    ds <- read_csv_file(file.path(ABSTRACTS_ANALYSIS_FOLDER_PATH, filename))
    create_pids_list_file(ds, RS_INPUT_LISTS_PATH, filename)
}



