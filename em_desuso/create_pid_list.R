# ==================================================
# 
# Utils
# 
# ==================================================


source("texts_and_langs_functions.R")


sort_by_year <- function(ds, folder_path) {
    print("Sort by year")
    ds <- ds %>%
          mutate(year=substr(pid, 11, 14)) %>%
          mutate(path=file.path(folder_path, year, substr(pid, 2, 10), pid)) %>%
          arrange(desc(year)) %>%
          select(c("path"))
    return (ds)
}


readRenviron(".appenv")


PID_SOURCE_CSV_FILE_PATH <- Sys.getenv("PID_SOURCE_CSV_FILE_PATH")
OUTPUT_FILE_PATH <- Sys.getenv("OUTPUT_FILE_PATH")
JSON_FOLDER_PATH <- Sys.getenv("JSON_FOLDER_PATH")

print(PID_SOURCE_CSV_FILE_PATH)
print(OUTPUT_FILE_PATH)
print(JSON_FOLDER_PATH)

if (PID_SOURCE_CSV_FILE_PATH != "" & OUTPUT_FILE_PATH != "" & JSON_FOLDER_PATH != "") {
    ds <- read_csv_file(PID_SOURCE_CSV_FILE_PATH) %>%
          sort_by_year(JSON_FOLDER_PATH) %>%
          write_csv_file(OUTPUT_FILE_PATH)
} else {
    print("Inform: PID_SOURCE_CSV_FILE_PATH and OUTPUT_FILE_PATH and JSON_FOLDER_PATH")

} 
