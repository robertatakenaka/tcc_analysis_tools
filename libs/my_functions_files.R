# ==================================================
# 
# Minhas funções para ler e escrever csv
# 
# ==================================================

library("dplyr")


read_csv_file <- function(dataset_path) {
    print(paste("Reading:", dataset_path))
    ds <- read.csv(dataset_path, header=TRUE, sep=",") %>%
            distinct()

    # print(head(ds))
    print(paste(nrow(ds), "rows"))
    return (ds)
}


write_csv_file <- function(ds, dataset_path) {
    path <- dirname(dataset_path)
    dir.create(path, recursive=TRUE, showWarnings=FALSE)
    ds <- ds %>%
        distinct()
    write.csv(ds, dataset_path)

    print(paste("Writing:", dataset_path))
    # print(head(ds))
    # print(paste(str(nrow(ds)), "rows"))
    return (ds)
}
