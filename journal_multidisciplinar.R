############################################################################
#
#  Identifica a frequência de periódicos multidisciplinares, unidisciplinares
#  etc
#
############################################################################

library("tidyverse")
library("stringi")
library("fastDummies")
library("reshape2")

options(digits = 2)
options(scipen = 999)
# specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))

read_ds <- function(dataset_path) {
  print(dataset_path)
  ds <- read.csv(dataset_path, header=TRUE, sep=",") %>% distinct()
  return (ds)
}

create_areas_dummies <- function(dataset, areas) {
    print(head(dataset))
    print(areas)
    l1 <- list("key", "collection")
    l2 <- areas
    column_names <- unlist(c(l1, l2))

    
    print('column_names')
    print(column_names)
    
    areas_dummies <- dummy_columns(.data = dataset,
                                    select_columns = c("value"),
                                    remove_selected_columns = T,
                                    remove_first_dummy = F)
    print(head(areas_dummies))
    colnames(areas_dummies) <- column_names 
    
    print("----")
    print(head(areas_dummies))
    
    print("----")
    return (areas_dummies)
}

merge_obs <- function(dataset) {
  ds <- dataset %>%
        group_by(key) %>%
        summarize(
            "Linguistics, Letters and Arts x"=sum("Linguistics, Letters and Arts")
        )
  return (ds)
}

n_disc <- function(dataset) {
  ds <- dataset %>%
    group_by(key) %>%
    # mutate(freq=n())
    summarize(n_disciplines=n())
  return (ds)
}

uni_disc <- function(dataset) {
  ds <- dataset %>%
    group_by(key) %>%
    mutate(freq=n()) %>%
    filter(freq==1)
    
  return (ds)
}


readRenviron(".appenv")
JOURNAL_AREAS_FILE_PATH <- Sys.getenv("JOURNAL_AREAS_FILE_PATH")
AREAS_FILE_PATH <- Sys.getenv("AREAS_FILE_PATH")

JOURNAL_AREAS_1_FILE_PATH <- Sys.getenv("JOURNAL_AREAS_1_FILE_PATH")
JOURNAL_AREAS_2_FILE_PATH <- Sys.getenv("JOURNAL_AREAS_2_FILE_PATH")
JOURNAL_AREAS_3_FILE_PATH <- Sys.getenv("JOURNAL_AREAS_3_FILE_PATH")
JOURNAL_AREAS_4_FILE_PATH <- Sys.getenv("JOURNAL_AREAS_4_FILE_PATH")
JOURNAL_AREAS_5_FILE_PATH <- Sys.getenv("JOURNAL_AREAS_5_FILE_PATH")

j_areas = read_ds(JOURNAL_AREAS_FILE_PATH)
areas = read_ds(AREAS_FILE_PATH)
areas_names = c(areas$Var1)

d1 <- create_areas_dummies(j_areas, areas_names)
write.csv(d1, JOURNAL_AREAS_1_FILE_PATH)

d2 <- dcast(j_areas, key ~ value, var.value=c("value"))
write.csv(d2, JOURNAL_AREAS_2_FILE_PATH)


d3 <- n_disc(j_areas)
head(d3)
write.csv(d3, JOURNAL_AREAS_3_FILE_PATH)

d4 <- uni_disc(j_areas)
head(d4)
write.csv(d4, JOURNAL_AREAS_4_FILE_PATH)

d5 <- group_by(d4, value) %>% summarize(freq=n())
head(d5, 10)
write.csv(d4, JOURNAL_AREAS_5_FILE_PATH)
