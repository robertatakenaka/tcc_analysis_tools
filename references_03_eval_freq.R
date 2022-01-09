library("tidyverse")
library("stringi")
library("fastDummies")
library("reshape2")

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


add_keys <- function(ds) {
    # "","pid","collection.x","pub_date","vol","num","suppl","page","surname","corpauth","doi","journal","article_title","source","issn","thesis_date","thesis_loc","thesis_country","thesis_degree","thesis_org","conf_date","conf_loc","conf_country","conf_name","conf_org","publisher_loc","publisher_country","publisher_name","edition","source_author","source_corpauth","issn_id","X","collection.y","area","freq"
    ds <- mutate(
            ds,
            ref_type=case_when(
                journal != "" ~ "journal",
                thesis_date != "" | thesis_loc != "" | thesis_country != "" | thesis_degree != "" | thesis_org != "" ~ "thesis",
                conf_date != "" | conf_loc != "" | conf_country != "" | conf_name != "" | conf_org != "" ~ "conference",
                source != "" ~ 'book',
                source == "" ~ "UNK"
            )
        ) %>%
        mutate(ref_year_contrib_vol=toupper(paste(substr(pub_date,1,4),surname,corpauth,vol,source,sep="_"))) 
    return (ds)
}

evaluate_refs_year_surname_corpauth_vol_source <- function(ds_only_one_subject_area_journals, folder_path) {
  #areas_freq <- data.frame(table(ds_only_one_subject_area_journals$area))

  # dir.create(folder_path)

  areas = c("Exact and Earth Sciences", "Health Sciences", "Human Sciences",
            "Linguistics, Letters and Arts")  
  #for (area in areas_freq$Var1) {
  for (area in areas) {  
      print(area)

      #file_path <- file.path(folder_path, paste(area, "csv", sep="."))
      new_file_path <- file.path(folder_path, paste(area, "k", "csv", sep="."))
      
      # ds_refs <- read_csv_file(file_path) %>% add_keys()%>%
      #  write.csv(new_file_path)
      ds_refs <- read_csv_file(new_file_path)

      new_file_path <- file.path(folder_path, paste(area, "freq", "csv", sep="."))
      print(new_file_path)
      get_freq_ds(table(ds_refs$ref_year_contrib_vol), new_file_path)
  }
}


readRenviron(".appenv")

# 
JOURNAL_ONLY_ONE_SUBJECT_AREA_FILE_PATH <- Sys.getenv("JOURNAL_ONLY_ONE_SUBJECT_AREA_FILE_PATH")
REFERENCES_ANALYSIS_FOLDER_PATH <- Sys.getenv("REFERENCES_ANALYSIS_FOLDER_PATH")

ds_only_one_subject_area_journals <- read_csv_file(JOURNAL_ONLY_ONE_SUBJECT_AREA_FILE_PATH)

evaluate_refs_year_surname_corpauth_vol_source(ds_only_one_subject_area_journals, REFERENCES_ANALYSIS_FOLDER_PATH)



