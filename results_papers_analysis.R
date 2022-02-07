#########################################################
#
# GERA A TABELA 1
#
#
#########################################################

source("my_functions_papers_analysis.R")

readRenviron("envs/results_covid.env")

PAPERS_CSV_FILE_PATH <- Sys.getenv("PAPERS_CSV_FILE_PATH")
results_covid <- read_csv_file(PAPERS_CSV_FILE_PATH) %>%
    xray() %>%
    rename("Artigos sobre COVID"="values")


readRenviron("envs/results_no_english.env")
RESULTS_PAPERS_FILE_PATH <- Sys.getenv("RESULTS_PAPERS_FILE_PATH")
PAPERS_CSV_FILE_PATH <- Sys.getenv("PAPERS_CSV_FILE_PATH")
results_no_english <- read_csv_file(PAPERS_CSV_FILE_PATH) %>%
    xray() %>%
    rename("Artigos sem resumos em inglês"="values")


report <- inner_join(results_no_english, results_covid, by="labels") %>%
    write_csv_file(RESULTS_PAPERS_FILE_PATH)
print(report)
