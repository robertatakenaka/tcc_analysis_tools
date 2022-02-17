#########################################################
#
# GERA A TABELA 1
#
#
#########################################################

source("my_functions_papers_analysis.R")

readRenviron("envs/results_covid.env")

SOURCES_CSV_FILE_PATH <- Sys.getenv("SOURCES_CSV_FILE_PATH")
sources_covid <- read_csv_file(SOURCES_CSV_FILE_PATH) %>%
    t_sources()
# print(sources_covid)

PAPERS_CSV_FILE_PATH <- Sys.getenv("PAPERS_CSV_FILE_PATH")
results_covid <- read_csv_file(PAPERS_CSV_FILE_PATH) %>%
    xray() %>%
    rename("Artigos sobre COVID"="values")


readRenviron("envs/results_no_english.env")

SOURCES_CSV_FILE_PATH <- Sys.getenv("SOURCES_CSV_FILE_PATH")
sources_no_english <- read_csv_file(SOURCES_CSV_FILE_PATH) %>%
    t_sources()
# print(sources_no_english)

RESULTS_PAPERS_FILE_PATH <- Sys.getenv("RESULTS_PAPERS_FILE_PATH")
PAPERS_CSV_FILE_PATH <- Sys.getenv("PAPERS_CSV_FILE_PATH")
results_no_english <- read_csv_file(PAPERS_CSV_FILE_PATH) %>%
    xray() %>%
    rename("Artigos sem resumos em inglês"="values")


readRenviron("envs/results_pares.env")

SOURCES_CSV_FILE_PATH <- Sys.getenv("SOURCES_CSV_FILE_PATH")
sources_pares <- read_csv_file(SOURCES_CSV_FILE_PATH) %>%
    t_sources()
# print(sources_pares)

RESULTS_PAPERS_FILE_PATH <- Sys.getenv("RESULTS_PAPERS_FILE_PATH")
PAPERS_CSV_FILE_PATH <- Sys.getenv("PAPERS_CSV_FILE_PATH")
results_pares <- read_csv_file(PAPERS_CSV_FILE_PATH) %>%
    xray() %>%
    rename("Artigos Pareados"="values")


report <- inner_join(results_no_english, results_covid, by="labels") %>%
    inner_join(results_pares, by="labels") %>%
    write_csv_file(RESULTS_PAPERS_FILE_PATH)
print(report)


print("====================")
print(sources_no_english)
print(sources_covid)
print(sources_pares)
