#########################################################
#
# GERA A TABELA 1
#
#
#########################################################

source("./libs/my_functions_papers_analysis.R")


readRenviron("envs/results_pares.env")

SOURCES_CSV_FILE_PATH <- Sys.getenv("SOURCES_CSV_FILE_PATH")
RESULTS_PAPERS_FILE_PATH <- Sys.getenv("RESULTS_PAPERS_FILE_PATH")
PAPERS_CSV_FILE_PATH <- Sys.getenv("PAPERS_CSV_FILE_PATH")

sources_pares <- read_csv_file(SOURCES_CSV_FILE_PATH)

papers <- read_csv_file(PAPERS_CSV_FILE_PATH)

results_pares <- papers %>%
    xray(sources_pares) %>%
    rename("C"="values")%>%
    write_csv_file(RESULTS_PAPERS_FILE_PATH)


langs <- read_csv_file("./datasets/LANGS.csv")
u_langs <- papers %>%
	mutate(lang = substr(pid, 25, 27)) %>%
	select(lang) %>%
	distinct() %>%
	left_join(langs, by="lang") %>%
    write_csv_file("_rep_/_s3_langs.csv")


u_pids <- papers %>%
	mutate(a_pid = substr(pid, 1, 23)) %>%
	select(a_pid) %>%
	distinct()%>%
    write_csv_file("_rep_/_s3_abstracts.csv")


