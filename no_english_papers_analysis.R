
source("my_functions_papers_analysis.R")

readRenviron("envs/results_no_english.env")

PAPERS_CSV_FILE_PATH <- Sys.getenv("PAPERS_CSV_FILE_PATH")
SOURCE_CSV_FILE_PATH <- Sys.getenv("SOURCE_CSV_FILE_PATH")
PAPER_ANALYSIS_SCORES_FILE_PATH <- Sys.getenv("PAPER_ANALYSIS_SCORES_FILE_PATH")
GRAPHIC_PAPERS_FILE_PATH <- Sys.getenv("GRAPHIC_PAPERS_FILE_PATH")
GRAPHIC_VIOLIN_PAPERS_FILE_PATH <- Sys.getenv("GRAPHIC_VIOLIN_PAPERS_FILE_PATH")


ds_graphic <- get_ds_graphic() %>%
    graphic_papers(aspect_ratio=2/5, ncol=2) %>%
    save_g(GRAPHIC_PAPERS_FILE_PATH) 


# ds_graphic <- get_ds_graphic() %>%
#     graphic_papers_mode2(aspect_ratio=2/5, ncol=2) %>%
#     save_g(GRAPHIC_VIOLIN_PAPERS_FILE_PATH) 
