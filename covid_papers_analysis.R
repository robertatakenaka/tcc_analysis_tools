
source("my_functions_papers_analysis.R")

readRenviron("envs/results_covid.env")

PAPERS_CSV_FILE_PATH <- Sys.getenv("PAPERS_CSV_FILE_PATH")
SOURCE_CSV_FILE_PATH <- Sys.getenv("SOURCE_CSV_FILE_PATH")
PAPER_ANALYSIS_SCORES_FILE_PATH <- Sys.getenv("PAPER_ANALYSIS_SCORES_FILE_PATH")
GRAPHIC1_PAPERS_FILE_PATH <- Sys.getenv("GRAPHIC1_PAPERS_FILE_PATH")
GRAPHIC2_PAPERS_FILE_PATH <- Sys.getenv("GRAPHIC2_PAPERS_FILE_PATH")
GRAPHIC3_PAPERS_FILE_PATH <- Sys.getenv("GRAPHIC3_PAPERS_FILE_PATH")


ds_graphic <- get_ds_graphic()


# no_en > 0 ~ 'nenhum resumo em inglês',
# en_only > 0 ~ 'somente resumo em inglês',
# with_en > 0 ~ 'resumos multilíngues com inglês'

ds_g1 <- ds_graphic %>%
    # filter(en_presence=='nenhum resumo em inglês') %>%
    graphic_papers_mode2(aspect_ratio=3/5, ncol=1) %>%
    save_g(GRAPHIC1_PAPERS_FILE_PATH) 


# ds_g2 <- ds_graphic %>%
#     filter(en_presence=='somente resumo em inglês' | en_presence=='resumos multilíngues com inglês' ) %>%
#     graphic_papers(aspect_ratio=3, ncol=2) %>%
#     save_g(GRAPHIC2_PAPERS_FILE_PATH) 


# ds_g3 <- ds_graphic %>%
#     filter(en_presence=='resumos multilíngues com inglês') %>%
#     graphic_papers(aspect_ratio=1, ncol=1) %>%
#     save_g(GRAPHIC3_PAPERS_FILE_PATH) 
