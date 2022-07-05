#########################################################
#
# Figure 8c
#
#
#########################################################

source("libs/my_functions_files.R")
source("libs/my_functions_graphics.R")

options(digits = 2)
options(scipen = 999)
#install.packages("incase")
library(incase)
library(ggplot2)
library(tidyverse)
library(hrbrthemes)
library(cowplot)


vp_graphic <- function(ds) {
    
    g <- ds %>%
        ggplot(aes(x=VP)) +
        geom_histogram(aes(y = after_stat(count)), fill="#21918c", color="#21918c", alpha=0.1) +
        labs(
            x = "VP",
            y = "Quantidade de artigos"
        ) +
        theme_bw() +
        theme(text = element_text(size = 30))
    return (g)
}


fp_graphic <- function(ds) {
    
    g <- ds %>%
        ggplot(aes(x=FP)) +
        geom_histogram(aes(y = after_stat(count)), fill="#21918c", color="#21918c", alpha=0.1) +
        labs(
            x = "FP",
            y = "Quantidade de artigos"
        ) +
        theme_bw() +
        theme(text = element_text(size = 30))
    return (g)
}


fn_graphic <- function(ds) {
    
    g <- ds %>%
        ggplot(aes(x=FN)) +
        geom_histogram(aes(y = after_stat(count)), binwidth = 0.2, fill="#21918c", color="#21918c", alpha=0.1) +
        labs(
            x = "FN",
            y = "Quantidade de artigos"
        ) +
        theme_bw() +
        theme(text = element_text(size = 30))
    return (g)
}


d_graphic_precision <- function(ds) {
    
    g <- ds %>%
        ggplot(aes(x=precision)) +
        scale_y_continuous(trans='log2') +
        geom_histogram(aes(y = after_stat(count)), fill="#21918c", color="#21918c", alpha=0.1) +
        labs(
            x = "Valores de Precisão",
            y = "Quantidade de artigos"
        ) +
        theme_bw() +
        theme(text = element_text(size = 30))
    return (g)
}

d_graphic_recall <- function(ds) {
    
    g <- ds %>%
        ggplot(aes(x=sensibilidade)) +
        scale_y_continuous(trans='log10') +
        geom_bar(aes(y = after_stat(count)), fill="#440154", color="#440154", alpha=0.1) +
        geom_text(aes(label = after_stat(count)), vjust = 2, stat = "count") +
        labs(
            x = "Valores de Sensibilidade",
            y = "Quantidade de artigos"
        ) +
        theme_bw() +
        theme(text = element_text(size = 30))
    
    return (g)
}


d_graphic_f1score <- function(ds) {
    
    g <- ds %>%
        ggplot(aes(x=F1)) +
        scale_y_continuous(trans='log2') +
        geom_histogram(aes(y = after_stat(count)), fill="#3b528b", color="#3b528b", alpha=0.1) +
        labs(
            x = "Valores de F score",
            y = "Quantidade de artigos"
        ) +
        theme_bw() +
        theme(text = element_text(size = 30))
    
    return (g)
}

d_graphic_f1score_q <- function(ds) {
    g <- ds %>%
        ggplot(aes(x=q)) +
        geom_bar(aes(y = after_stat(count)), fill="#3b528b", color="#3b528b", alpha=0.1) +
        geom_text(aes(label = after_stat(count)), vjust = 2, stat = "count") +
        labs(
            x = "Valores de F score",
            y = "Quantidade de artigos"
        ) +
        theme_bw() +
        theme(text = element_text(size = 30))
    
    return (g)
}

d_graphic_check <- function(ds) {
    
    g <- ds %>%
        ggplot(aes(x=check)) +
        scale_y_continuous(trans='log10') +
        geom_histogram(aes(y = after_stat(count)), fill="#440154", color="#440154", alpha=0.1) +
        labs(
            x = "Valores de ",
            y = "Quantidade de artigos"
        ) +
        theme_bw() +
        theme(text = element_text(size = 15))
    
    return (g)
}


papers_add_highly_similar_mark <- function(ds) {
    # ==> s3_papers_refs_report.csv <==
    # pid,refs,refs_with_doi,refs_without_doi,connections,connections_with_score,connections_without_score,min_score,max_score
    # S0004-05922014000300016_it,15,0,15,1,1,0,0.76,0.76

    d <- ds %>%
        mutate(real_a_pid=substr(pid, 1, 23), a_pid=pid) %>%
        group_by(real_a_pid) %>%
        mutate(min_expected_connections_by_semantic_similarity=n()-1) %>%
        ungroup()
    return (d)
}

connections_add_numbers <- function(ds) {
    # a_pid,c_pid,real_a_pid,real_c_pid,same,c_score,c_score_none
    # S0004-05922014000300016_it,S0004-05922014000300016_en,S0004-05922014000300016,S0004-05922014000300016,1,0.76,0

    ds[is.na(ds)] <- 0
    d <- ds %>%
        mutate(ge_07=case_when(c_score_none == 0 & c_score >= 0.7 ~ 1, TRUE ~ 0)) %>%
        select(
            a_pid, 
            c_pid,
            ge_07,
            same,
            c_score
        )
    #d$ge_07 <- as.integer(d$ge_07)
    return (d)
}


connections_summarize <- function(ds) {
    ds[is.na(ds)] <- 0

    d <- ds %>%
        group_by(a_pid) %>%
        mutate(total_connections=n()) %>%
        mutate(connections_with_score_ge_07=sum(ge_07)) %>%
        mutate(connections_with_same_paper=sum(same)) %>%
        select(
            a_pid, 
            total_connections, 
            connections_with_score_ge_07,
            connections_with_same_paper,
            min_expected_connections_by_semantic_similarity,
        ) %>%
        unique()
    return (d)
}


get_graphic <- function(ds_metrics, g_path) {

    ds <- ds_metrics %>%
        mutate(
            precision = VP / (VP + FP),
            sensibilidade = VP / (VP + FN),
            F1 = 2 * (precision * sensibilidade) / (precision + sensibilidade)
        )
    vp <- vp_graphic(ds)
    fp <- fp_graphic(ds)
    fn <- fn_graphic(ds)

    g_precision <- d_graphic_precision(ds)
    g_recall <- d_graphic_recall(ds)
    g_f1score <- d_graphic_f1score(ds)
    g_f1score_q <- qq(ds) %>%
        d_graphic_f1score_q()

    g_p <- plot_grid(vp, fp, fn, nrow = 1)
    g <- plot_grid(g_precision, g_recall, g_f1score, g_f1score_q, g_p,  nrow = 3) %>%
        graphics(g_path)
}

add_metrics_x <- function(ds) {
    ds[is.na(ds)] <- 0

    ds3 <- ds %>%
        connections_summarize()

    ds3 <- ds3 %>%
        rename(min_expected=min_expected_connections_by_semantic_similarity) %>%
        select(
            min_expected,
            connections_with_same_paper,
            same,
            connections_with_score_ge_07,
            total_connections
        )

    print("AAAAA")
    ds3$min_expected <- as.integer(
        ds3$min_expected
    )
    ds3$fnaux <- ds3$min_expected - ds3$connections_with_same_paper

    print("CCC")

    ds3 <- ds3 %>%
        mutate(
            VP=case_when(
                same == 1 ~ connections_with_same_paper,
                same == 0 ~ connections_with_score_ge_07)
            )

    print("DDD")

    ds3 <- ds3 %>%
        mutate(
            FP=case_when(
                same == 1 ~ (total_connections - connections_with_same_paper),
                same == 0 ~ (total_connections - connections_with_score_ge_07)
            )
        )
    print("EEE")

    ds3[is.na(ds3)] <- 0


    ds3 <- ds3 %>%
        mutate(
            FNx=incase(
                same == 0 ~ 0,
                TRUE ~ fnaux
            ))

    print("BBBBB")

    write.csv(ds3, "_rep_/metrics4_x.csv")

    # print(head(ds3))
    # print("............")
    return (ds3)
}

add_metrics <- function(ds) {

    d <- ds %>%
        connections_summarize() %>%
        mutate(
            VP=connections_with_score_ge_07,
            FP=total_connections - connections_with_score_ge_07,
            FN=min_expected_connections_by_semantic_similarity - connections_with_same_paper
        ) %>% 
        select(VP, FP, FN, total_connections, min_expected_connections_by_semantic_similarity, connections_with_same_paper, connections_with_score_ge_07) %>%
        write_csv_file("_rep_/metrics3_todos_only.csv")
    return (d)
}

add_metrics_to_same <- function(ds) {

    d <- ds %>%
        filter(same==1) %>%
        connections_summarize() %>%
        mutate(
            VP=connections_with_same_paper,
            FP=total_connections - connections_with_same_paper,
            FN=min_expected_connections_by_semantic_similarity - connections_with_same_paper
        ) %>% 
        select(VP, FP, FN, total_connections, min_expected_connections_by_semantic_similarity, connections_with_same_paper) %>%
        write_csv_file("_rep_/metrics1_same_only.csv")
    return (d)
}

add_metrics_to_diff <- function(ds) {

    d <- ds %>%
        filter(same==0) %>%
        connections_summarize() %>%
        mutate(
            VP=connections_with_score_ge_07,
            FP=total_connections - connections_with_score_ge_07,
            FN=0
        ) %>% 
        select(VP, FP, FN, total_connections, connections_with_score_ge_07) %>%
        write_csv_file("_rep_/metrics2_diff_only.csv")
    return (d)
}

qq <- function(ds_metrics) {
    a <- ds_metrics %>%
        mutate(
            q=case_when(
                F1 <= 0.25 ~ "0 a 0.25",
                0.25 < F1 & F1 <= 0.5 ~ "0.25 a 0.5",
                0.5 < F1  & F1 <= 0.75 ~ "0.5 a 0.75",
                F1 > 0.75 ~ "0.75 a 1.0"
            )
        )
    return (a)
}


readRenviron("envs/results_pares.env")

# G_PRECISION <- Sys.getenv("G_PRECISION")
# G_RECALL <- Sys.getenv("G_RECALL")
# G_F1SCORE <- Sys.getenv("G_F1SCORE")
# RECALL_FILE_PATH <- Sys.getenv("RECALL_FILE_PATH")
# G_RECALL2 <- Sys.getenv("G_RECALL2")


PAPERS_CSV_FILE_PATH <- Sys.getenv("PAPERS_CSV_FILE_PATH")
CONNECTIONS_FILE_PATH <- Sys.getenv("CONNECTIONS_FILE_PATH")
METRICS1 <- Sys.getenv("METRICS1")
METRICS2 <- Sys.getenv("METRICS2")
METRICS3 <- Sys.getenv("METRICS3")
METRICS4 <- Sys.getenv("METRICS4")

ADD_PAPERS_CSV_FILE_PATH <- Sys.getenv("ADD_PAPERS_CSV_FILE_PATH")
ADD_CONNECTIONS_FILE_PATH <- Sys.getenv("ADD_CONNECTIONS_FILE_PATH")

# ds_papers <- read_csv_file(PAPERS_CSV_FILE_PATH) %>%
#     papers_add_highly_similar_mark() %>%
#     write_csv_file(ADD_PAPERS_CSV_FILE_PATH)

# ds_connections <- read_csv_file(CONNECTIONS_FILE_PATH) %>%
#     connections_add_numbers() %>%
#     write_csv_file(ADD_CONNECTIONS_FILE_PATH)

# ds <- left_join(ds_papers, ds_connections, by="a_pid") %>%
#     write_csv_file("zz_items.csv")

# ds_metrics <- add_metrics_x(ds)
#  # %>%
#  #    get_graphic(METRICS4)

# ds_metrics <- add_metrics_to_same(ds) %>%
#     get_graphic(METRICS1)
# ds_metrics <- add_metrics_to_diff(ds) %>%
#     get_graphic(METRICS2)
# ds_metrics <- add_metrics(ds) %>%
#     get_graphic(METRICS3)

ds_papers <- read_csv_file(PAPERS_CSV_FILE_PATH)
ds_papers_added <- papers_add_highly_similar_mark(ds_papers)
write_csv_file(ds_papers_added, ADD_PAPERS_CSV_FILE_PATH)

ds_connections <- read_csv_file(CONNECTIONS_FILE_PATH)
ds_connections_added <- connections_add_numbers(ds_connections)
write_csv_file(ds_connections_added, ADD_CONNECTIONS_FILE_PATH)

ds <- left_join(ds_papers_added, ds_connections_added, by="a_pid")
write_csv_file(ds, "zz_items.csv")

# dsm4 <- add_metrics_x(ds)
# print("fim....")
 # %>%
 #    get_graphic(METRICS4)

dsm1 <- add_metrics_to_same(ds) %>%
    get_graphic(METRICS1)
dsm2 <- add_metrics_to_diff(ds) %>%
    get_graphic(METRICS2)
# dsm3 <- add_metrics(ds) %>%
#     get_graphic(METRICS3)
