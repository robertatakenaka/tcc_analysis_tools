#########################################################
#
# Figure 8c
#
#
#########################################################

source("libs/my_functions_papers_analysis.R")
library(ggplot2)
library(tidyverse)
library(hrbrthemes)
library(cowplot)

d_graphic_precision <- function(ds) {
    
    g <- ds %>%
        ggplot(aes(x=precision)) +
        scale_y_continuous(trans='log2') +
        geom_histogram(aes(y = after_stat(count)), fill="#21918c", color="#21918c", alpha=0.05) +
        labs(
            x = "Precisão",
            y = "Quantidade"
        ) +
        theme_bw() +
        theme(text = element_text(size = 15))
    return (g)
}

d_graphic_recall <- function(ds) {
    
    g <- ds %>%
        ggplot(aes(x=recall)) +
        scale_y_continuous(trans='log10') +
        geom_bar(aes(y = after_stat(count)), fill="#440154", color="#440154", alpha=0.05) +
        geom_text(aes(label = after_stat(count)), vjust = 2, stat = "count") +
        labs(
            x = "Sensibilidade",
            y = "Quantidade"
        ) +
        theme_bw() +
        theme(text = element_text(size = 15))
    
    return (g)
}


d_graphic_f1score <- function(ds) {
    
    g <- ds %>%
        ggplot(aes(x=f1score)) +
        scale_y_continuous(trans='log2') +
        geom_histogram(aes(y = after_stat(count)), fill="#3b528b", color="#3b528b", alpha=0.05) +
        labs(
            x = "F score",
            y = "Quantidade"
        ) +
        theme_bw() +
        theme(text = element_text(size = 15))
    
    return (g)
}

d_graphic_f1score_q <- function(ds) {
    g <- ds %>%
        ggplot(aes(x=q)) +
        geom_bar(aes(y = after_stat(count)), fill="#3b528b", color="#3b528b", alpha=0.05) +
        geom_text(aes(label = after_stat(count)), vjust = 2, stat = "count") +
        labs(
            x = "F score",
            y = "Quantidade"
        ) +
        theme_bw() +
        theme(text = element_text(size = 15))
    
    return (g)
}

d_graphic_check <- function(ds) {
    
    g <- ds %>%
        ggplot(aes(x=check)) +
        scale_y_continuous(trans='log10') +
        geom_histogram(aes(y = after_stat(count)), fill="#440154", color="#440154", alpha=0.1) +
        labs(
            x = "",
            y = "Quantidade"
        ) +
        theme_bw() +
        theme(text = element_text(size = 15))
    
    return (g)
}


papers_add_highly_similar_mark <- function(ds) {
    ds <- ds %>%
        mutate(real_a_pid=substr(pid, 1, 23)) %>%
        group_by(real_a_pid) %>%
        mutate(min_expected_connections_by_semantic_similarity=n()-1) %>%
        ungroup()
    return (ds)
}

connections_sum_same <- function(ds) {
    ds <- ds %>%
        group_by(a_pid) %>%
        mutate(total_conn=n()) %>%
        mutate(total_scored=total_conn-sum(c_score_none)) %>%
        mutate(sum_same=sum(same)) %>%
        ungroup() %>%
        select(a_pid, total_conn, total_scored, real_a_pid, sum_same)
    return (ds)
}

qq <- function(ds_metrics) {
    a <- ds_metrics %>%
        group_by(real_a_pid) %>%
        mutate(
            q=case_when(
                f1score < 0.25 ~ "0 - 0.25",
                f1score < 0.5 ~ "0.25 a 0.5",
                f1score < 0.75 ~ "0.5 a 0.75",
                TRUE ~ "0.75 a 1.0"
            )
        )
}



metrics <- function(ds_papers, ds_connections) {

    ds1 <- ds_papers %>%
        filter(min_expected_connections_by_semantic_similarity==0)

    print("========")
    print(nrow(ds1))
    print("========")

    ds_papers <- ds_papers %>%
        dplyr::filter(min_expected_connections_by_semantic_similarity > 0) %>%
        select(pid, min_expected_connections_by_semantic_similarity) %>%
        rename("a_pid"="pid")

    write_csv_file(ds_papers, "_papers.csv")

    ds_x <- ds_connections %>%
        dplyr::filter(total_scored>0)
    write_csv_file(ds_x, "_connections.csv")

    ds <- inner_join(ds_papers, ds_x, by="a_pid") %>%
        mutate(
            precision=sum_same / total_conn,
            recall=sum_same / min_expected_connections_by_semantic_similarity
        ) %>%
        mutate(
            f1score=(2 * precision * recall)/(precision + recall)
        ) %>%
        mutate(check= sum_same / min_expected_connections_by_semantic_similarity)

    print("porcent")
    print(sum(ds$sum_same))
    print(sum(ds$min_expected_connections_by_semantic_similarity))
    print(sum(ds$sum_same) / sum(ds$min_expected_connections_by_semantic_similarity))
    print("--------")
    print(nrow(ds_papers))
    print(nrow(ds_connections))
    print(nrow(ds))
    return (ds)
}

readRenviron("envs/results_pares.env")
G_PRECISION <- Sys.getenv("G_PRECISION")
G_RECALL <- Sys.getenv("G_RECALL")
G_F1SCORE <- Sys.getenv("G_F1SCORE")
METRICS <- Sys.getenv("METRICS")
RECALL_FILE_PATH <- Sys.getenv("RECALL_FILE_PATH")
G_RECALL2 <- Sys.getenv("G_RECALL2")


PAPERS_CSV_FILE_PATH <- Sys.getenv("PAPERS_CSV_FILE_PATH")
CONNECTIONS_FILE_PATH <- Sys.getenv("CONNECTIONS_FILE_PATH")

ADD_PAPERS_CSV_FILE_PATH <- Sys.getenv("ADD_PAPERS_CSV_FILE_PATH")
ADD_CONNECTIONS_FILE_PATH <- Sys.getenv("ADD_CONNECTIONS_FILE_PATH")

ds_papers <- read_csv_file(PAPERS_CSV_FILE_PATH) %>%
    papers_add_highly_similar_mark() %>%
    write_csv_file(ADD_PAPERS_CSV_FILE_PATH)

ds_connections <- read_csv_file(CONNECTIONS_FILE_PATH) %>%
    connections_sum_same() %>%
    write_csv_file(ADD_CONNECTIONS_FILE_PATH)


ds_metrics <- metrics(ds_papers, ds_connections) %>%
    write_csv_file(METRICS)

g_precision <- d_graphic_precision(ds_metrics) %>%
    save_g(G_PRECISION)
g_recall <- d_graphic_recall(ds_metrics) %>%
    save_g(G_RECALL)
g_f1score <- d_graphic_f1score(ds_metrics) %>%
    save_g(G_F1SCORE)

g_f1score_q <- qq(ds_metrics) %>%
    d_graphic_f1score_q() %>%
    save_g("q.jpg")

g_check <- d_graphic_check(ds_metrics) %>%
    save_g("check.jpg")

recall_ <- ds_metrics %>%
    filter(recall < 1, sum_same > 0) %>%
    select(a_pid, recall, sum_same) %>%
    write_csv_file(RECALL_FILE_PATH)

g_recall2 <- d_graphic_recall(recall_) %>%
    save_g(G_RECALL2)


g <- plot_grid(g_precision, g_recall, g_f1score, g_f1score_q, nrow = 2) %>%
    xxx("g.jpg")
