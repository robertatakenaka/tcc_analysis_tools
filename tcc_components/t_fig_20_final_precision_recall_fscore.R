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

connections_graphic <- function(ds) {
    
    g <- ds %>%
        ggplot(aes(x=connections)) +
        scale_y_continuous(trans='log10') +
        # geom_histogram(aes(y = after_stat(count)), binwidth = 0.5, fill="#21918c", color="#21918c", alpha=0.2) +
        geom_bar(aes(y = after_stat(count)), binwidth = 0.5, fill="#13306dff", color="#13306dff", alpha=0.2) +
        geom_text(aes(label = after_stat(count)), vjust = 2, stat = "count", size = 4) +
        labs(
            x = "Quantidade de conexões",
            y = "Quantidade de artigos"
        ) +
        theme_bw() +
        theme(text = element_text(size = 13))
    return (g)
}


vp_graphic <- function(ds) {
    
    g <- ds %>%
        ggplot(aes(x=VP)) +
        scale_y_continuous(trans='log10') +
        # geom_histogram(aes(y = after_stat(count)), binwidth = 0.5, fill="#21918c", color="#21918c", alpha=0.2) +
        geom_bar(aes(y = after_stat(count)), binwidth = 0.5, fill="#13306dff", color="#13306dff", alpha=0.2) +
        geom_text(aes(label = after_stat(count)), vjust = 2, stat = "count", size = 4) +
        labs(
            x = "VP = Quantidade de artigos relevantes recomendados",
            y = "Quantidade de artigos"
        ) +
        theme_bw() +
        theme(text = element_text(size = 13))
    return (g)
}


fp_graphic <- function(ds) {
    ds <- ds %>%
            mutate(
            qFP=case_when(
                FP == 0 ~ 0,
                FP > 0 & FP <= 25 ~ 25,
                FP > 25 & FP <= 50 ~ 50,
                FP > 50 & FP <= 75 ~ 75,
                FP > 75 & FP <= 100 ~ 100,
                FP > 100 & FP <= 125 ~ 125,
                FP > 125 & FP <= 150 ~ 150,
                FP > 150 & FP <= 175 ~ 175,
                FP > 175 & FP <= 200 ~ 200,
                FP > 200 & FP <= 250 ~ 250,
                FP > 225 & FP <= 250 ~ 250,
                FP > 250 & FP <= 275 ~ 275,
                FP > 375 & FP <= 300 ~ 300,
                FP > 300 ~ 500
            )
        )

    g <- ds %>%
        ggplot(aes(x=qFP)) +
        scale_y_continuous(trans='log10') +
        # scale_x_continuous(trans='log2') +
        # # geom_histogram(aes(y = after_stat(count)), binwidth = 0.5, fill="#21918c", color="#21918c", alpha=0.2) +
        geom_bar(aes(y = after_stat(count)), binwidth = 2, fill="#21918c", color="#21918c", alpha=0.2) +
        geom_text(aes(label = after_stat(count)), vjust = 2, stat = "count", size = 4) +
        # geom_freqpoly(binwidth = 0.5) +

        labs(
            x = "FP = Quantidade de artigos irrelevantes recomendados",
            y = "Quantidade de artigos"
        ) +
        theme_bw() +
        theme(text = element_text(size = 13))
    return (g)
}


fn_graphic <- function(ds) {
    
    g <- ds %>%
        ggplot(aes(x=FN)) +
        scale_y_continuous(trans='log10') +
        # geom_histogram(aes(y = after_stat(count)), binwidth = 0.5, fill="#21918c", color="#21918c", alpha=0.2) +
        geom_bar(aes(y = after_stat(count)), binwidth = 1, fill="#FE5A27", color="#FE5A27", alpha=0.2) +
        geom_text(aes(label = after_stat(count)), vjust = 2, stat = "count", size = 4) +
        labs(
            x = "FN = Quantidade de artigos relevantes não recomendados",
            y = "Quantidade de artigos"
        ) +
        theme_bw() +
        theme(text = element_text(size = 13))
    return (g)
}


d_graphic_precision <- function(ds) {
    ds <- ds %>%
        qq()
    g <- ds %>%
        ggplot(aes(x=q)) +
        scale_y_continuous(trans='log10') +
        ## geom_histogram(aes(y = after_stat(count)), binwidth = 0.5, fill="#21918c", color="#21918c", alpha=0.2) +
        geom_bar(aes(y = after_stat(count)), binwidth = 0.1, fill="#90548bff", color="#90548bff", alpha=0.2) +
        geom_text(aes(label = after_stat(count)), vjust = 2, stat = "count", size = 4) +
        ##geom_freqpoly(binwidth = 0.5) +
        labs(
            x = "Precisão = artigos relevantes / total de recomendados",
            y = "Quantidade de artigos"
        ) +
        theme_bw() +
        theme(text = element_text(size = 13))
    return (g)
}

# d_graphic_precision <- function(ds) {
    
#     g <- ds %>%
#         ggplot(aes(x=precision)) +
#         scale_y_continuous(trans='log10') +
#         # scale_x_continuous(trans='log2') +

#         ## geom_histogram(aes(y = after_stat(count)), binwidth = 0.5, fill="#21918c", color="#21918c", alpha=0.2) +
#         geom_bar(aes(y = after_stat(count)), binwidth = 0.1, fill="#3b528b", color="#3b528b", alpha=0.2) +
#         # geom_text(aes(label = after_stat(count)), vjust = 2, stat = "count", size = 4) +
#         ##geom_freqpoly(binwidth = 0.5) +
#         labs(
#             x = "Precisão = artigos relevantes / total de recomendados",
#             y = "Quantidade de artigos"
#         ) +
#         theme_bw() +
#         theme(text = element_text(size = 13))
#     return (g)
# }

d_graphic_recall <- function(ds) {
    
    g <- ds %>%
        ggplot(aes(x=sensibilidade)) +
        scale_y_continuous(trans='log10') +
        geom_bar(aes(y = after_stat(count)), binwidth = 1, fill="#00AA4F", color="#00AA4F", alpha=0.2) +
        geom_text(aes(label = after_stat(count)), vjust = 2, stat = "count", size = 4) +
        labs(
            x = "Sensibilidade= relevantes recomendados/ total de relevantes",
            y = "Quantidade de artigos"
        ) +
        theme_bw() +
        theme(text = element_text(size = 13))
    
    return (g)
}


d_graphic_f1score <- function(ds) {
    ds <- ds %>%
        mutate(
            fs=case_when(
                F1 == 0 ~ "0",
                F1 > 0 & F1 <= 0.1 ~ "0,1",
                F1 > 0.1 & F1 <= 0.2 ~ "0,2",
                F1 > 0.2 & F1 <= 0.3 ~ "0,3",
                F1 > 0.3 & F1 <= 0.4 ~ "0,4",
                F1 > 0.4 & F1 <= 0.5 ~ "0,5",
                F1 > 0.5 & F1 <= 0.6 ~ "0,6",
                F1 > 0.6 & F1 <= 0.7 ~ "0,7",
                F1 > 0.7 & F1 <= 0.8 ~ "0,8",
                F1 > 0.8 & F1 <= 0.9 ~ "0,9",
                F1 > 0.9 & F1 <= 1 ~ "1",
                F1 == 1 ~ "1"
            )
        )    
    g <- ds %>%
        ggplot(aes(x=fs)) +
        scale_y_continuous(trans='log10') +
        # geom_histogram(aes(y = after_stat(count)), binwidth = 0.05, fill="#fb528b", color="#fb528b", alpha=0.2) +
        geom_bar(aes(y = after_stat(count)), binwidth = 0.5, fill="#f9b641ff", color="#f9b641ff", alpha=0.2) +
        geom_text(aes(label = after_stat(count)), vjust = 2, stat = "count", size = 4) +
        labs(
            x = "F-score",
            y = "Quantidade de artigos"
        ) +
        theme_bw() +
        theme(text = element_text(size = 13))
    
    return (g)
}

#13306dff
#21918cff
#403891ff
#90548bff
#cc6a70ff
#f9b641ff

d_graphic_f1score_q <- function(ds) {
    g <- ds %>%
        ggplot(aes(x=q)) +
        geom_bar(aes(y = after_stat(count)), binwidth = 0.5, fill="#3b528b", color="#3b528b", alpha=0.2) +
        geom_text(aes(label = after_stat(count)), vjust = 2, stat = "count", size=8) +
        labs(
            x = "Valores de F score",
            y = "Quantidade de artigos"
        ) +
        theme_bw() +
        theme(text = element_text(size = 13))
    
    return (g)
}


d_graphic_check <- function(ds) {
    
    g <- ds %>%
        ggplot(aes(x=check)) +
        scale_y_continuous(trans='log10') +
        geom_histogram(aes(y = after_stat(count)), fill="#440154", color="#440154", alpha=0.2) +
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
        mutate(real_a_pid=substr(pid, 1, 23)) %>%
        group_by(real_a_pid) %>%
        mutate(min_expected_connections_by_semantic_similarity=n()-1) %>%
        ungroup()
    return (d)
}


connections_add_numbers <- function(ds) {
    # a_pid,c_pid,real_a_pid,real_c_pid,same,c_score,c_score_none
    # S0004-05922014000300016_it,S0004-05922014000300016_en,S0004-05922014000300016,S0004-05922014000300016,1,0.76,0
    d <- ds %>%
        mutate(ge_07=case_when(c_score_none == 0 & c_score >= 0.7 ~ 1, TRUE ~ 0)) %>%
        mutate(same_ge_07=case_when(same == 1 & ge_07 == 1 ~ 1, TRUE ~ 0)) %>%
        mutate(diff_ge_07=case_when(same == 0 & ge_07 == 1 ~ 1, TRUE ~ 0)) %>%
        mutate(same_papers=case_when(same == 1 ~ 1, TRUE ~ 0)) %>%
        mutate(diff_papers=case_when(same == 0 ~ 1, TRUE ~ 0)) %>%
        select(
            a_pid,
            c_pid,
            same_ge_07,
            diff_ge_07,
            same_papers,
            diff_papers,
        )
    return (d)
}


connections_summarize <- function(ds) {
    d <- ds %>%
        group_by(pid) %>%
        mutate(sum_connections_with_same_ge_07=sum(same_ge_07)) %>%
        mutate(sum_connections_with_diff_ge_07=sum(diff_ge_07)) %>%
        mutate(sum_connections_with_same_papers=sum(same_papers)) %>%
        mutate(sum_connections_with_diff_papers=sum(diff_papers)) %>%
        select(
            pid, 
            connections, 
            sum_connections_with_same_ge_07,
            sum_connections_with_diff_ge_07,
            sum_connections_with_same_papers,
            sum_connections_with_diff_papers,
            min_expected_connections_by_semantic_similarity,
        ) %>%
        unique()
    return (d)
}


join_from_and_to <- function(ds) {
    ds_from <- ds %>%
        mutate(to=a_pid, from=c_pid)
    ds_to <- ds %>%
        mutate(from=a_pid, to=c_pid)
    ds <- rbind(ds_from, ds_to) %>%
        select(
            from, to,
            same_ge_07,
            diff_ge_07,
            same_papers,
            diff_papers,
        ) %>%
        unique()
    return (ds)
}

get_graphic <- function(ds_metrics, g_path, csv_path) {
    ds <- ds_metrics %>%
        mutate(
            precision = VP / (VP + FP),
            sensibilidade = VP / (VP + FN),
            F1 = 2 * (precision * sensibilidade) / (precision + sensibilidade)
        )

    write.csv(ds, csv_path)

    conn <- connections_graphic(ds)
    vp <- vp_graphic(ds)
    fp <- fp_graphic(ds)
    fn <- fn_graphic(ds)

    g_precision <- d_graphic_precision(ds)
    g_recall <- d_graphic_recall(ds)
    g_f1score <- d_graphic_f1score(ds)
    # g_f1score_q <- qq(ds) %>%
    #     d_graphic_f1score_q()

    #g_p <- plot_grid(vp, fp, fn, nrow = 1)
    g <- plot_grid(vp, fp, fn, g_precision, g_recall, g_f1score, nrow = 3) %>%
        graphics(g_path, width=30, height=25)
}

add_metrics_x <- function(ds) {
    ds3 <- ds
    ds3 <- ds3 %>%
        select(
            min_expected_connections_by_semantic_similarity,
            sum_connections_with_same_papers,
            sum_connections_with_diff_ge_07,
            sum_connections_with_same_ge_07,
            connections
        )
    ds3 <- ds3 %>%
        mutate(VP = sum_connections_with_same_papers + sum_connections_with_diff_ge_07)
    ds3 <- ds3 %>%
        mutate(FP = connections - VP)
    ds3 <- ds3 %>%
        mutate(FN = min_expected_connections_by_semantic_similarity - sum_connections_with_same_papers)
    ds3 <- ds3 %>%
        select(
            VP, FP, FN, 
            connections, 
            min_expected_connections_by_semantic_similarity, 
            sum_connections_with_same_papers, 
            sum_connections_with_same_ge_07,
            sum_connections_with_diff_ge_07
        )
    write.csv(ds3, "_rep_/metrics4_x.csv")
    return (ds3)
}

add_metrics <- function(ds) {
    d <- ds %>%
        mutate(
            VP=sum_connections_with_same_ge_07 + sum_connections_with_diff_ge_07,
            FP=connections - VP,
            FN=min_expected_connections_by_semantic_similarity - sum_connections_with_same_papers
        ) %>% 
        select(
            VP, FP, FN, 
            connections, 
            min_expected_connections_by_semantic_similarity, 
            sum_connections_with_same_papers, 
            sum_connections_with_same_ge_07,
            sum_connections_with_diff_ge_07
        ) %>%
        write_csv_file("_rep_/metrics3_todos_only.csv")
    return (d)
}

add_metrics_to_same <- function(ds) {
    d <- ds %>%
        # mutate(
        #     VP=sum_connections_with_same_papers,
        #     FP=connections - VP,
        #     FN=min_expected_connections_by_semantic_similarity - sum_connections_with_same_papers
        # ) %>% 
        mutate(
            VP=sum_connections_with_same_ge_07,
            FP=connections - VP,
            FN=min_expected_connections_by_semantic_similarity - sum_connections_with_same_papers
        ) %>% 
        select(
            VP, FP, FN, 
            connections,
            min_expected_connections_by_semantic_similarity,
            sum_connections_with_same_papers
        ) %>%
        write_csv_file("_rep_/metrics1_same_only.csv")
    return (d)
}

add_metrics_to_diff <- function(ds) {

    d <- ds %>%
        mutate(
            VP=sum_connections_with_diff_ge_07,
            FP=connections - VP,
            FN=0
        ) %>% 
        select(VP, FP, FN, connections, sum_connections_with_diff_ge_07) %>%
        write_csv_file("_rep_/metrics2_diff_only.csv")
    return (d)
}

qq <- function(ds_metrics) {
    a <- ds_metrics %>%
        mutate(
            q=case_when(
                precision == 0 ~ "0",
                precision > 0 & precision <= 0.1 ~ "0,1",
                precision > 0.1 & precision <= 0.2 ~ "0,2",
                precision > 0.2 & precision <= 0.3 ~ "0,3",
                precision > 0.3 & precision <= 0.4 ~ "0,4",
                precision > 0.4 & precision <= 0.5 ~ "0,5",
                precision > 0.5 & precision <= 0.6 ~ "0,6",
                precision > 0.6 & precision <= 0.7 ~ "0,7",
                precision > 0.7 & precision <= 0.8 ~ "0,8",
                precision > 0.8 & precision <= 0.9 ~ "0,9",
                precision > 0.9 & precision <= 1 ~ "1",
                precision == 1 ~ "1",
                TRUE ~ "?"
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

ds_papers <- read_csv_file(PAPERS_CSV_FILE_PATH)
ds_papers_added <- papers_add_highly_similar_mark(ds_papers)
write_csv_file(ds_papers_added, ADD_PAPERS_CSV_FILE_PATH)

ds_connections <- read_csv_file(CONNECTIONS_FILE_PATH)
ds_connections_added <- connections_add_numbers(ds_connections)
write_csv_file(ds_connections_added, ADD_CONNECTIONS_FILE_PATH)

# joined_from_and_to <- join_from_and_to(ds_connections_added)
# write_csv_file(joined_from_and_to, "joined_from_and_to.csv")

joined_from_and_to <- ds_connections_added %>% rename(pid=a_pid)

ds <- left_join(ds_papers_added, joined_from_and_to, by="pid")
write_csv_file(ds, "zz_00.csv")
ds <- connections_summarize(ds)
write_csv_file(ds, "zz_01.csv")
ds[is.na(ds)] <- 0
write_csv_file(ds, "zz_02.csv")


print(nrow(ds))

ds <- ds %>%
    filter(connections > 0)
print(nrow(ds))

# dsm1 <- add_metrics_to_same(ds) %>%
#     get_graphic(METRICS1, "_rep_/m1.csv")
# dsm2 <- add_metrics_to_diff(ds) %>%
#     get_graphic(METRICS2, "_rep_/m2.csv")
dsm3 <- add_metrics(ds) %>%
    get_graphic(METRICS3, "_rep_/m3.csv")
# dsm4 <- add_metrics_x(ds) %>%
#     get_graphic(METRICS4, "_rep_/m4.csv")

