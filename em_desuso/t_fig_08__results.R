#########################################################
#
# Figure 8
#
#
#########################################################

source("my_functions_papers_analysis.R")
library(ggplot2)


add_precision <- function(ds_papers) {
    t_recommended_papers = sum(ds_papers$connections)
    ds_papers <- ds_papers %>%
        filter(connections > 0) %>%
        mutate(
            n_relevant_papers=score_gt_69 + score_gt_79 + score_gt_89,
            t_recommended_papers=connections
        ) %>%
        mutate(precision=n_relevant_papers/t_recommended_papers)
    return (ds_papers)
}


add_recall <- function(ds_papers) {
    t_relevant_papers = sum(ds_papers$score_gt_69) +
                           sum(ds_papers$score_gt_79) +
                           sum(ds_papers$score_gt_89)
    ds_papers <- ds_papers %>%
        mutate(recall=n_relevant_papers/(connections - score_none))
    return (ds_papers)
}


add_f1 <- function(ds_papers) {
    ds_papers <- ds_papers %>%
        mutate(f1=(2 * precision * recall) / (precision + recall))
    return (ds_papers)
}


add_n_occurrences <- function(ds_papers) {
    ds_papers <- ds_papers %>%
        mutate(prefix=substr(pid, 1, 23)) %>%
        group_by(prefix) %>%
        mutate(n_occurrences=n()) %>%
        ungroup()
        
    return (ds_papers)
}


precision_graphic <- function(ds_papers) {

    g <- ds_papers %>%
        ggplot(aes(precision)) +
        geom_histogram(aes(y = after_stat(count))) +
        #scale_x_log10() +
        scale_y_log10()+
        theme_bw()

    return (g)
}


recall_graphic <- function(ds_papers) {
    
    g <- ds_papers %>%
        ggplot(aes(recall)) +
        geom_histogram(aes(y = after_stat(count))) +
        #scale_x_log10() +
        scale_y_log10() +
        theme_bw()
    
    return (g)
}


f1_graphic <- function(ds_papers) {
    
    g <- ds_papers %>%
        ggplot(aes(f1)) +
        geom_histogram(aes(y = after_stat(count))) +
        #scale_x_log10() +
        scale_y_log10() +
        theme_bw()
    
    return (g)
}


readRenviron("envs/results_pares.env")

PAPERS_CSV_FILE_PATH <- Sys.getenv("PAPERS_CSV_FILE_PATH")
results_pares <- read_csv_file(PAPERS_CSV_FILE_PATH) %>%
    add_precision() %>%
    add_recall() %>%
    add_f1() %>%
    add_n_occurrences() %>%
    filter(n_occurrences > 1)
g <- precision_graphic(results_pares)
g

g_recall <- recall_graphic(results_pares)
g_recall

g_f1 <- f1_graphic(results_pares)
g_f1
