#########################################################
#
# Figure 8c
#
#
#########################################################

library(ggplot2)
library(gridExtra)
library(tidyverse)
library(hrbrthemes)
library(cowplot)
library(viridis)
source("libs/my_functions_papers_analysis.R")
source("libs/my_functions_graphics.R")


papers_add_data <- function(ds) {
    # pid,refs,refs_with_doi,connections,score_gt_89,score_gt_79,score_gt_69,score_gt_59,score_gt_1_59,score_lt_1,score_none,no_en,en_only,with_en
    # S0004-282X1951000100001_it,9,0,1,0,1,0,0,0,0,0,1,0,0
    # S0004-282X1956000300001_pt,15,0,2,0,1,1,0,0,0,0,1,0,0
    # S0004-282X1965000200001_pt,75,0,2,0,2,0,0,0,0,0,1,0,0
    # S0004-282X1974000200003_pt,19,0,2,0,2,0,0,0,0,0,1,0,0
    ds <- ds %>%
        mutate(real_a_pid=substr(pid, 1, 23))

    pids <- ds %>%
        group_by(real_a_pid) %>%
        mutate(
            abstracts=n(),
        ) %>%
        filter(abstracts==1) %>%
        select(real_a_pid) %>%
        distinct() %>%
        write_csv_file("s3_pids.csv")

    ds <- ds %>%
        group_by(real_a_pid) %>%
        mutate(
            min_expected_connections_by_semantic_similarity=n()-1,
            abstracts=n(),
        ) %>%
        ungroup()
    print(head(ds))
    return (ds)
}


papers_profiles <- function(ds) {
    # pid,refs,refs_with_doi,connections,score_gt_89,score_gt_79,score_gt_69,score_gt_59,score_gt_1_59,score_lt_1,score_none,no_en,en_only,with_en
    # S0004-282X1951000100001_it,9,0,1,0,1,0,0,0,0,0,1,0,0
    # S0004-282X1956000300001_pt,15,0,2,0,1,1,0,0,0,0,1,0,0
    # S0004-282X1965000200001_pt,75,0,2,0,2,0,0,0,0,0,1,0,0
    # S0004-282X1974000200003_pt,19,0,2,0,2,0,0,0,0,0,1,0,0
    ds <- ds %>%
        select(real_a_pid, abstracts) %>%
        distinct()
    print("papers:")
    print(nrow(ds))
    return (ds)
}


g_papers_profiles <- function(ds, g_path) {
    g <- ds %>%
        ggplot(aes(x=abstracts)) +
        geom_bar(aes(y = after_stat(count), x=abstracts, fill=as.factor(abstracts)), stat = 'count') +
        geom_text(aes(label = after_stat(count)), vjust = -1, stat = "count") +
        labs(
            x = "Quantidade de resumos",
            y = "Quantidade de artigos"
        ) +
        theme_bw() +
        scale_fill_viridis_d(option = 'magma') +
        theme(text = element_text(size = 15), legend.position = "none")
    
    save_g(g, g_path)
    return (g)
}


get_langs_pairs_and_scores <- function(ds) {
    # a_pid,c_pid,real_a_pid,real_c_pid,same,c_score,c_score_none
    # S0004-282X1951000100001_it,S0004-282X1951000100001_fr,S0004-282X1951000100001,S0004-282X1951000100001,1,0.89,0
    # S0004-282X1956000300001_pt,S0004-282X1956000300001_en,S0004-282X1956000300001,S0004-282X1956000300001,1,0.77,0
    # S0004-282X1956000300001_pt,S0004-282X1956000300001_de,S0004-282X1956000300001,S0004-282X1956000300001,1,0.83,0
    # S0004-282X1965000200001_pt,S0004-282X1965000200001_en,S0004-282X1965000200001,S0004-282X1965000200001,1,0.85,0
    # S0004-282X1965000200001_pt,S0004-282X1965000200001_de,S0004-282X1965000200001,S0004-282X1965000200001,1,0.89,0
    # S0004-282X1974000200003_pt,S0004-282X1974000200003_en,S0004-282X1974000200003,S0004-282X1974000200003,1,0.87,0
    # S0004-282X1974000200003_pt,S0004-282X1974000200003_de,S0004-282X1974000200003,S0004-282X1974000200003,1,0.89,0
    ds <- ds %>%
        mutate(la = substr(a_pid, 25, 27), lb=substr(c_pid, 25, 27)) %>%
        mutate(l1=case_when(la < lb ~ la, TRUE ~ lb), l2=case_when(la < lb ~ lb, TRUE ~ la)) %>%
        select(l1, l2, c_score, same)
    return (ds)
}

add_median_min_max <- function(ds) {
    # a_pid,c_pid,real_a_pid,real_c_pid,same,c_score,c_score_none
    # S0004-282X1951000100001_it,S0004-282X1951000100001_fr,S0004-282X1951000100001,S0004-282X1951000100001,1,0.89,0
    # S0004-282X1956000300001_pt,S0004-282X1956000300001_en,S0004-282X1956000300001,S0004-282X1956000300001,1,0.77,0
    # S0004-282X1956000300001_pt,S0004-282X1956000300001_de,S0004-282X1956000300001,S0004-282X1956000300001,1,0.83,0
    # S0004-282X1965000200001_pt,S0004-282X1965000200001_en,S0004-282X1965000200001,S0004-282X1965000200001,1,0.85,0
    # S0004-282X1965000200001_pt,S0004-282X1965000200001_de,S0004-282X1965000200001,S0004-282X1965000200001,1,0.89,0
    # S0004-282X1974000200003_pt,S0004-282X1974000200003_en,S0004-282X1974000200003,S0004-282X1974000200003,1,0.87,0
    # S0004-282X1974000200003_pt,S0004-282X1974000200003_de,S0004-282X1974000200003,S0004-282X1974000200003,1,0.89,0

    translations <- read_csv_file('LANGS.csv')

    ds <- ds %>% 
        rename(lang=l1) %>%
        inner_join(translations, by="lang") %>%
        rename(lt1=lang_text)  %>%
        select(lt1, l2, c_score, same) 

    ds <- ds %>% 
        rename(lang=l2) %>%
        inner_join(translations, by="lang") %>%
        rename(lt2=lang_text)  %>%
        select(lt1, lt2, c_score, same)

    ds <- ds %>%
        group_by(lt1, lt2, same) %>%
        summarize(
            median=median(c_score),
            min=min(c_score),
            max=max(c_score),
            n=n(),
            c_score=c_score,
        ) %>%
        mutate(text = paste0("min: ", min, "\n", "max: ", max, "\n", "n: ", n)) %>%
        ungroup()
    return (ds)
}

g_langs_pairs_and_scores <- function(ds, g_path, title, subtitle, legend, text_color="white") {
    # a_pid,c_pid,real_a_pid,real_c_pid,same,c_score,c_score_none
    # S0004-282X1951000100001_it,S0004-282X1951000100001_fr,S0004-282X1951000100001,S0004-282X1951000100001,1,0.89,0
    # S0004-282X1956000300001_pt,S0004-282X1956000300001_en,S0004-282X1956000300001,S0004-282X1956000300001,1,0.77,0
    # S0004-282X1956000300001_pt,S0004-282X1956000300001_de,S0004-282X1956000300001,S0004-282X1956000300001,1,0.83,0
    # S0004-282X1965000200001_pt,S0004-282X1965000200001_en,S0004-282X1965000200001,S0004-282X1965000200001,1,0.85,0
    # S0004-282X1965000200001_pt,S0004-282X1965000200001_de,S0004-282X1965000200001,S0004-282X1965000200001,1,0.89,0
    # S0004-282X1974000200003_pt,S0004-282X1974000200003_en,S0004-282X1974000200003,S0004-282X1974000200003,1,0.87,0
    # S0004-282X1974000200003_pt,S0004-282X1974000200003_de,S0004-282X1974000200003,S0004-282X1974000200003,1,0.89,0

    ds['c_score'] <- NULL
    g <- ds %>%
        distinct() %>%
        ggplot(aes(lt1, lt2, fill=metric)) + 
        geom_tile(color = text_color) +
        geom_text(aes(label = metric), color = text_color , size = 10) +
        scale_fill_viridis(option="turbo", discrete=FALSE) +
        # scale_fill_viridis_c() +
        labs(
            title = title,
            subtitle = subtitle,
            x = "",
            y = ""
        ) +
        theme(
            plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
            legend.position = "none",
            title = element_text(size=38),
            axis.text.x = element_text(angle = 90, size = 36),
            axis.text.y = element_text(size = 36),
            aspect.ratio = 1
        ) 
    save_g(g, g_path)
    return (g)
}

readRenviron("envs/results_pares.env")
# G_PRECISION <- Sys.getenv("G_PRECISION")
# G_RECALL <- Sys.getenv("G_RECALL")
# G_F1SCORE <- Sys.getenv("G_F1SCORE")
# METRICS <- Sys.getenv("METRICS")
# RECALL_FILE_PATH <- Sys.getenv("RECALL_FILE_PATH")
# G_RECALL2 <- Sys.getenv("G_RECALL2")


PAPERS_CSV_FILE_PATH <- Sys.getenv("PAPERS_CSV_FILE_PATH")
G_PROFILES_CSV_FILE_PATH <- Sys.getenv("G_PROFILES_CSV_FILE_PATH")

CONNECTIONS_FILE_PATH <- Sys.getenv("CONNECTIONS_FILE_PATH")
G_CONNECTIONS_FILE_PATH <- Sys.getenv("G_CONNECTIONS_FILE_PATH")
# ADD_PAPERS_CSV_FILE_PATH <- Sys.getenv("ADD_PAPERS_CSV_FILE_PATH")
# ADD_CONNECTIONS_FILE_PATH <- Sys.getenv("ADD_CONNECTIONS_FILE_PATH")

ds_papers <- read_csv_file(PAPERS_CSV_FILE_PATH) %>%
    papers_add_data() %>%
    papers_profiles() %>%
    g_papers_profiles(G_PROFILES_CSV_FILE_PATH)
    
ds_connections <- read_csv_file(CONNECTIONS_FILE_PATH) %>%
    filter(c_score_none == 0)
print("nrows ds_connections: ")
print(nrow(ds_connections))

g_same <- ds_connections %>%
    filter(same == 1) %>%
    write_csv_file("ds_same.csv")
print("nrows g_same: ")
print(nrow(g_same))

g_diff <- ds_connections %>%
    filter(same == 0) %>%
    write_csv_file("ds_diff.csv")

print("nrows g_diff: ")
print(nrow(g_diff))


ds <- ds_connections %>%
    get_langs_pairs_and_scores() %>%
    add_median_min_max()  %>%
    write_csv_file("connections_metrics.csv")



g_same_median <- ds %>%
    filter(same == 1) %>%
    mutate(metric=median) %>%
    g_langs_pairs_and_scores(
        "g_same_mediana.jpg",
        "Medianas % de similaridade semântica",
        "Resumos oriundos de mesmo artigo",
        "medianas"
    )
    
g_same_min <- ds %>%
    filter(same == 1) %>%
    mutate(metric=min) %>%
    g_langs_pairs_and_scores(
        "g_same_min.jpg",
        "Mínimos % de similaridade semântica",
        "Resumos oriundos de mesmo artigo",
        "mínimos"
    )

g_same_max <- ds %>%
    filter(same == 1) %>%
    mutate(metric=max) %>%
    g_langs_pairs_and_scores(
        "g_same_max.jpg",
        "Máximos % de similaridade semântica",
        "Resumos oriundos de mesmo artigo",
        "máximos"
    )

g_same_n <- ds %>%
    filter(same == 1) %>%
    mutate(metric=n) %>%
    g_langs_pairs_and_scores(
        "g_same_n.jpg",
        "Quantidade de conexões",
        "Resumos oriundos de mesmo artigo",
        "conexões"
    )



    
g_diff_median <- ds %>%
    filter(same == 0) %>%
    mutate(metric=median) %>%
    g_langs_pairs_and_scores(
        "g_diff_mediana.jpg",
        "Medianas % de similaridade semântica",
        "Resumos oriundos de artigos diferentes",
        "medianas"
    )
    
g_diff_min <- ds %>%
    filter(same == 0) %>%
    mutate(metric=min) %>%
    g_langs_pairs_and_scores(
        "g_diff_min.jpg",
        "Mínimos % de similaridade semântica",
        "Resumos oriundos de artigos diferentes",
        "mínimos"
    )

g_diff_max <- ds %>%
    filter(same == 0) %>%
    mutate(metric=max) %>%
    g_langs_pairs_and_scores(
        "g_diff_max.jpg",
        "Máximos % de similaridade semântica",
        "Resumos oriundos de artigos diferentes",
        "máximos"
    )

g_diff_n <- ds %>%
    filter(same == 0) %>%
    mutate(metric=n) %>%
    g_langs_pairs_and_scores(
        "g_diff_n.jpg",
        "Quantidade de conexões",
        "Resumos oriundos de artigos diferentes",
        "conexões"
    )



g <- grid.arrange(g_same_n, g_diff_n, nrow = 1) %>%
    graphics("g_connections_numbers.jpg", width=80, height=40)
g <- grid.arrange(g_same_median, g_diff_median, nrow = 1) %>%
    graphics("g_connections_median.jpg", width=80, height=40)

g <- grid.arrange(g_same_min, g_diff_min, nrow = 1) %>%
    graphics("g_connections_min.jpg", width=80, height=40)

g <- grid.arrange(g_same_max, g_diff_max, nrow = 1) %>%
    graphics("g_connections_max.jpg", width=80, height=40)

# g <- grid.arrange(g_same_min, g_same_max, nrow = 1) %>%
#     graphics("g_connections_same2.jpg")

# g <- grid.arrange(g_diff_n, g_diff_median, g_diff_min, g_diff_max, nrow = 2) %>%
#     graphics("g_connections_diff.jpg")
# g <- grid.arrange(g_diff_n, g_diff_median, nrow = 1) %>%
#     graphics("g_connections_diff1.jpg")
# g <- grid.arrange(g_diff_min, g_diff_max, nrow = 1) %>%
#     graphics("g_connections_diff2.jpg")



# g <- grid.arrange(g_same_n, g_same_median, g_same_min, g_same_max, nrow = 2) %>%
#     graphics("g_connections_same.jpg")
# g <- grid.arrange(g_same_n, g_same_median, nrow = 1) %>%
#     graphics("g_connections_same1.jpg")
# g <- grid.arrange(g_same_min, g_same_max, nrow = 1) %>%
#     graphics("g_connections_same2.jpg")

# g <- grid.arrange(g_diff_n, g_diff_median, g_diff_min, g_diff_max, nrow = 2) %>%
#     graphics("g_connections_diff.jpg")
# g <- grid.arrange(g_diff_n, g_diff_median, nrow = 1) %>%
#     graphics("g_connections_diff1.jpg")
# g <- grid.arrange(g_diff_min, g_diff_max, nrow = 1) %>%
#     graphics("g_connections_diff2.jpg")
