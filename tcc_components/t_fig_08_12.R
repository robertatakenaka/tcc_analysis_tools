#########################################################
#
# Figure 8c
#
#
#########################################################

# install.packages("ggplot2")
# install.packages("gridExtra")
# install.packages("tidyverse")
# install.packages("hrbrthemes")
# install.packages("cowplot")
# install.packages("viridis")

library(ggplot2)
library(gridExtra)
library(tidyverse)
library(hrbrthemes)

library(cowplot)
library(viridis)

source("./libs/my_functions_papers_analysis.R")
source("./libs/my_functions_graphics.R")


one_abstract_papers_data <- function(ds) {
    # a_pid,c_pid,real_a_pid,real_c_pid,same,c_score,c_score_none
    # S0004-05922014000400016_it,S0004-05922014000300016_en,S0004-05922014000300016,S0004-05922014000300016,1,0.76,0
    # S0004-05922014000300016_en,S0004-05922014000300016_it,S0004-05922014000300016,S0004-05922014000300016,1,0.76,0

    ds <- ds %>%
        mutate(real_a_pid=substr(pid, 1, 23))

    pids <- ds %>%
        group_by(real_a_pid) %>%
        mutate(
            abstracts=n(),
        ) %>%
        filter(abstracts==1 | same == 0)
    return (ds)
}


papers_add_data <- function(ds) {
    # pid,refs,refs_with_doi,connections,score_gt_89,score_gt_79,score_gt_69,score_gt_59,score_gt_1_59,score_lt_1,score_none,no_en,en_only,with_en
    # S0004-282X1951000100001_it,9,0,1,0,1,0,0,0,0,0,1,0,0
    # S0004-282X1957000300001_pt,15,0,2,0,1,1,0,0,0,0,1,0,0
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
        write_csv_file("_rep_/s3_with_one_abstract_only.csv")

    ds <- ds %>%
        group_by(real_a_pid) %>%
        mutate(
            min_expected_connections_by_semantic_similarity=n()-1,
            abstracts=n(),
        ) %>%
        mutate(sum(min_expected_connections_by_semantic_similarity)) %>%
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
        # scale_y_continuous(trans='log10') +
        geom_bar(aes(y = after_stat(count), x=abstracts, fill=as.factor(abstracts)), stat = 'count') +
        # geom_text(aes(label = after_stat(count)), vjust = -1, stat = "count", size=3) +
        geom_text(
            aes(label=after_stat(count)),
            stat = "count",
            size=5,
            hjust = -0.2,
            position=position_dodge(width = 1)
        ) +
        labs(
            x = "Quantidade de resumos",
            y = "Quantidade de artigos"
        ) +
        # scale_y_continuous(trans='log2') +
        coord_flip() + 
        expand_limits(y = c(-0.1, 3000)) +
        theme_bw() +
        scale_fill_viridis_d(option = 'viridis') +
        theme(
            aspect.ratio=1 / 5,
            plot.margin = margin(1, 0, 0, 0, "cm"),
            text = element_text(size = 12), 
            legend.position = "none"
        )
    
    # save_g(g, g_path)
    graphics(g, g_path, width=18, height=5)
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
        select(a_pid, c_pid, same, c_score)
    ds <- ds %>%
        mutate(to=a_pid, from=c_pid) %>%
    # ds_to <- ds %>%
    #     mutate(from=a_pid, to=c_pid)
    # ds <- rbind(ds_from, ds_to) %>%
        select(
            from, to,
            same,
            c_score,
        ) %>%
        unique()

    ds <- ds %>%
        mutate(lang_from = substr(from, 25, 27), lang_to=substr(to, 25, 27))


    translations <- read_csv_file('./datasets/LANGS.csv')
    ds <- ds %>% 
        rename(lang=lang_from) %>%
        inner_join(translations, by="lang") %>%
        rename(lt1=lang_text) %>%
        select(from, to, same, c_score, lt1, lang_to)

    ds <- ds %>% 
        rename(lang=lang_to) %>%
        inner_join(translations, by="lang") %>%
        rename(lt2=lang_text) %>%
        select(from, to, same, c_score, lt1, lt2)


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

    ds <- ds %>%
        group_by(lt1, lt2, same) %>%
        summarise(
            median=median(c_score),
            min=min(c_score),
            max=max(c_score),
            n=n(),
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

    text_size <- 27
    cell_size <- 9
    if (ds$same[1] == 1){
        cell_size <- cell_size * 0.75
        # text_size <- 21
    }
    ds['c_score'] <- NULL
    g <- ds %>%
        distinct() %>%
        ggplot(aes(x=reorder(lt1, desc(lt1)), y=lt2, fill=metric)) + 
        geom_tile(color = text_color) +
        geom_text(aes(label = metric), color = text_color, size=cell_size) +
        scale_fill_viridis(option="turbo", discrete=FALSE) +
        coord_flip() + 
        # scale_fill_viridis_c() +
        labs(
            title = paste(title, subtitle, sep=" | "),
            x = "De",
            y = "Para"
        ) +
        theme(
            plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
            legend.position = "none",
            text = element_text(size=text_size),
            axis.text.x = element_text(angle = 90, size=text_size),
            axis.text.y = element_text(size=text_size),
            aspect.ratio = 1
        ) 
    # save_g(g, g_path)
    graphics(g, g_path, width=40, height=40)

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

PAPERS_ONE_ABSTRACT_CSV_FILE_PATH <- Sys.getenv("PAPERS_ONE_ABSTRACT_CSV_FILE_PATH")
G_PROFILES_ONE_ABSTRACT_CSV_FILE_PATH <- Sys.getenv("G_PROFILES_ONE_ABSTRACT_CSV_FILE_PATH")

CONNECTIONS_FILE_PATH <- Sys.getenv("CONNECTIONS_FILE_PATH")
G_CONNECTIONS_FILE_PATH <- Sys.getenv("G_CONNECTIONS_FILE_PATH")
# ADD_PAPERS_CSV_FILE_PATH <- Sys.getenv("ADD_PAPERS_CSV_FILE_PATH")
# ADD_CONNECTIONS_FILE_PATH <- Sys.getenv("ADD_CONNECTIONS_FILE_PATH")

ds_papers <- read_csv_file(PAPERS_CSV_FILE_PATH) %>%
    papers_add_data() %>%
    papers_profiles()

profile <- ds_papers %>%
    g_papers_profiles(G_PROFILES_CSV_FILE_PATH)
    

ds_connections <- read_csv_file(CONNECTIONS_FILE_PATH)

print("same")
ds_same <- ds_connections %>%
    filter(same == 1)
print(nrow(ds_same))
ds_same <- ds_same %>%
    filter(c_score_none == 0)
print(nrow(ds_same))
print("----")

print("diff")
ds_diff <- ds_connections %>%
    filter(same == 0)
print(nrow(ds_diff))
ds_diff <- ds_diff %>%
    filter(c_score_none == 0)
print(nrow(ds_diff))
print("----")


ds_semantic_connections <- ds_connections %>%
    filter(c_score_none == 0) %>%
    get_langs_pairs_and_scores() %>%
    write_csv_file("_rep_/connections_metrics.csv")


ds_semantic_connections <- add_median_min_max(ds_semantic_connections)

print("nrows ds_semantic_connections: ")
print(nrow(ds_semantic_connections))

ds_same <- ds_semantic_connections %>%
    filter(same == 1) %>%
    write_csv_file("_rep_/ds_same.csv")
print("nrows ds_same: ")
print(nrow(ds_same))

ds_diff <- ds_semantic_connections %>%
    filter(same == 0) %>%
    write_csv_file("_rep_/ds_diff.csv")

print("nrows ds_diff: ")
print(nrow(ds_diff))



g_same_median <- ds_same %>%
    filter(same == 1) %>%
    mutate(metric=median) %>%
    g_langs_pairs_and_scores(
        "_rep_/g_same_mediana.jpg",
        "Medianas",
        "Resumos oriundos de mesmo artigo",
        "medianas"
    )
    
g_same_min <- ds_same %>%
    filter(same == 1) %>%
    mutate(metric=min) %>%
    g_langs_pairs_and_scores(
        "_rep_/g_same_min.jpg",
        "Mínimos",
        "Resumos oriundos de mesmo artigo",
        "mínimos"
    )

g_same_max <- ds_same %>%
    filter(same == 1) %>%
    mutate(metric=max) %>%
    g_langs_pairs_and_scores(
        "_rep_/g_same_max.jpg",
        "Máximos",
        "Resumos oriundos de mesmo artigo",
        "máximos"
    )

g_same_n <- ds_same %>%
    filter(same == 1) %>%
    mutate(metric=n) %>%
    g_langs_pairs_and_scores(
        "_rep_/g_same_n.jpg",
        "Quantidade de conexões",
        "Resumos oriundos de mesmo artigo",
        "conexões"
    )



    
g_diff_median <- ds_diff %>%
    filter(same == 0) %>%
    mutate(metric=median) %>%
    g_langs_pairs_and_scores(
        "_rep_/g_diff_mediana.jpg",
        "Medianas",
        "Resumos oriundos de artigos diferentes",
        "medianas"
    )
    
g_diff_min <- ds_diff %>%
    filter(same == 0) %>%
    mutate(metric=min) %>%
    g_langs_pairs_and_scores(
        "_rep_/g_diff_min.jpg",
        "Mínimos",
        "Resumos oriundos de artigos diferentes",
        "mínimos"
    )

g_diff_max <- ds_diff %>%
    filter(same == 0) %>%
    mutate(metric=max) %>%
    g_langs_pairs_and_scores(
        "_rep_/g_diff_max.jpg",
        "Máximos",
        "Resumos oriundos de artigos diferentes",
        "máximos"
    )

g_diff_n <- ds_diff %>%
    filter(same == 0) %>%
    mutate(metric=n) %>%
    g_langs_pairs_and_scores(
        "_rep_/g_diff_n.jpg",
        "Quantidade de conexões",
        "Resumos oriundos de artigos diferentes",
        "conexões"
    )

g <- grid.arrange(g_same_n, g_diff_n, nrow = 2) %>%
    graphics("_rep_/v_g_connections_numbers.jpg", width=40, height=80)

g <- grid.arrange(g_same_median, g_diff_median, nrow = 2) %>%
    graphics("_rep_/v_g_connections_median.jpg", width=40, height=80)

g <- grid.arrange(g_same_min, g_diff_min, nrow = 2) %>%
    graphics("_rep_/v_g_connections_min.jpg", width=40, height=80)

g <- grid.arrange(g_same_max, g_diff_max, nrow = 2) %>%
    graphics("_rep_/v_g_connections_max.jpg", width=40, height=80)


# g <- grid.arrange(g_same_n, g_diff_n, nrow = 1) %>%
#     graphics("_rep_/g_connections_numbers.jpg", width=80, height=40)

# g <- grid.arrange(g_same_median, g_diff_median, nrow = 1) %>%
#     graphics("_rep_/g_connections_median.jpg", width=80, height=40)

# g <- grid.arrange(g_same_min, g_diff_min, nrow = 1) %>%
#     graphics("_rep_/g_connections_min.jpg", width=80, height=40)

# g <- grid.arrange(g_same_max, g_diff_max, nrow = 1) %>%
#     graphics("_rep_/g_connections_max.jpg", width=80, height=40)
