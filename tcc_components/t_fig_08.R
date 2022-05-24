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

library(ggplot2)
library(gridExtra)
library(tidyverse)
library(hrbrthemes)

library(cowplot)
library(viridis)
library("dplyr")

source("./libs/my_functions_papers_analysis.R")
source("./libs/my_functions_graphics.R")

d_graphic <- function(ds, xlabel, title, size=20) {
    print(head(ds))
    g <- ds %>%
        ggplot(aes(x=item)) +
        # scale_y_continuous(trans='log10') +
        geom_bar(aes(y = after_stat(count), x=item, fill=as.factor(item)), stat = 'count') +
        geom_text(aes(label = after_stat(count)), vjust = -1, stat = "count") +
        labs(
            title = title,
            x = xlabel,
            y = "Quantidade de artigos"
        ) +
        theme_bw() +
        scale_fill_viridis_d(option = 'turbo') +
        theme(text = element_text(size = size), legend.position = "none")
    return (g)
}
numbers <- function(ds, name, filename, size=30) {
    g_min <- ds %>%
            mutate(item=min_score) %>%
            d_graphic("menor coeficiente de similaridade por artigo", name, size)

    g_max <- ds %>%
            mutate(item=max_score) %>%
            d_graphic("maior coeficiente de similaridade por artigo", name, size)

    g_n <- ds %>%
            mutate(item=connections) %>%
            d_graphic("quantidade de conexões por artigo", name, size)

    g_lang <- ds %>%
            mutate(item=lang_text) %>%
            d_graphic("idioma do artigo", name, size)

    g <- grid.arrange(g_min, g_max, g_n, g_lang, nrow = 2) %>%
        graphics(filename, width=45, height=45)
    return(g)
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

    translations <- read_csv_file('./datasets/LANGS.csv')

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

g_langs_pairs_and_scores <- function(ds, g_path, title, subtitle, legend, text_color="white", size=10) {
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
            title = element_text(size=size),
            axis.text.x = element_text(angle = 90, size = size),
            axis.text.y = element_text(size = size),
            aspect.ratio = 1
        ) 
    save_g(g, g_path)
    return (g)
}

readRenviron("envs/results_pares.env")

CONNECTIONS_FILE_PATH <- Sys.getenv("CONNECTIONS_FILE_PATH")
ONE_CSV_FILE_PATH <- Sys.getenv("ONE_CSV_FILE_PATH")
PAPERS_CSV_FILE_PATH <- Sys.getenv("PAPERS_CSV_FILE_PATH")
PAPERS_REFS_CSV_FILE_PATH <- Sys.getenv("PAPERS_REFS_CSV_FILE_PATH")
# a_pid,c_pid,real_a_pid,real_c_pid,same,c_score,c_score_none
# S0004-05922014000300016_it,S0004-05922014000300016_en,S0004-05922014000300016,S0004-05922014000300016,1,0.76,0
# S0004-05922014000300016_en,S0004-05922014000300016_it,S0004-05922014000300016,S0004-05922014000300016,1,0.76,0

ds_conn <- read_csv_file(CONNECTIONS_FILE_PATH)
ds_papers_refs <- read_csv_file(PAPERS_REFS_CSV_FILE_PATH)
ds_papers <- read_csv_file(PAPERS_CSV_FILE_PATH)
translations <- read_csv_file('./datasets/LANGS.csv')

ds_n <- ds_papers_refs %>%
    mutate(real_a_pid=substr(pid, 1, 23)) %>%
    mutate(lang=substr(pid, 25, 26)) %>%
    inner_join(translations, by="lang") %>%
    group_by(real_a_pid) %>%
    mutate(n=n()) %>%
    write_csv_file("_rep_/f08_n_mutate.csv")

ds_unique <- ds_n %>%
    filter(n==1) %>%
    write_csv_file("_rep_/f08_unique.csv")

ds_multiple <- ds_n %>%
    filter(n>1) %>%
    write_csv_file("_rep_/f08_multiple.csv")

dj <- inner_join(ds_unique, ds_conn, by="real_a_pid") %>%
    write_csv_file("_rep_/f08_unique_and_cnx.csv") 

x<- dj   %>%
    numbers("Desempenho dos resumos sem par", "_rep_/f08_unique_and_cnx.jpg")


ds <- dj %>%
    get_langs_pairs_and_scores() %>%
    add_median_min_max()  %>%
    write_csv_file("_rep_/f08_unique_papers_metrics.csv")



f08_g_median <- ds %>%
    mutate(metric=median) %>%
    g_langs_pairs_and_scores(
        "_rep_/f08_unique_g_mediana.jpg",
        "Medianas % de similaridade semântica",
        "Resumos sem par",
        "medianas",
        size=40
    )
    
f08_g_min <- ds %>%
    mutate(metric=min) %>%
    g_langs_pairs_and_scores(
        "_rep_/f08_unique_g_min.jpg",
        "Mínimos % de similaridade semântica",
        "Resumos sem par",
        "mínimos",
        size=40
    )

f08_g_max <- ds %>%
    mutate(metric=max) %>%
    g_langs_pairs_and_scores(
        "_rep_/f08_unique_g_max.jpg",
        "Máximos % de similaridade semântica",
        "Resumos sem par",
        "máximos",
        size=40
    )

f08_g_n <- ds %>%
    mutate(metric=n) %>%
    g_langs_pairs_and_scores(
        "_rep_/f08_unique_g_n.jpg",
        "Quantidade de conexões",
        "Resumos sem par",
        "conexões",
        size=40
    )

g <- grid.arrange(f08_g_n, f08_g_median, f08_g_min, f08_g_max, nrow = 2) %>%
    graphics("_rep_/f08_unique_connections_numbers.jpg", width=40, height=40)

