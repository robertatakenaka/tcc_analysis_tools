#########################################################
#
# Figure 9
#
#
#########################################################

source("./libs/my_functions_files.R")
source("./libs/my_functions_graphics.R")
library(gridExtra)
library(ggplot2)
library(tidyverse)
library(hrbrthemes)

library(cowplot)
library(viridis)

options(digits = 10)
options(scipen = 999999999)


fix_connections_pid <- function(path) {
    ds <- read_csv_file(path) %>%
        filter(c_score_none == 0) %>%
        rename(pid=a_pid) %>%
        select(pid, c_score)
    return (ds)
}

fix_connections <- function(path) {
    ds <- read_csv_file(path) %>%
        filter(c_score_none == 0) %>%
        select(pid, c_score)
    return (ds)
}

d_min_max_connections <- function(ds) {
    ds <- ds %>%
        group_by(pid) %>%
        mutate(min_score=min(c_score), max_score=max(c_score), connections=n()) %>%
        mutate(diff = (max_score - min_score)) %>%
        select(pid, min_score, max_score, diff, connections) %>%
        distinct()
    print(head(ds))
    return (ds)
}

d_graphic <- function(ds, xlabel, title) {
    print(head(ds))
    g <- ds %>%
        ggplot(aes(x=item)) +
        scale_y_continuous(trans='log10') +
        geom_bar(aes(y = after_stat(count), x=item, fill=as.factor(item)), stat = 'count') +
        # geom_text(aes(label = after_stat(count)), vjust = -1, stat = "count") +
        labs(
            title = title,
            x = xlabel,
            y = "Quantidade de artigos"
        ) +
        theme_bw() +
        scale_fill_viridis_d(option = 'turbo') +
        theme(text = element_text(size = 15), legend.position = "none")
    return (g)
}

numbers <- function(ds, name, filename) {
    g_min <- ds %>%
            mutate(item=min_score) %>%
            d_graphic("menor valor do coeficiente de similaridade de cada artigo", name)

    g_max <- ds %>%
            mutate(item=max_score) %>%
            d_graphic("maior valor do coeficiente de similaridade de cada artigo", name)

    # g_n <- ds %>%
    #         mutate(item=connections) %>%
    #         d_graphic("quantidade de conexões", name)
    # g_nc <- ds %>%
    #         mutate(item=connections) %>%
    #         filter(min_score > 0.5) %>%
    #         d_graphic("quantidade de conexões | min > 50%", name)

    g <- grid.arrange(g_min, g_max, nrow = 2) %>%
        graphics(filename, width=15, height=30)
    return(g)
}

readRenviron("envs/min_max.env")
G2_MIN_MAX_FILE_PATH <- Sys.getenv("G2_MIN_MAX_FILE_PATH")

S1_CONNECTIONS <- Sys.getenv("S1_CONNECTIONS")
S2_CONNECTIONS <- Sys.getenv("S2_CONNECTIONS")
S3_CONNECTIONS <- Sys.getenv("S3_CONNECTIONS")

S1_CONNECTIONS_fixed <- Sys.getenv("S1_CONNECTIONS_fixed")
S2_CONNECTIONS_fixed <- Sys.getenv("S2_CONNECTIONS_fixed")
S3_CONNECTIONS_fixed <- Sys.getenv("S3_CONNECTIONS_fixed")

G_MIN_MAX_FILE_PATH <- Sys.getenv("G_MIN_MAX_FILE_PATH")
g_a <- fix_connections(S1_CONNECTIONS)%>%
        d_min_max_connections() %>%
        write_csv_file(S1_CONNECTIONS_fixed)

g_b <- fix_connections_pid(S2_CONNECTIONS)%>%
        d_min_max_connections() %>%
        write_csv_file(S2_CONNECTIONS_fixed)
        
g_c <- fix_connections_pid(S3_CONNECTIONS)%>%
        d_min_max_connections() %>%
        write_csv_file(S3_CONNECTIONS_fixed)
        

gs_a <- numbers(g_a, "A", "_rep_/min_max_a.jpg")

gs_b <- numbers(g_b, "B", "_rep_/min_max_b.jpg")

gs_c <- numbers(g_c, "C", "_rep_/min_max_c.jpg")



grid.arrange(gs_a, gs_b, gs_c, nrow = 1) %>%
         graphics("_rep_/min_max_all.jpg", width=40, height=20)
# grid.arrange(gs_b, nrow = 1) %>%
#         graphics("min_max_b.jpg", width=40, height=20)
# grid.arrange(gs_c, nrow = 1) %>%
#         graphics("min_max_c.jpg", width=40, height=20)


