#########################################################
#
# Figure 9
#
#
#########################################################

source("libs/my_functions_files.R")
source("libs/my_functions_graphics.R")
library(ggplot2)
library(tidyverse)
library(hrbrthemes)

d_min_max_connections <- function(path) {
    ds <- read_csv_file(path) %>%
        filter(c_score_none == 0) %>%
        group_by(pid) %>%
        summarise(min_score=min(c_score), max_score=max(c_score)) %>%
        select(min_score, max_score) %>%
        pivot_longer(c("min_score", "max_score"), names_to = "score_type",
        values_to = "score_value", values_drop_na = FALSE)
    return (ds)
}

d_min_max_connections_a <- function(path) {
    ds <- read_csv_file(path) %>%
        dplyr::filter(c_score_none == 0) %>%
        group_by(a_pid)

    print(nrow(ds))
    ds <- ds %>%
        summarise(min_score=min(c_score), max_score=max(c_score)) %>%
        select(min_score, max_score) %>%
        pivot_longer(c("min_score", "max_score"), names_to = "score_type",
        values_to = "score_value", values_drop_na = FALSE)

    return (ds)
}

d_graphic_min_max <- function(ds) {
    p <- ds %>%
      ggplot( aes(x=score_value, fill=score_type)) +
        geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
        scale_fill_manual(values=c("#69b3a2", "#404080")) +
        theme_ipsum() +
        labs(fill="")
    return (p)
}

readRenviron("envs/results_covid.env")
CONNECTIONS_FILE_PATH <- Sys.getenv("CONNECTIONS_FILE_PATH")
G_MIN_MAX_FILE_PATH <- Sys.getenv("G_MIN_MAX_FILE_PATH")
G_MIN_MAX_DS_FILE_PATH <- Sys.getenv("G_MIN_MAX_DS_FILE_PATH")
g <- d_min_max_connections_a(CONNECTIONS_FILE_PATH) %>%
        write_csv_file(G_MIN_MAX_DS_FILE_PATH) %>%
        d_graphic_min_max()
save_g(g, G_MIN_MAX_FILE_PATH)


readRenviron("envs/results_no_english.env")
CONNECTIONS_FILE_PATH <- Sys.getenv("CONNECTIONS_FILE_PATH")
G_MIN_MAX_FILE_PATH <- Sys.getenv("G_MIN_MAX_FILE_PATH")
G_MIN_MAX_DS_FILE_PATH <- Sys.getenv("G_MIN_MAX_DS_FILE_PATH")
g <- d_min_max_connections(CONNECTIONS_FILE_PATH) %>%
        write_csv_file(G_MIN_MAX_DS_FILE_PATH) %>%
        d_graphic_min_max()
save_g(g, G_MIN_MAX_FILE_PATH)


readRenviron("envs/results_pares.env")
CONNECTIONS_FILE_PATH <- Sys.getenv("CONNECTIONS_FILE_PATH")
G_MIN_MAX_FILE_PATH <- Sys.getenv("G_MIN_MAX_FILE_PATH")
G_MIN_MAX_DS_FILE_PATH <- Sys.getenv("G_MIN_MAX_DS_FILE_PATH")

g <- d_min_max_connections_a(CONNECTIONS_FILE_PATH) %>%
        write_csv_file(G_MIN_MAX_DS_FILE_PATH) %>%
        d_graphic_min_max()
save_g(g, G_MIN_MAX_FILE_PATH)


