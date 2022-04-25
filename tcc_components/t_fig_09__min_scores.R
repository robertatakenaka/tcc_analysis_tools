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

d_min_connections <- function(path) {
    ds <- read_csv_file(path) %>%
        filter(c_score_none == 0) %>%
        group_by(pid) %>%
        summarise(min_score=min(c_score)) %>%
        select(min_score)
    return (ds)
}

d_min_connections_a <- function(path) {
    ds <- read_csv_file(path) %>%
        filter(c_score_none == 0) %>%
        group_by(a_pid) %>%
        summarise(min_score=min(c_score)) %>%
        select(min_score)
    return (ds)
}

d_graphic <- function(ds) {
    print(nrow(ds))
    g <- ds %>%
        ggplot(aes(x=min_score)) +
        geom_histogram(
            color = '#21918c', 
            aes(y = after_stat(count)), alpha = 0.01, position = "identity") +
            theme_bw() 
    return (g)
}


readRenviron("envs/results_covid.env")
CONNECTIONS_FILE_PATH <- Sys.getenv("CONNECTIONS_FILE_PATH")
G_MIN_FILE_PATH <- Sys.getenv("G_MIN_FILE_PATH")
G_MIN_DS_FILE_PATH <- Sys.getenv("G_MIN_DS_FILE_PATH")
g <- d_min_connections_a(CONNECTIONS_FILE_PATH) %>%
        write_csv_file(G_MIN_DS_FILE_PATH) %>%
        d_graphic()
save_g(g, G_MIN_FILE_PATH)


readRenviron("envs/results_no_english.env")
CONNECTIONS_FILE_PATH <- Sys.getenv("CONNECTIONS_FILE_PATH")
G_MIN_FILE_PATH <- Sys.getenv("G_MIN_FILE_PATH")
G_MIN_DS_FILE_PATH <- Sys.getenv("G_MIN_DS_FILE_PATH")
g <- d_min_connections(CONNECTIONS_FILE_PATH) %>%
        write_csv_file(G_MIN_DS_FILE_PATH) %>%
        d_graphic()
save_g(g, G_MIN_FILE_PATH)


readRenviron("envs/results_pares.env")
CONNECTIONS_FILE_PATH <- Sys.getenv("CONNECTIONS_FILE_PATH")
G_MIN_FILE_PATH <- Sys.getenv("G_MIN_FILE_PATH")
G_MIN_DS_FILE_PATH <- Sys.getenv("G_MIN_DS_FILE_PATH")

g <- d_min_connections_a(CONNECTIONS_FILE_PATH) %>%
        write_csv_file(G_MIN_DS_FILE_PATH) %>%
        d_graphic()
save_g(g, G_MIN_FILE_PATH)
