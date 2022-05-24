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

d_min_max_connections_a_pid <- function(path, name) {
    ds <- read_csv_file(path) %>%
        filter(c_score_none == 0) %>%
        rename(pid=a_pid) %>%
        mutate(total=sum(n())) %>%
        group_by(pid, total) %>%
        summarise(min_score=min(c_score), max_score=max(c_score)) %>%
        select(pid, min_score, max_score, total) %>%
        pivot_longer(cols=2:3, names_to="score_level", values_to="score") %>%
        group_by(score_level, score, total) %>%
        summarise(n=n(), perc=n * 100 / total) %>%
        select(score_level, score, perc, n, total)
    ds['name'] <- name
    return (ds)
}

d_min_max_connections <- function(path, name) {
    ds <- read_csv_file(path) %>%
        filter(c_score_none == 0) %>%
        mutate(total=sum(n())) %>%
        group_by(pid, total) %>%
        summarise(min_score=min(c_score), max_score=max(c_score)) %>%
        select(pid, min_score, max_score, total) %>%
        pivot_longer(cols=2:3, names_to="score_level", values_to="score") %>%
        group_by(score_level, score, total) %>%
        summarise(n=n(), perc=n * 100 / total) %>%
        select(score_level, score, perc, n, total)
    ds['name'] <- name
    return (ds)
}


d_graphic_min_max <- function(ds) {
    ds$score <- as.numeric(as.vector(ds$score))
    p <- ds %>%
        mutate(group_name=paste(name, score_level, sep=" | ")) %>%
        ggplot( aes(x=score, y=perc, group=group_name, color=group_name)) +
        geom_line() +
        scale_color_viridis(discrete=TRUE, option="turbo") +
        
        theme_ipsum()
    return (p)
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
g1 <- d_min_max_connections(S1_CONNECTIONS, "A") %>%
        write_csv_file(S1_CONNECTIONS_fixed)

g2 <- d_min_max_connections_a_pid(S2_CONNECTIONS, "B") %>%
        write_csv_file(S2_CONNECTIONS_fixed)

g3 <- d_min_max_connections_a_pid(S3_CONNECTIONS, "C") %>%
        write_csv_file(S3_CONNECTIONS_fixed)

g <- rbind(g1, g2, g3) %>%
    write_csv_file("all_samples.csv")

g1 <- d_graphic_min_max(g) %>% graphics(G2_MIN_MAX_FILE_PATH, width=40, height=40)


