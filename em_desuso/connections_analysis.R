#########################################################
#
# Figure 9
#
#
#########################################################

source("my_functions_papers_analysis.R")
library(ggplot2)


d_graphic <- function(ds_papers) {
    
    g <- ds_papers %>%
        ggplot(aes(c_score)) +
        geom_histogram(aes(y = after_stat(count))) +
        #scale_x_log10() +
        scale_y_log10() + 
        theme_bw()
    
    return (g)
}

g_connections <- function(ds) {
    selection <- ds %>%
        filter(c_score_none == 0)
    g <- d_graphic(selection)
    return (g)
}

draw_graphic_min_max_score <- function(ds_g, title, file_path, nrow=1, arg_xlab="", arg_ylab="", aspect_ratio=1) {
      
     # With a bit more style
    g <- ds_g %>%
        ggplot(aes(x=min_max)) +
        geom_histogram(aes(y = n)) +
        #scale_x_log10() +
        theme_bw()
    
        
    save_g(g, paste(file_path, "pdf", sep="."))
    # save_g(g, paste(file_path, "svg", sep="."))
    
    svg_file <- paste(file_path, "svg", sep=".")
    svg(svg_file)
    plot(g)
    invisible(dev.off())
    return (g)

}

draw_graphic_max_score <- function(ds_g, title, file_path, nrow=1, arg_xlab="", arg_ylab="", aspect_ratio=1) {
      
     # With a bit more style
    g <- ds_g %>%
        ggplot(aes(range_max_score)) +
        geom_histogram(aes(y = after_stat(count))) +
        #scale_x_log10() +
        scale_y_log10() + 
        theme_bw()
    
        
    save_g(g, paste(file_path, "pdf", sep="."))
    # save_g(g, paste(file_path, "svg", sep="."))
    
    svg_file <- paste(file_path, "svg", sep=".")
    svg(svg_file)
    plot(g)
    invisible(dev.off())
    return (g)

}

draw_graphic_min_score <- function(ds_g, title, file_path, nrow=1, arg_xlab="", arg_ylab="", aspect_ratio=1) {
      
     # With a bit more style
    g <- ds_g %>%
        ggplot(aes(range_min_score)) +
        geom_histogram(aes(y = after_stat(count))) +
        #scale_x_log10() +
        scale_y_log10() +
        theme_bw()
    
        
    save_g(g, paste(file_path, "pdf", sep="."))
    # save_g(g, paste(file_path, "svg", sep="."))
    
    svg_file <- paste(file_path, "svg", sep=".")
    svg(svg_file)
    plot(g)
    invisible(dev.off())
    return (g)

}

paper_connections_min_max <- function(ds) {
    ds <- ds %>%
        mutate(pid=substr(pid, 1, 23)) %>%
        group_by(pid) %>%
        mutate(total_conn=n()) %>%
        mutate(total_candidates_to_sem_sim=total_conn-sum(c_score_none)) %>%
        mutate(cut=sum(c_score_none)) %>%
        mutate(min_score=min(c_score)) %>%
        mutate(max_score=max(c_score)) %>%
        select(pid, total_conn, total_candidates_to_sem_sim,
               min_score, max_score) %>%
        distinct()
    print(head(ds))
    return (ds)
}

paper_connections_min_and_max_ranges <- function(ds) {
    ds <- ds %>%
        mutate(range_min_score=case_when(
            min_score >= 0.9 ~ '>=90%',
            min_score >= 0.8 ~ '>=80%',
            min_score >= 0.7 ~ '>=70%',
            min_score >= 0.6 ~ '>=60%',
            min_score >= 0.5 ~ '>=50%',
            min_score >= 0.4 ~ '>=40%',
            min_score >= 0.3 ~ '>=30%',
            min_score >= 0.2 ~ '>=20%',
            min_score >= 0.1 ~ '>=10%',
            min_score >= 0.0 ~ '>=0%',
            TRUE ~''
        )) %>%
        mutate(range_max_score=case_when(
            max_score >= 0.9 ~ '>=90%',
            max_score >= 0.8 ~ '>=80%',
            max_score >= 0.7 ~ '>=70%',
            max_score >= 0.6 ~ '>=60%',
            max_score >= 0.5 ~ '>=50%',
            max_score >= 0.4 ~ '>=40%',
            max_score >= 0.3 ~ '>=30%',
            max_score >= 0.2 ~ '>=20%',
            max_score >= 0.1 ~ '>=10%',
            max_score >= 0.0 ~ '>=0%',
            TRUE ~''
        )) %>%
        select(pid, total_conn, total_candidates_to_sem_sim,
               range_min_score, range_max_score) %>%
        distinct()
    print(head(ds))
    return (ds)
}

paper_connections_min_max_freq <- function(ds) {
    ds <- ds %>%
        mutate(min_max=paste(range_min_score, range_max_score, sep=",")) %>%
        group_by(min_max) %>%
        summarise(n=n())
    print(head(ds))
    return (ds)
}

paper_connections <- function(ds) {

    ds <- ds %>%
        paper_connections_min_max() %>%
        paper_connections_min_and_max_ranges() %>%
        paper_connections_min_max_freq()
        
    return(ds)
}


readRenviron("envs/results_covid.env")

CONNECTIONS_FILE_PATH <- Sys.getenv("CONNECTIONS_FILE_PATH")
results_covid <- read_csv_file(CONNECTIONS_FILE_PATH)
g <- g_connections(results_covid)
g

readRenviron("envs/results_no_english.env")
CONNECTIONS_FILE_PATH <- Sys.getenv("CONNECTIONS_FILE_PATH")
results_no_english <- read_csv_file(CONNECTIONS_FILE_PATH)
result <- results_no_english
g <- g_connections(result)
g
pc <- paper_connections(result)
g1 <- draw_graphic_min_max_score(pc, "title", "bmin_max")


readRenviron("envs/results_pares.env")
CONNECTIONS_FILE_PATH <- Sys.getenv("CONNECTIONS_FILE_PATH")
results_pares <- read_csv_file(CONNECTIONS_FILE_PATH)
result <- results_pares
g <- g_connections(result)
g

pc <- paper_connections(result)
g1 <- draw_graphic_min_max_score(pc, "title", "cmin_max")


