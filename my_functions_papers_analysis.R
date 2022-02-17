
# Instalando e carregando pacotes -----------------------------------------

# pacotes <- c("tidyverse","knitr","kableExtra","car","rgl","gridExtra",
#              "PerformanceAnalytics","reshape2","rayshader","psych","pracma",
#              "polynom","rqPen","ggrepel")

# if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
#   instalador <- pacotes[!pacotes %in% installed.packages()]
#   for(i in 1:length(instalador)) {
#     install.packages(instalador, dependencies = T)
#     break()}
#   sapply(pacotes, require, character = T) 
# } else {
#   sapply(pacotes, require, character = T) 
# }

########################################
#
#   CHAMANDO BIBLIOTECAS IMPORTANTES
#
########################################

library("tidyverse") #pacote para manipulacao de dados
library("dplyr")

source("my_functions_files.R")
source("my_functions_translated_codes.R")
source("my_functions_graphics.R")

options(digits = 2)
options(scipen = 999)


# papers.csv - original columns
# pid,refs,refs_with_doi,connections,score_gt_89,score_gt_79,score_gt_69,
# score_gt_59,score_gt_0_59,score_none,no_en,en_only,with_en

xray <- function(ds_papers) {
    
    labels <- c()
    values <- c()

    labels <- c(labels, 'Total de artigos registrados')
    values <- c(values, nrow(ds_papers))

    labels <- c(labels, 'Total de artigos sem resumo em inglês')
    values <- c(values, sum(ds_papers$no_en))

    labels <- c(labels, 'Total de artigos com resumo somente em inglês')
    values <- c(values, sum(ds_papers$en_only))

    labels <- c(labels, 'Total de artigos com resumos em inglês + outros idiomas')
    values <- c(values, sum(ds_papers$with_en))

    labels <- c(labels, 'Total de referências')
    values <- c(values, sum(ds_papers$refs))

    labels <- c(labels, 'Total de referências com DOI')
    values <- c(values, sum(ds_papers$refs_with_doi))

    labels <- c(labels, 'Total de conexões dos artigos pelas referências')
    values <- c(values, sum(ds_papers$connections))

    labels <- c(labels, 'Total de conexões por similaridade semântica >= 70%')
    values <- c(values,
        sum(ds_papers$score_gt_69) + sum(ds_papers$score_gt_79) + sum(ds_papers$score_gt_89))
    
    labels <- c(labels, 'Total de conexões por similaridade semântica >= 90%')
    values <- c(values, sum(ds_papers$score_gt_89))

    labels <- c(labels, 'Total de conexões por similaridade semântica 80-89%')
    values <- c(values, sum(ds_papers$score_gt_79))

    labels <- c(labels, 'Total de conexões por similaridade semântica 70-79%')
    values <- c(values, sum(ds_papers$score_gt_69))

    labels <- c(labels, 'Total de conexões por similaridade semântica 60-69%')
    values <- c(values, sum(ds_papers$score_gt_59))

    labels <- c(labels, 'Total de conexões por similaridade semântica 1-60%')
    values <- c(values, sum(ds_papers$score_gt_1_59))

    labels <- c(labels, 'Total de conexões por similaridade semântica < 1%')
    values <- c(values, sum(ds_papers$score_lt_1))

    df <- data.frame(labels=labels, values=values)

    df1 <- connections_papers(ds_papers) %>% 
        rename(labels=n, values=total)
    df <- rbind(df, df1)

    df1 <- max_score_ranges_papers(ds_papers) %>% 
        rename(labels="max_score_ranges", values=total)  %>% 
        select(labels, values)  %>%
        unique()
    df <- rbind(df, df1)

    df1 <- highest_scores_papers(ds_papers) %>% 
        rename(labels="papers_with_highest_scores", values="total")  %>%
        unique()
    df <- rbind(df, df1)

    print(df)
    return (df)
}


connections_papers <- function(ds_papers) {
    report <- ds_papers %>%
        mutate(n=case_when(
            connections > 0 ~ 'Total de artigos conectados',
            TRUE ~ 'Total de artigos sem nenhuma conexão'
        )) %>%
        group_by(n) %>%
        summarise(total=n()) %>%
        select(n, total)
    print(head(report))
    return (report)
}


max_score_ranges_papers <- function(ds_papers) {
    report <- ds_papers %>%
        mutate(max_score_ranges=case_when(
            score_gt_89 > 0 ~ 'Total de artigos cuja pontuação mais alta é entre 90-100',
            score_gt_79 > 0 ~ 'Total de artigos cuja pontuação mais alta é entre 80-89',
            score_gt_69 > 0 ~ 'Total de artigos cuja pontuação mais alta é entre 70-79',
            score_gt_59 > 0 ~ 'Total de artigos cuja pontuação mais alta é entre 60-69',
            score_gt_1_59 > 0 ~ 'Total de artigos cuja pontuação mais alta é entre 0-59',
            score_lt_1 > 0 ~ 'Total de artigos cuja pontuação mais alta é entre 0-59',
            TRUE ~ 'Total de artigos sem pontuação',
        )) %>%
        group_by(max_score_ranges) %>%
        summarise(total=n()) %>%
        select(max_score_ranges, total)
    print(head(report))
    return (report)
}


highest_scores_papers <- function(ds_papers) {
    report <- ds_papers %>%
        mutate(papers_with_highest_scores=case_when(
            score_gt_89 + score_gt_79 + score_gt_69 > 0 ~ 'Total de artigos cuja pontuação mais alta é entre 70-100',
            TRUE ~ 'Total de artigos cuja pontuação mais alta é < 70%',
        )) %>%
        group_by(papers_with_highest_scores) %>%
        mutate(total=n()) %>%
        ungroup() %>%
        distinct() %>%
        select(papers_with_highest_scores, total)
    print(head(report))
    return (report)
}


english_presence_papers <- function(ds_papers) {
    report <- ds_papers %>%
        mutate(en_presence=case_when(
            no_en > 0 ~ 'Total de artigos sem resumo em inglês',
            en_only > 0 ~ 'Total de artigos com resumo somente em inglês',
            with_en > 0 ~ 'Total de artigos com resumos em inglês + outros idiomas'
        )) %>%

        group_by(en_presence) %>%
        summarise(total=n()) %>%
        select(en_presence, total)
    print(head(report))
    return (report)
}


t_sources <- function(ds_sources) {
    d <- ds_sources %>%
        mutate(linked=case_when(
            reflinks > 1 ~ "Fonte conecta artigos científicos",
            TRUE ~ "Fonte encontrada em apenas uma referência")) %>%
        group_by(linked) %>%
        summarise(n=n()) %>%
        mutate(total=sum(n)) %>%
        mutate(perc=100*n/total) %>%
        select(linked, perc, n, total) %>%
        distinct()
    return (d)
}
