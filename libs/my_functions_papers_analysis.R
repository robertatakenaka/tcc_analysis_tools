
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

source("./libs/my_functions_files.R")
source("./libs/my_functions_translated_codes.R")
source("./libs/my_functions_graphics.R")

options(digits = 2)
options(scipen = 999)


display_numbers <- function(n, total, x="") {
    if (x == TRUE) {
        print("!!!!!!")
        print(sum(n))
    }
    sprintf("%3.0f (%0.2f%%)", n, 100*n/total)
}


# papers.csv - original columns
# pid,refs,refs_with_doi,connections,score_gt_89,score_gt_79,score_gt_69,
# score_gt_59,score_gt_0_59,score_none,no_en,en_only,with_en

xray <- function(ds_papers, ds_sources) {
    
    

    total_papers <- nrow(ds_papers)
    total_refs <- sum(ds_papers$refs)
    total_sources <- nrow(ds_sources)
    total_connections <- sum(ds_papers$connections)

    df_connections <- connections_papers(ds_papers)

    labels <- c()
    values <- c()

    labels <- c(labels, 'Total de artigos registrados')
    values <- c(values, total_papers)

    labels <- c(labels, 'Artigos sem resumo em inglês')
    values <- c(values, display_numbers(sum(ds_papers$no_en), total_papers))

    labels <- c(labels, 'Artigos com resumo somente em inglês')
    values <- c(values, display_numbers(sum(ds_papers$en_only), total_papers))

    labels <- c(labels, 'Artigos com resumos em inglês + outros idiomas')
    values <- c(values, display_numbers(sum(ds_papers$with_en), total_papers))

    labels <- c(labels, 'Total de referências bibliográficas')
    values <- c(values, total_refs)

    labels <- c(labels, 'Total de fontes criadas a partir de referências bibliográficas')
    values <- c(values, total_sources)

    labels <- c(labels, 'Total de conexões dos artigos (feitas pelas referências bibliográficas)')
    values <- c(values, total_connections)

    df_papers <- data.frame(labels=labels, values=values)
    df <- rbind(df_papers, df_connections)

    df_scored_connections <- max_score_ranges_papers(ds_papers) %>%
        rename("labels"="max_score_ranges", "values"="total")
    df <- rbind(df, df_scored_connections)
    
    
    labels <- c()
    values <- c()
    
    labels <- c(labels, 'Total de referências bibliográficas')
    values <- c(values, total_refs)

    labels <- c(labels, 'Referências bibliográficas com DOI')
    values <- c(values, display_numbers(sum(ds_papers$refs_with_doi), total_refs))


    labels <- c(labels, 'Total de fontes criadas a partir de referências bibliográficas')
    values <- c(values, total_sources)

    df_refs <- data.frame(labels=labels, values=values)
    df <- rbind(df, df_refs)

    labels <- c(labels, 'Fontes com DOI')
    values <- c(values, display_numbers(sum(ds_sources$doi), total_sources, TRUE))

    ds_sources_cited <- t_sources(ds_sources)
    df <- rbind(df, ds_sources_cited)

    ds_doi_vs_reflinks <- t_sources__doi_vs_reflinks(ds_sources)
    df <- rbind(df, ds_doi_vs_reflinks)

    labels <- c()
    values <- c()

    labels <- c(labels, 'Total de conexões dos artigos (feitas pelas referências bibliográficas)')
    values <- c(values, total_connections)

    labels <- c(labels, 'Conexões pelas referências bibliográficas, mas cortados na seleção de candidatos')
    values <- c(values, display_numbers(sum(ds_papers$score_none), total_connections))

    total_candidatas <- total_connections - sum(ds_papers$score_none)
    labels <- c(labels, 'Conexões candidatas à similaridade')
    values <- c(values, display_numbers(total_candidatas, total_connections))

    ge_70 <- sum(ds_papers$score_gt_69) + sum(ds_papers$score_gt_79) + sum(ds_papers$score_gt_89)
    labels <- c(labels, 'Conexões por similaridade semântica >= 70%')
    values <- c(values, display_numbers(ge_70, total_candidatas))
    
    labels <- c(labels, 'Conexões por similaridade semântica >= 90%')
    values <- c(values, display_numbers(sum(ds_papers$score_gt_89), total_candidatas))

    labels <- c(labels, 'Conexões por similaridade semântica 80-89%')
    values <- c(values, display_numbers(sum(ds_papers$score_gt_79), total_candidatas))

    labels <- c(labels, 'Conexões por similaridade semântica 70-79%')
    values <- c(values, display_numbers(sum(ds_papers$score_gt_69), total_candidatas))

    labels <- c(labels, 'Conexões por similaridade semântica 60-69%')
    values <- c(values, display_numbers(sum(ds_papers$score_gt_59), total_candidatas))

    labels <- c(labels, 'Conexões por similaridade semântica 1-59%')
    values <- c(values, display_numbers(sum(ds_papers$score_gt_1_59), total_candidatas))

    labels <- c(labels, 'Conexões por similaridade semântica < 1%')
    values <- c(values, display_numbers(sum(ds_papers$score_lt_1), total_candidatas))

    df_conns <- data.frame(labels=labels, values=values)
    df <- rbind(df, df_conns)

    return (df)
}


connections_papers <- function(ds_papers) {
    report <- ds_papers %>%
        mutate(labels=case_when(
            connections > 0 ~ 'Artigos vinculados pelas referências bibliográficas',
            connections == 0 ~ 'Artigos sem recomendações'
        )) %>%
        group_by(labels) %>%
        summarise(n=n()) %>%
        mutate(values=display_numbers(n, nrow(ds_papers))) %>%
        select(labels, values) %>%
        distinct()
        
    print(head(report))
    return (report)
}


max_score_ranges_papers <- function(ds_papers) {
    ds <- ds_papers %>%
        mutate(linked_by_ref=case_when(
            connections > 0 ~ 1,
            connections == 0 ~ 0
        ))
    candidatos <- sum(ds$linked_by_ref)
    report <- ds %>%
        mutate(max_score_ranges=case_when(
            score_gt_89 > 0 ~ 'Artigos cuja pontuação mais alta está entre 90-100',
            score_gt_79 > 0 ~ 'Artigos cuja pontuação mais alta está entre 80-89',
            score_gt_69 > 0 ~ 'Artigos cuja pontuação mais alta está entre 70-79',
            score_gt_59 > 0 ~ 'Artigos cuja pontuação mais alta está entre 60-69',
            score_gt_1_59 > 0 ~ 'Artigos cuja pontuação mais alta está entre 1-59',
            score_lt_1 > 0 ~ 'Artigos cuja pontuação mais alta está menor que 1',
            score_none > 0 ~ 'Artigos não candidatos à similaridade semântica',
        )) %>%
        group_by(max_score_ranges) %>%
        summarise(tt=n()) %>%
        mutate(total=display_numbers(tt, candidatos)) %>%
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
    print(head(ds_sources))
    d <- ds_sources %>%
        mutate(labels=case_when(
            reflinks > 1 ~ "Fontes citadas por mais de 1 artigo",
            TRUE ~ "Fontes citadas por um artigo")) %>%
        group_by(labels) %>%
        summarise(n=n()) %>%
        mutate(values=display_numbers(n, nrow(ds_sources))) %>%
        select(labels, values) %>%
        distinct()
    return (d)
}

t_sources__doi_vs_reflinks <- function(ds_sources) {
    print(".....")
    print(head(ds_sources))
    print(nrow(ds_sources))
    print(sum(ds_sources$doi))
    d <- ds_sources %>%
        mutate(cited_once=case_when(
            reflinks == 1 ~ 1,
            reflinks > 1 ~ 0
        )) %>%
        group_by(doi, cited_once) %>%
        mutate(n=n()) %>%
        mutate(values=display_numbers(n, nrow(ds_sources))) %>%
        mutate(labels=case_when(
            doi == 1 & cited_once == 1 ~ 'Fontes com DOI citadas uma vez',
            doi == 1 & cited_once == 0 ~ 'Fontes com DOI citadas > 1 vez',
            doi == 0 & cited_once == 1 ~ 'Fontes sem DOI citadas uma vez',
            doi == 0 & cited_once == 0 ~ 'Fontes sem DOI citadas > 1 vez'
        )) %>%
        dplyr::select(labels, values) %>%
        distinct()
    d <- data.frame(labels=d$labels, values=d$values)
    print(d)
    return (d)
}
