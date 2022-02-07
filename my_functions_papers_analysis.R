
# Instalando e carregando pacotes -----------------------------------------

pacotes <- c("tidyverse","knitr","kableExtra","car","rgl","gridExtra",
             "PerformanceAnalytics","reshape2","rayshader","psych","pracma",
             "polynom","rqPen","ggrepel")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

########################################
#
#   CHAMANDO BIBLIOTECAS IMPORTANTES
#
########################################

library(cluster) #algoritmo de cluster
library(clustree)
library(clValid)
library(corrplot)
library(cowplot)
library(dendextend) #compara dendogramas
library(factoextra) #algoritmo de cluster e visualizacao
library(FactoMineR)
library(fpc) #algoritmo de cluster e visualizacao
library(GGally)
library(ggfortify)
library(ggiraphExtra)
library(gridExtra) #para a funcao grid arrange
library(hrbrthemes)
library(kableExtra)
library(knitr)
library(magrittr)
library(NbClust)
library(readxl)
library(tidyverse) #pacote para manipulacao de dados
library(viridis)
library(ggplot2)
library(dplyr)

source("my_functions_files.R")
source("my_functions_translated_codes.R")
source("my_functions_graphics.R")

options(digits = 2)
options(scipen = 999)


# graphic_reflinks_freq <- function(ds_graphic) {

#     df <- data.frame(
#         x=as.factor(ds_graphic$reflinks),
#         val=10+ds_graphic$perc,
#         display=ds_graphic$display
#       ) %>% arrange(val)

#     # With a bit more style
#     g <- df %>%
#         ggplot() +
#         geom_segment( aes(x=x, xend=x, y=0, yend=val), color="black") +
#         geom_point( aes(x=x, y=val, color=x), size=3 ) +
#         geom_text(
#             aes(x=x, y=val, label=display),
#             hjust = -0.2, size = 2.5,
#             position=position_dodge(width = 1)
#         ) +
#         coord_flip()+
#         expand_limits(y = c(-0.5, df$val+10)) +
#         theme_bw() +
#         theme(
#             legend.position = "none",
#             plot.margin=margin(2,2,2,2),
#             panel.border = element_blank(),
#             panel.spacing = unit(0.1, "lines"),
#             strip.text.x = element_text(size = 9)
#         ) +
#         xlab("quantidade artigos que citaram uma fonte") +
#         ylab("porcentagem de fontes citadas") 
        
#     return (g)

# }

# group_by(en_presence, aproveitamento) %>%
# summarise(n=n()) %>%
# mutate(total=sum(n)) %>%
# mutate(perc=n*100/total) %>%
# mutate(display=sprintf("%1.0f (%0.2f%%)", n, perc)) %>%
# mutate(display2=sprintf("%1.0f / %1.0f (%0.2f%%)", higher_score, connections, aproveitamento)) %>%
# mutate(grp=en_presence) %>%
# write_csv_file(PAPER_ANALYSIS_SCORES_FILE_PATH)



graphic_papers <- function(ds_graphic, aspect_ratio=1, ncol=1) {

    df <- data.frame(
        grp=ds_graphic$en_presence,
        x=ds_graphic$aproveitamento,
        val=10+ds_graphic$perc,
        display=ds_graphic$display
      ) %>% arrange(val)

    # With a bit more style
    g <- df %>%
        ggplot() +
        geom_segment( aes(x=x, xend=x, y=0, yend=val), color="black") +
        geom_point( aes(x=x, y=val, color=x), size=3 ) +
        geom_text(
            aes(x=x, y=val, label=display),
            hjust = -0.2, size = 2.5,
            position=position_dodge(width = 1)
        ) +
        coord_flip()+
        expand_limits(y = c(-0.5, df$val+30)) +
        theme_bw() +
        theme(
            legend.position = "none",
            plot.margin=margin(2,2,2,2),
            panel.border = element_blank(),
            panel.spacing = unit(0.1, "lines"),
            aspect.ratio = aspect_ratio,
            strip.text.x = element_text(size = 9)
        ) +
        xlab("aproveitamento") +
        ylab("porcentagem de artigos com pontuações correspondentes") +
        facet_wrap(~grp, ncol=ncol, scale="free")
        
    return (g)

}




graphic_papers_mode2 <- function(ds_graphic, aspect_ratio=1, ncol=1) {

    df <- data.frame(
        x=ds_graphic$en_presence,
        y=ds_graphic$aproveitamento,
        display=ds_graphic$display
      ) 

    # With a bit more style
    g <- df %>%
        ggplot( aes(x=x, y=y, fill=x)) +
        geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) + 
        geom_boxplot(width=0.1, color="gray") + 
        scale_fill_viridis(discrete = TRUE) +
        theme_ipsum() +
        theme(
          legend.position="none",
            aspect.ratio = aspect_ratio,
          plot.title = element_text(size=11)
        ) +
        coord_flip() +
         #        ggtitle("A Violin wrapping a boxplot") +
        xlab("") + 
        ylab("Porcentagem de recomendações com alta pontuação / candidatos")
        
    return (g)

}


get_ds_graphic <- function() {
    ds_papers <- read_csv_file(PAPERS_CSV_FILE_PATH) %>%
        mutate(higher_score=score_gt_69+score_gt_79+score_gt_89) %>%
        mutate(aproveitamento=case_when(
            connections > 0 ~ 100.0 * higher_score / connections,
            TRUE ~ 0
        )) %>%
        mutate(en_presence=case_when(
            no_en > 0 ~ 'nenhum resumo em inglês',
            en_only > 0 ~ 'somente resumo em inglês',
            with_en > 0 ~ 'resumos multilíngues com inglês'
        ))

    print('Quantidade artigos registrados')
    print(nrow(ds_papers))

    print('Quantidade total de refs')
    print(sum(ds_papers$refs))

    print('Quantidade total de no_en')
    print(sum(ds_papers$no_en))

    print('Quantidade total de en_only')
    print(sum(ds_papers$en_only))

    print('Quantidade total de with_en')
    print(sum(ds_papers$with_en))

    # correlation_graphic(ds_papers) %>%
    #     grid.arrange()

    ds_g <- ds_papers %>%
        group_by(en_presence, aproveitamento, higher_score, connections) %>%
        summarise(n=n()) %>%
        mutate(total=sum(n)) %>%
        ungroup() %>%
        mutate(perc=n*100/total) %>%
        mutate(display=sprintf("%1.0f (%0.2f%%)", n, perc)) %>%
        # mutate(display2=sprintf("%1.0f / %1.0f (%0.2f%%)", higher_score, connections, aproveitamento)) %>%
        mutate(grp=en_presence) %>%
        write_csv_file(PAPER_ANALYSIS_SCORES_FILE_PATH)
    return (ds_g)
}


# read_env_vars <- function(env_file_path) {
#     readRenviron(env_file_path)

#     PAPERS_CSV_FILE_PATH <- Sys.getenv("PAPERS_CSV_FILE_PATH")
#     SOURCE_CSV_FILE_PATH <- Sys.getenv("SOURCE_CSV_FILE_PATH")
#     PAPER_ANALYSIS_SCORES_FILE_PATH <- Sys.getenv("PAPER_ANALYSIS_SCORES_FILE_PATH")
#     GRAPHIC_PAPERS_FILE_PATH <- Sys.getenv("GRAPHIC_PAPERS_FILE_PATH")
# }