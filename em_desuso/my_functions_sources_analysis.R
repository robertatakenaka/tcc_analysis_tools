
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


graphic_reflinks_freq <- function(ds_graphic) {

    df <- data.frame(
        x=as.factor(ds_graphic$reflinks),
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
        expand_limits(y = c(-0.5, df$val+10)) +
        theme_bw() +
        theme(
            legend.position = "none",
            plot.margin=margin(2,2,2,2),
            panel.border = element_blank(),
            panel.spacing = unit(0.1, "lines"),
            strip.text.x = element_text(size = 9)
        ) +
        xlab("quantidade artigos que citaram uma fonte") +
        ylab("porcentagem de fontes citadas") 
        
    return (g)

}

graphic_reflinks_freq_2 <- function(ds_graphic) {

    df <- data.frame(
        grp=ds_graphic$grp,
        x=as.factor(ds_graphic$reflinks),
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
            aspect.ratio = 2/5,
            strip.text.x = element_text(size = 9)
        ) +
        xlab("quantidade artigos que citaram uma fonte") +
        ylab("porcentagem de fontes citadas") +
        facet_wrap(~grp, ncol=2, scale="free")
        
    return (g)

}

print_source_data <- function(ds_sources) {
    print('Quantidade total de refs registradas em source')
    print(sum(ds_sources$reflinks))

    print('Quantidade fontes registradas')
    print(nrow(ds_sources))

    print('Quantidade de fontes registradas usando DOI')
    print(sum(ds_sources$doi))
}


correlation_graphic <- function(ds_sources) {
    # Salvando a Matriz de Correlações -----------------------------------
    ds_sources_selected_cols <- ds_sources %>%
        select(doi, reflinks) %>%
        scale()

    rho <- cor(ds_sources_selected_cols)

    # Observando as correlações entre variáveis
    g1 <- chart.Correlation(ds_sources_selected_cols)

    # Construindo um mapa de calor a partir das correlações
    g2 <- rho %>% 
        melt() %>% 
        ggplot() +
        geom_tile(aes(x = Var1, y = Var2, fill = value)) +
        geom_text(aes(x = Var1, y = Var2, label = round(x = value, digits = 2)),
                size = 4) +
        labs(x = NULL,
           y = NULL,
           fill = "Correlações") +
        scale_fill_gradient2(low = "dodgerblue4", 
                           mid = "white", 
                           high = "brown4",
                           midpoint = 0) +
        theme(panel.background = element_rect("white"),
            panel.grid = element_line("grey95"),
            panel.border = element_rect(NA),
            legend.position = "bottom",
            axis.text.x = element_text(angle = 0))
    return (g2)
}


main <- function(env_file_path) {
    readRenviron(env_file_path)

    SOURCE_CSV_FILE_PATH <- Sys.getenv("SOURCE_CSV_FILE_PATH")
    SOURCES_ANALYSIS_DOI_VS_REFLINKS_CSV_FILE_PATH <- Sys.getenv("SOURCES_ANALYSIS_DOI_VS_REFLINKS_CSV_FILE_PATH")
    GRAPHIC_WITHOUT_DOI_FILE_PATH <- Sys.getenv("GRAPHIC_WITHOUT_DOI_FILE_PATH")
    GRAPHIC_WITH_DOI_FILE_PATH <- Sys.getenv("GRAPHIC_WITH_DOI_FILE_PATH")
    GRAPHIC__FILE_PATH <- Sys.getenv("GRAPHIC__FILE_PATH")


    # _id,doi,reflinks,ref_type,pub_year,reflinks_year_min,reflinks_year_max
    ds_sources <- read_csv_file(SOURCE_CSV_FILE_PATH)


    print_source_data(ds_sources)

    # correlation_graphic(ds_sources) %>%
    #     grid.arrange()

    ds_g <- ds_sources %>%
        group_by(doi, reflinks) %>%
        summarise(n=n()) %>%
        mutate(total=sum(n)) %>%
        mutate(perc=n*100/total) %>%
        mutate(display=sprintf("%1.0f (%0.2f%%)", n, perc)) %>%
        mutate(grp=case_when(doi == 1 ~ 'DOI presente', TRUE ~ 'DOI ausente')) %>%
        write_csv_file(SOURCES_ANALYSIS_DOI_VS_REFLINKS_CSV_FILE_PATH)


    ds_graphic_with_doi <- ds_g %>%
        filter(doi==1)
    g_with_doi <- graphic_reflinks_freq(ds_graphic_with_doi) %>%
        save_g(GRAPHIC_WITH_DOI_FILE_PATH) 

    ds_graphic_without_doi <- ds_g %>%
        filter(doi==0)
    g_without_doi <- graphic_reflinks_freq(ds_graphic_without_doi) %>%
        save_g(GRAPHIC_WITHOUT_DOI_FILE_PATH)

    g_x <- graphic_reflinks_freq_2(ds_g) %>%
        save_g(GRAPHIC__FILE_PATH) 
}
