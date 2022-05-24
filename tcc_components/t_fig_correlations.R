
# install.packages("corrplot")

library(ggplot2)
library(gridExtra)
library(tidyverse)
library(hrbrthemes)

library(cowplot)
library(viridis)
library("tidyverse") #pacote para manipulacao de dados
library("dplyr")


source("./libs/my_functions_graphics.R")
source("./libs/my_functions_files.R")

# install.packages("Hmisc")
# install.packages("corrplot")
library("Hmisc")
library("corrplot")


gcorr <- function(ds, output, title) {
    s1 <- ds

    s1["pid"] <- NULL

    s1[is.na(s1)] <- 0

    s1 <- s1 %>%
        mutate(
            citation_unique=case_when(connections == 0 ~ 1, TRUE ~ 0),
            citation_multiple=case_when(connections > 0 ~ 1, TRUE ~ 0),
            null_similarity=case_when(min_score == 0 ~ 1, TRUE ~ 0),
            min_49=case_when(min_score < 0.5 & min_score > 0 ~ 1, TRUE ~ 0),
            min_50=case_when(min_score >= 0.5 ~ 1, TRUE ~ 0),
            min_75=case_when(min_score >= 0.75 ~ 1, TRUE ~ 0),
            ) %>% 
        rename(
            "total de referências"=refs,
            "quantidade de refs com DOI"=refs_with_doi,
            "quantidade de refs sem DOI"=refs_without_doi,
            "citações únicas"=citation_unique,
            "citações múltiplas"=citation_multiple,
            "quantidade de conexões por refs em comum"=connections,
            "quantidade de conexões por similaridade"=connections_with_score,
            "artigos sem conexões"=null_similarity,
            "artigos com coeficiente mínimo < 0,5"=min_49,
            "artigos com coeficiente mínimo >= 0,5"=min_50,
            "artigos com coeficiente mínimo >= 0,75"=min_75,
        )
    # s1["quantidade de conexões por similaridade"] <- NULL
    # s1["quantidade de conexões por refs em comum"] <- NULL
    s1["min_score"] <- NULL
    s1["max_score"] <- NULL
    s1["connections"] <- NULL
    s1["max_score"] <- NULL
    s1["artigos com coeficiente mínimo < 0,5"] <- NULL
    s1["artigos com coeficiente mínimo >= 0,5"] <- NULL
    s1["artigos com coeficiente mínimo >= 0,75"] <- NULL

    s1.cor <- cor(s1)
    s1.rcorr <- rcorr(as.matrix(s1))
    s1.coeff <- s1.rcorr$r
    s1.p <- s1.rcorr$P

    #p.mat <- cor.mtest(s1.cor)

    cols <- colorRampPalette(
            c(  
                # preto
                "#000004",
                # petroleo
                "#2c728e", 
                # verde
                "#28ae80", 
                # laranja
                "#f98e09",
                # vermelho 
                "#bc3754",
                # vinho 
                "#8a226a"
                )
            )
    jpeg(filename=output, width=600, height=600)
    plot(1:1)
    par(mar=c(0,0,0,0))
    corrplot(s1.cor, method="color", addCoef.col="white", 
        number.cex = 1, 
        tl.cex = 1.2,
        type="upper", col=cols(20),
        # p.mat = p.mat, sig.level = 0.05, insig="blank",
        title=title, mar=c(0,0,2,0), oma=c(0,0,0,0)
    )
    dev.off()
#return (g)
  
}


g_source_corr <- function(ds, output) {
    df <- ds %>%
        mutate(connections=case_when(reflinks == 1 ~ 0, TRUE ~ 1)) %>%
        select(doi, connections)

        df.cor <- cor(df, method="pearson")
        df.rcorr <- rcorr(as.matrix(df))
        df.coeff <- df.rcorr$r
        df.p <- df.rcorr$P

        cols <- colorRampPalette(c("#440f76","#721f81",  "#9e2f7f", "#cd4071",  "#f1605d",  "#35b779", "#31688e"))
        corrplot(df.cor, method="color", addCoef.col="white", type="lower", diag=FALSE,
                 col=cols(100), sig.level = 0.05, insig="blank",
                 mar=c(0,0,1,0)
                 ) %>%
        save_g(output)  
}


readRenviron("envs/refs_correlations.env")
G2_MIN_MAX_FILE_PATH <- Sys.getenv("G2_MIN_MAX_FILE_PATH")

S1_PAPERS_REFS <- Sys.getenv("S1_PAPERS_REFS")
S2_PAPERS_REFS <- Sys.getenv("S2_PAPERS_REFS")
S3_PAPERS_REFS <- Sys.getenv("S3_PAPERS_REFS")

G_S1_PAPERS_REFS <- Sys.getenv("G_S1_PAPERS_REFS")
G_S2_PAPERS_REFS <- Sys.getenv("G_S2_PAPERS_REFS")
G_S3_PAPERS_REFS <- Sys.getenv("G_S3_PAPERS_REFS")

GS_PAPERS_REFS <- Sys.getenv("GS_PAPERS_REFS")


s1 <- read_csv_file(S1_PAPERS_REFS)
s2 <- read_csv_file(S2_PAPERS_REFS)
s3 <- read_csv_file(S3_PAPERS_REFS)

ga <- gcorr(s1, G_S1_PAPERS_REFS, "A")
gb <- gcorr(s2, G_S2_PAPERS_REFS, "B")
gc <- gcorr(s3, G_S3_PAPERS_REFS, "C")

# s <- rbind(s1, s2, s3) %>% gcorr(GS_PAPERS_REFS, "")
# g <- grid.arrange(ga, gb, nrow = 1) %>%
#     graphics(GS_PAPERS_REFS, width=80, height=40)


# S1_SOURCES_REFS <- Sys.getenv("S1_SOURCES_REFS")
# S2_SOURCES_REFS <- Sys.getenv("S2_SOURCES_REFS")
# S3_SOURCES_REFS <- Sys.getenv("S3_SOURCES_REFS")

# G_S1_SOURCES_REFS <- Sys.getenv("G_S1_SOURCES_REFS")
# G_S2_SOURCES_REFS <- Sys.getenv("G_S2_SOURCES_REFS")
# G_S3_SOURCES_REFS <- Sys.getenv("G_S3_SOURCES_REFS")

# s1 <- read_csv_file(S1_SOURCES_REFS)
# s2 <- read_csv_file(S2_SOURCES_REFS)
# s3 <- read_csv_file(S3_SOURCES_REFS)

# g <- g_source_corr(s1, G_S1_SOURCES_REFS)
# g <- g_source_corr(s2, G_S2_SOURCES_REFS)
# g <- g_source_corr(s3, G_S3_SOURCES_REFS)


