
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
            without_doi=sources - with_doi,
            citation_unique=case_when(connections == 0 ~ 1, TRUE ~ 0),
            citation_multiple=case_when(connections > 0 ~ 1, TRUE ~ 0),
            ) %>% 
        rename(
            "total de referências"=refs,
            "total de referências bibliográficas com DOI"=refs_with_doi,
            "total de referências bibliográficas sem DOI"=refs_without_doi,
            "total de fontes registradas"=sources,
            "total de fontes registradas com DOI"=with_doi,
            "total de fontes registradas sem DOI"=without_doi,
            "tem citação única"=citation_unique,
            "tem citações múltiplas"=citation_multiple,
            "total de conexões pelas citações em comum"=connections,
            "total de conexões pela similaridade"=connections_with_score,
        )
    # s1["quantidade de conexões por similaridade"] <- NULL
    # s1["quantidade de conexões por refs em comum"] <- NULL
    s1["connections_without_score"] <- NULL
    s1["min_score"] <- NULL
    s1["max_score"] <- NULL

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
    jpeg(filename=output, width=1600, height=1200)
    plot(1:2)
    par(mar=c(0,0,0,0))
    corrplot(s1.cor, method="color", addCoef.col="white", 
        number.cex = 2, 
        tl.cex = 2.2,
        tl.srt = 45,
        type="lower", col=cols(20),
        # p.mat = p.mat, sig.level = 0.05, insig="blank",
        mar=c(0,4,0,0), oma=c(0,0,0,0)
    )
    dev.off()
#return (g)
  
}

add_qtd_sources_to_papers <- function(ds_papers, ds_sources) {
    ds_refs <- ds_sources %>%
        group_by(pid) %>%
        summarise(sources=n(), with_doi=sum(doi))
    print(head(ds_refs))
    ds <- left_join(ds_papers, ds_refs, by="pid")
    return (ds)
}

readRenviron("envs/refs_correlations.env")

S1_PAPERS_REFS <- Sys.getenv("S1_PAPERS_REFS")
S2_PAPERS_REFS <- Sys.getenv("S2_PAPERS_REFS")
S3_PAPERS_REFS <- Sys.getenv("S3_PAPERS_REFS")

S1_SOURCES_REFS <- Sys.getenv("S1_SOURCES_REFS")
S2_SOURCES_REFS <- Sys.getenv("S2_SOURCES_REFS")
S3_SOURCES_REFS <- Sys.getenv("S3_SOURCES_REFS")

G_S1_PAPERS_REFS <- Sys.getenv("G_S1_PAPERS_REFS")
G_S2_PAPERS_REFS <- Sys.getenv("G_S2_PAPERS_REFS")
G_S3_PAPERS_REFS <- Sys.getenv("G_S3_PAPERS_REFS")

GS_PAPERS_REFS <- Sys.getenv("GS_PAPERS_REFS")


s1 <- read_csv_file(S1_PAPERS_REFS)
# s2 <- read_csv_file(S2_PAPERS_REFS)
# s3 <- read_csv_file(S3_PAPERS_REFS)

ds_sources_refs_1 <- read_csv_file(S1_SOURCES_REFS)
# ds_sources_refs_2 <- read_csv_file(S2_SOURCES_REFS)
# ds_sources_refs_3 <- read_csv_file(S3_SOURCES_REFS)

ds1 <- add_qtd_sources_to_papers(s1, ds_sources_refs_1)
# ds2 <- add_qtd_sources_to_papers(s2, ds_sources_refs_2)
# ds3 <- add_qtd_sources_to_papers(s3, ds_sources_refs_3)

ga <- gcorr(ds1, G_S1_PAPERS_REFS, "A")
# gb <- gcorr(ds2, G_S2_PAPERS_REFS, "B")
# gc <- gcorr(ds3, G_S3_PAPERS_REFS, "C")

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


