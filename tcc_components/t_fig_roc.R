#########################################################
#
# Figure 8c
#
#
#########################################################

# install.packages("pROC")
# install.packages("ROCR")
# install.packages("ggplot2")
# install.packages("gridExtra")
# install.packages("tidyverse")
# install.packages("hrbrthemes")
# install.packages("cowplot")
# install.packages("viridis")

library(dplyr)
library(ggplot2)
library(gridExtra)
library(tidyverse)
library(hrbrthemes)

library(cowplot)
library(viridis)

library(ROCR)
library(pROC)
source( "./libs/my_functions_files.R")
readRenviron("envs/results_pares.env")

CONNECTIONS_FILE_PATH <- Sys.getenv("CONNECTIONS_FILE_PATH")

ds_connections <- read_csv_file(CONNECTIONS_FILE_PATH) %>%
    mutate(score=case_when(c_score_none == 0 ~ c_score, TRUE ~ 0))

ds_roc <- ds_connections %>%
    mutate(
        esperado=case_when(same==1 ~ 1, TRUE ~ 0),
        modelo=case_when(score >= 0.9 ~ 1, TRUE ~ 0))

ds_roc1 <- ds_connections %>%
    mutate(
        esperado=case_when(same==1 ~ 1, TRUE ~ 0),
        modelo=case_when(score >= 0.85 ~ 1, TRUE ~ 0))

ds_roc2 <- ds_connections %>%
    mutate(
        esperado=case_when(same==1 ~ 1, TRUE ~ 0),
        modelo=case_when(score >= 0.8 ~ 1, TRUE ~ 0))

ds_roc25 <- ds_connections %>%
    mutate(
        esperado=case_when(same==1 ~ 1, TRUE ~ 0),
        modelo=case_when(score >= 0.75 ~ 1, TRUE ~ 0))

ds_roc3 <- ds_connections %>%
    mutate(
        esperado=case_when(same==1 ~ 1, TRUE ~ 0),
        modelo=case_when(score >= 0.7 ~ 1, TRUE ~ 0))

ds_roc4 <- ds_connections %>%
    mutate(
        esperado=case_when(same==1 ~ 1, TRUE ~ 0),
        modelo=case_when(score >= 0.65 ~ 1, TRUE ~ 0))
ds_roc5 <- ds_connections %>%
    mutate(
        esperado=case_when(same==1 ~ 1, TRUE ~ 0),
        modelo=case_when(score >= 0.6 ~ 1, TRUE ~ 0))

# Fonte: statquest

par(pty="s")
roc1 <- roc(ds_roc$esperado, ds_roc$modelo, plot=TRUE, legacy.axes=TRUE, percent=TRUE, 
  xlab="Especificidade | Porcentagem de falsos positivos | Não relevantes",
  ylab="Sensibilidade \n Porcentagem de verdadeiros positivos \n Relevantes",
  col="#cc9652", lwd=3,
  print.auc=TRUE, print.auc.cex=0.8, print.auc.x = 21, print.auc.y=80
)

plot.roc(ds_roc$esperado, ds_roc1$modelo, percent=TRUE, col="#96528b", lwd=3, print.auc=TRUE, print.auc.cex=0.8,
    add=TRUE, print.auc.x = 21, print.auc.y=75
)

plot.roc(ds_roc$esperado, ds_roc2$modelo, percent=TRUE, col="#DD577A", lwd=3, print.auc=TRUE, print.auc.cex=0.8,
    add=TRUE, print.auc.x = 21, print.auc.y=70
)

plot.roc(ds_roc$esperado, ds_roc25$modelo, percent=TRUE, col="#1822BC", lwd=3, print.auc=TRUE, print.auc.cex=0.8,
    add=TRUE, print.auc.x = 21, print.auc.y=65
)

plot.roc(ds_roc$esperado, ds_roc3$modelo, percent=TRUE, col="#21918c", lwd=3, print.auc=TRUE, print.auc.cex=0.8,
    add=TRUE, print.auc.x = 21, print.auc.y=60
)

plot.roc(ds_roc$esperado, ds_roc4$modelo, percent=TRUE, col="#FE5A27", lwd=3, print.auc=TRUE, print.auc.cex=0.8,
    add=TRUE, print.auc.x = 21, print.auc.y=55
)
plot.roc(ds_roc$esperado, ds_roc5$modelo, percent=TRUE, col="#00AA4F", lwd=3, print.auc=TRUE, print.auc.cex=0.8,
    add=TRUE, print.auc.x = 21, print.auc.y=50
)


legend(
    "bottomright",
    legend=c(
        "coef similaridade >= 0.9",
        "coef similaridade >= 0.85",
        "coef similaridade >= 0.8",
        "coef similaridade >= 0.75",
        "coef similaridade >= 0.7",
        "coef similaridade >= 0.65",
        "coef similaridade >= 0.6"

        ),
    col=c("#cc9652", "#96528b", "#DD577A", "#1822BC", "#21918c", "#FE5A27", "#00AA4F"),
    lwd=3,
    cex=0.75
)

