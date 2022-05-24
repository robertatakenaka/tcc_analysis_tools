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
        modelo=case_when(score >= 0.8 ~ 1, TRUE ~ 0))

ds_roc2 <- ds_connections %>%
    mutate(
        esperado=case_when(same==1 ~ 1, TRUE ~ 0),
        modelo=case_when(score >= 0.7 ~ 1, TRUE ~ 0))

ds_roc3 <- ds_connections %>%
    mutate(
        esperado=case_when(same==1 ~ 1, TRUE ~ 0),
        modelo=case_when(score >= 0.85 ~ 1, TRUE ~ 0))

# Fonte: statquest

par(pty="s")
roc1 <- roc(ds_roc$esperado, ds_roc$modelo, plot=TRUE, legacy.axes=TRUE, percent=TRUE, 
  xlab="Especificidade | Porcentagem de falsos positivos | Não relevantes",
  ylab="Sensibilidade \n Porcentagem de verdadeiros positivos \n Relevantes",
  col="#440154", lwd=4,
  print.auc=TRUE,
  xlab.y = -20
)

#plot.roc(ds_roc$modelo, ds_roc$esperado, percent=TRUE, col="#440154", lwd=4, print.auc=TRUE)

roc2 <- roc(ds_roc2$esperado, ds_roc2$modelo, plot=TRUE, legacy.axes=TRUE, percent=TRUE, 
  xlab="Especificidade | Porcentagem de falsos positivos | Não relevantes",
  ylab="Sensibilidade \n Porcentagem de verdadeiros positivos \n Relevantes",
  col="#3b528b", lwd=4,
  print.auc=TRUE
)

# legend(
#     "bottomright",
#     legend=c(
#         "Relevantes = coef similiradade >= 0.7"
#         ),
#     col=c("#3b528b"),
#     lwd=4
# )

plot.roc(ds_roc$esperado, ds_roc2$modelo[,2], percent=TRUE, col="#3b528b", lwd=4, print.auc=TRUE,
    add=TRUE, print.auc.y=40
)

roc3 <- roc(ds_roc3$esperado, ds_roc3$modelo, plot=TRUE, legacy.axes=TRUE, percent=TRUE, 
  xlab="Especificidade | Porcentagem de falsos positivos | Não relevantes",
  ylab="Sensibilidade \n Porcentagem de verdadeiros positivos \n Relevantes",
  col="#21918c", lwd=4,
  print.auc=TRUE
)

plot.roc(ds_roc$esperado, ds_roc3$modelo, percent=TRUE, col="#21918c", lwd=4, print.auc=TRUE,
    add=TRUE, print.auc.y=20
)

# legend(
#     "bottomright",
#     legend=c(
#         "Relevantes = coef similiradade >= 0.6"
#         ),
#     col=c("#21918c"),
#     lwd=4
# )
