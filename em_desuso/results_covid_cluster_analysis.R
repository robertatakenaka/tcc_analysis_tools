
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
source("my_functions_files.R")
source("my_functions_translated_codes.R")
source("my_functions_graphics.R")

options(digits = 2)
options(scipen = 999)


readRenviron("envs/results_covid.env")

PAPERS_CSV_FILE_PATH <- Sys.getenv("PAPERS_CSV_FILE_PATH")
SOURCE_CSV_FILE_PATH <- Sys.getenv("SOURCE_CSV_FILE_PATH")

# pid,refs,refs_with_doi,connections,
# score_gt_89,score_gt_79,score_gt_69,score_gt_59,score_gt_0-59,score_none,
# no_en,en_only,with_en

ds_papers_csv <- read_csv_file(PAPERS_CSV_FILE_PATH)
 # %>%
 #    mutate(high_scores=score_gt_89+score_gt_79) %>%
 #    group_by(high_scores) %>%
 #    mutate(n_high_scores=n())
    

ds_papers <- ds_papers_csv

ds_papers["no_en"] <- NULL
ds_papers["en_only"] <- NULL
ds_papers["with_en"] <- NULL

########################################
#
#     CLUSTER HIERARQUICO - MCDonald
#
########################################

#transformar o nome dos artigos em linhas
#rownames(ds_papers) <- ds_papers[,1]
ds_papers <- ds_papers[,-1]

#Padronizar variaveis
ds_papers.padronizado <- scale(ds_papers)

#calcular as distancias da matriz utilizando a distancia euclidiana
distancia <- dist(ds_papers.padronizado, method = "euclidean")

#Calcular o Cluster: metodos disponiveis "average", "single", "complete" e "ward.D"
cluster.hierarquico <- hclust(distancia, method = "single" )

# Dendrograma
plot(cluster.hierarquico, cex = 0.6, hang = -1)

#Criar o grafico e destacar os grupos
rect.hclust(cluster.hierarquico, k = 2)

#VERIFICANDO ELBOW 
fviz_nbclust(ds_papers.padronizado, FUN = hcut, method = "wss")


#criando 4 grupos de artigos
ds_with_4groups <- cutree(cluster.hierarquico, k = 5)
table(ds_with_4groups)

#transformando em data frame a saida do cluster
grupos_de_artigos <- data.frame(ds_with_4groups)

#juntando com a base original
Base_artigos_fim <- cbind(ds_papers, grupos_de_artigos)

#FAZENDO ANALISE DESCRITIVA
#MEDIAS das variaveis por grupo
# pid,refs,refs_with_doi,connections,
# score_gt_89,score_gt_79,score_gt_69,score_gt_59,score_gt_0-59,score_none,
# no_en,en_only,with_en

mediagrupo <- Base_artigos_fim %>% 
  group_by(ds_with_4groups) %>% 
  summarise(n = n(),
            refs = mean(refs), 
            refs_with_doi = mean(refs_with_doi), 
            score_gt_89 = mean(score_gt_89),
            score_gt_79 = mean(score_gt_79), 
            score_gt_69 = mean(score_gt_69), 
            score_gt_59 = mean(score_gt_59),
            score_gt_0.59 = mean(score_gt_0.59), 
            score_none = mean(score_none)
  )
mediagrupo


########################################
#
#    CLUSTER NAO HIERARQUICO - Mcdonald
#
########################################

#AGRUPANDO LANCHES PELO METODO NAO HIERARQUICO

#Rodar o modelo
ds_papers.k2 <- kmeans(ds_papers.padronizado, centers = 2)

#Visualizar os clusters
fviz_cluster(ds_papers.k2, data = ds_papers.padronizado, main = "Cluster K2")

#Criar clusters
ds_papers.k3 <- kmeans(ds_papers.padronizado, centers = 3)
ds_papers.k4 <- kmeans(ds_papers.padronizado, centers = 4)
ds_papers.k5 <- kmeans(ds_papers.padronizado, centers = 5)

#Criar graficos
G1 <- fviz_cluster(ds_papers.k2, geom = "point", data = ds_papers.padronizado) + ggtitle("k = 2")
G2 <- fviz_cluster(ds_papers.k3, geom = "point", data = ds_papers.padronizado) + ggtitle("k = 3")
G3 <- fviz_cluster(ds_papers.k4, geom = "point", data = ds_papers.padronizado) + ggtitle("k = 4")
G4 <- fviz_cluster(ds_papers.k5, geom = "point", data = ds_papers.padronizado) + ggtitle("k = 5")

#Imprimir graficos na mesma tela
grid.arrange(G1, G2, G3, G4, nrow = 2)

#VERIFICANDO ELBOW 
fviz_nbclust(ds_papers.padronizado, kmeans, method = "wss")

ds_papers.k6 <- kmeans(ds_papers.padronizado, centers = 6)
G6 <- fviz_cluster(ds_papers.k6, geom = "point",  data = ds_papers.padronizado) + ggtitle("k = 6")
G6


grupos <- ds_papers.k6
papers_scaled <- ds_papers.padronizado

papers_df <- as.data.frame(papers_scaled) %>% rownames_to_column()
cluster_pos <- as.data.frame(grupos$cluster) %>% rownames_to_column()
colnames(cluster_pos) <- c("rowname", "cluster")

papers_scaled_final <- inner_join(cluster_pos, papers_df, by="rowname")
clusters <- papers_scaled_final %>%
    select(rowname, cluster)
colnames(clusters) <- c("pid", "cluster")
ds_papers_with_groups <- inner_join(ds_papers_csv, clusters, by="pid") %>%
    write_csv_file(CLUSTERED_PAPERS_CSV_FILE_PATH)


