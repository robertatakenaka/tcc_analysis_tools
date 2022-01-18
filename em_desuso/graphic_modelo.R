# library
library(ggplot2)
library(viridis)

# create a dataset
# specie <- c(rep("sorgho" , 3) , rep("poacee" , 3) , rep("banana" , 3) , rep("triticum" , 3) )
# condition <- rep(c("normal" , "stress" , "Nitrogen") , 4)
# value <- abs(rnorm(12 , 0 , 15))
# data <- data.frame(specie,condition,value)


readRenviron(".appenv")

ABSTRACTS_ANALYSIS_LANG_IN_AREA_FILE_PATH <- Sys.getenv("ABSTRACTS_ANALYSIS_LANG_IN_AREA_FILE_PATH")

ds_langs <- get_ds_translation()
print(head(ds_langs))

ds <- read_csv_file(ABSTRACTS_ANALYSIS_LANG_IN_AREA_FILE_PATH)
ds <- add_lang_text(ds, ds_langs)
 
ds2 <-  spread(ds, key="lang_text", value="n")
ds2[is.na(ds2)] <- 0

ds3 <- gather(ds2, key="subject_area", value="n")

# Small multiple
ggplot(data, aes(fill=ds$lang_text, y=ds$n, x=ds$subject_area)) + 
    geom_bar(position="stack", stat="identity") +
    scale_fill_viridis(discrete = T) +
    ggtitle("Studying 4 species..") +
    #theme_ipsum() +
    xlab("")
