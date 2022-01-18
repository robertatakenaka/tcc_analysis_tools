############################################################################
#
#  Identifica a frequência dos idiomas em resumos, títulos e palavras-chaves
#
############################################################################

library("ggplot2")
library("tidyverse")

options(digits = 2)
options(scipen = 999)
# specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))

evaluate_langs <- function(dataset_path) {
  print(dataset_path)
  ds <- read.csv(dataset_path, header=TRUE, sep=",") %>% distinct()
  langs <- data.frame(table(ds$lang))
  total_obs = sum(langs$Freq)
  return (mutate(langs, perc=Freq*100/total_obs) %>% arrange(desc(Freq)))
}

evaluate_less_freq_langs <- function(ds) {
  less_freq_langs = filter(ds, perc < 20)
  total_obs = sum(less_freq_langs$Freq)
  less_freq_langs <- (mutate(less_freq_langs, perc=Freq*100/total_obs) %>% arrange(desc(Freq)))
  less_freq_langs <- rename(less_freq_langs, "lang"="Var1")
  less_freq_langs$Freq <- NULL
  return (less_freq_langs)
}

make_graphic_1 <- function(dataset, file_path) {
    # PNG device
    png(file_path)

    # Code
    data <- data.frame(
      name=dataset$lang,
      value=dataset$perc
    )

    # barplot
    barplot(height=data$value, names=data$name,
            density=c(5,10,20,30,7),
            angle=c(0,45,90,11,36),
            col="brown")     

    # Close device
    dev.off()

} 


make_graphic_2 <- function(dataset, file_path) {
    # PNG device
    png(file_path)

    data <- data.frame(
      category=dataset$lang,
      fraction=dataset$perc
    )
     
    # Compute the cumulative percentages (top of each rectangle)
    data$ymax = cumsum(data$fraction)

    # Compute the bottom of each rectangle
    data$ymin = c(0, head(data$ymax, n=-1))
     
    # Make the plot
    ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
         geom_rect() +
         coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
         xlim(c(2, 4)) # Try to remove that to see how to make a pie chart


    # Close device
    dev.off()

} 



readRenviron(".appenv")
 # CSV_FOLDER_PATH = "/PATH/csv/"
CSV_FOLDER_PATH <- Sys.getenv("CSV_FOLDER_PATH")
ANALYSIS_LANGS_FOLDER_PATH <- Sys.getenv("ANALYSIS_LANGS_FOLDER_PATH")

FILENAMES <- list("abstracts", "article_titles", "keywords")
for(filename in FILENAMES){
  ds_path = file.path(CSV_FOLDER_PATH, filename)
  ds_path_csv <- paste(ds_path, "csv", sep=".")

  ds_langs <- evaluate_langs(ds_path_csv)
  View(ds_langs)

  ds_less_freq_langs <- evaluate_less_freq_langs(ds_langs)
  View(ds_less_freq_langs)

  new_name = paste(filename, "less_freq_langs.csv", sep="_")
  new_file <- file.path(ANALYSIS_LANGS_FOLDER_PATH, new_name)
  print(new_file)
  write.csv(ds_less_freq_langs, new_file)

  new_name = paste(filename, "less_freq_langs.png", sep="_")
  new_file <- file.path(ANALYSIS_LANGS_FOLDER_PATH, new_name)
  print(new_file)
  make_graphic_2(ds_less_freq_langs, new_file)

}
