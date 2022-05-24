# library
#install.packages(hrbrthemes)
# install.packages("ggplot2")
# install.packages("viridis")
# install.packages("dplyr")
# install.packages("ggeasy")
# install.packages("stringr")
# install.packages("patchwork")

library("ggplot2")
library("dplyr")
library("viridis")
library("forcats")
library("tools")
# library("ggeasy")
# library("stringr")
# library("patchwork")

# options(digits = 2)
# options(scipen = 999)


save_g <- function(graphic, file_path){
    path <- dirname(file_path)
    dir.create(path, recursive=TRUE, showWarnings=FALSE)

    ext = file_ext(file_path)
    if (ext[1] == "svg") {
        svg(file_path)
    } else if (ext[1] %in% c("jpeg", "jpg")) {
        jpeg(file_path)
    } else if (ext[1] == "png") {
        png(file_path)
    } else {
        pdf(file_path)
    }
    print(graphic)
    dev.off()
    print(file_path)
    return (graphic)
}


xxx <- function(graphic, file_path) {

    ggsave(filename = file_path, plot = graphic, width=30, height=15, units="cm")

    return (graphic)

}

graphics <- function(graphic, file_path, width=80, height=40) {

    ggsave(filename = file_path, plot = graphic, width=width, height=height, units="cm", dpi = 600)

    return (graphic)

}
