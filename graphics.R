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

options(digits = 2)
options(scipen = 999)


to_wide <- function(ds_tidy, columns1, columns2) {
    ds <- spread(ds_tidy, key=columns1, value=columns2)
    return (ds)
}


to_long <- function(ds_wide, col_name, row_name) {
    ds <- gather(ds_wide, key=col_name, value=row_name)
    return (ds)
}

sorted_hor_bar <- function(ds, texts, numbers, perc, v_text="", h_text="") {

    ds <- ds %>%
        mutate(texts = fct_reorder(texts, desc(perc)),
               percent=perc*100) %>%
        mutate(display_numbers = sprintf("%1.0f (%0.2f%%)", numbers, percent))

    g <- ds %>%
        ggplot() +
        geom_bar(
            aes(x=texts, y=percent, fill=numbers),
            stat="identity", 
            position=position_dodge(width = 2)
        ) +
        geom_text(
            aes(x=texts, y=percent, label=display_numbers),
            hjust = -0.1, size = 3, vjust = 0, 
            inherit.aes = TRUE
        ) +
        
        viridis::scale_fill_viridis(discrete = FALSE, direction = -1) +
        coord_flip() +
        xlab(v_text) +
        ylab(h_text) + 
        theme_linedraw() +
        expand_limits(y = c(-10, max(ds$percent)*1.8))

    return (g)
}

sorted_hor_bar_groups <- function(ds, texts, numbers, perc, v_text="", h_text="", reorder_by_text=FALSE) {

    if (reorder_by_text == TRUE) {
        ds <- ds %>%
            mutate(texts = fct_reorder(texts, desc(texts)))
        direction <- 1
    } else {
        ds <- ds %>%
            mutate(texts = fct_reorder(texts, desc(perc)))
        direction <- -1
    }
    ds <- ds %>%
        mutate(percent=perc*100) %>%
        mutate(display_numbers = sprintf("%1.0f (%0.2f%%)", numbers, percent))

    print(head(ds))
    g <- ds %>%
        ggplot() +
        geom_bar(
            aes(x=texts, y=percent, fill=numbers),
            stat="identity", 
            position=position_dodge(width = 2),
            width=0.2
        ) +
        geom_text(
            aes(x=texts, y=percent, label=display_numbers),
            hjust = -0.1, size = 3, vjust = 0, 
            inherit.aes = TRUE
        ) +
        
        viridis::scale_fill_viridis(discrete = TRUE, direction = direction) +
        coord_flip() +
        xlab(v_text) +
        ylab(h_text) + 
        theme_linedraw() +
        expand_limits(y = c(-10, max(ds$percent)*1.8))

    return (g)
}

save_g <- function(graphic, file_path){
    ext = file_ext(file_path)
    if (ext[1] == "svg") {
        svg(file_path)
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
