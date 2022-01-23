# ==================================================
# 
# Análise do conjunto de dados que tem texto e idioma
# 
# ==================================================

source("texts_and_langs_functions.R")
source("graphics.R")


readRenviron(".appenv")

ABSTRACTS_ANALYSIS_ENGLISH_PRESENCE_FILE_PATH <- Sys.getenv("ABSTRACTS_ANALYSIS_ENGLISH_PRESENCE_FILE_PATH")

ds_english_presence <- read_csv_file(ABSTRACTS_ANALYSIS_ENGLISH_PRESENCE_FILE_PATH)


ds_g1 <- ds_english_presence %>%
    group_by(subject_areas) %>%
    mutate(total_by_subject_area=sum(total)) %>%
    ungroup()

ds_g <- ds_g1 %>%
    mutate(percent_by_subject_area=total*100/total_by_subject_area) %>%
    mutate(display_numbers = sprintf("%1.0f (%0.2f%%)", total, percent_by_subject_area))

g_english_presence <- ds_g %>%
    select(english_presence) %>%
    distinct()
g_english_presence <- g_english_presence$english_presence
g_subject_areas <- ds_g %>%
                   select(subject_areas) %>%
                   distinct()
g_subject_areas <- g_subject_areas$subject_areas
PALETTE<- c('#440154', '#fdb130', '#86d549')

labels <- paste(ds_g$percent_by_subject_area)

g <- ds_g %>%
    ggplot() +
    geom_bar(
        width=0.5,
        aes(x=subject_areas, y=total+5999, fill=english_presence),
        stat="identity", 
        position=position_dodge(width = 1),
    ) +
    geom_text(
        aes(x=subject_areas, y=total+5999, label=display_numbers, group=english_presence),
        hjust = -0.1, size = 2.8,
        position=position_dodge(width = 1)
    ) +
    scale_fill_manual(
        name=NULL,
        breaks=g_english_presence,
        values=PALETTE
    ) +
    coord_flip() +
    xlab("") +
    ylab("") + 
    theme_linedraw() +
    expand_limits(y = c(-10, max(ds_g$total)*1.4)) +
    theme(panel.grid.minor.y = element_line(color = "red",
                                          size = 0.5,
                                          linetype = 2))


folder_path = dirname(ABSTRACTS_ANALYSIS_ENGLISH_PRESENCE_FILE_PATH)
save_g(g, file.path(folder_path, 'english_presence.pdf'))
save_g(g, file.path(folder_path, 'english_presence.svg'))


