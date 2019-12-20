
library(rio)
library(tidyverse)
library(ggplot2)
library(gganimate)

#- file_to_download <- "rankings_1973-2017.csv"
#- url <- paste0("https://datahub.io/sports-data/atp-world-tour-tennis-data/r/7.html", file_to_download)
#- download.file(url, destfile = file_to_download)

#- file_to_download <- "player_overviews_unindexed_csv.csv"
#- url <- paste0("https://datahub.io/sports-data/atp-world-tour-tennis-data/r/8.html", file_to_download)
#- download.file(url, destfile = file_to_download)


atp_database_1 <- import("./rankings_1973-2017.csv")

atp_database_2 <- import("./player_overviews_unindexed_csv.csv")

df <- full_join(atp_database_1,atp_database_2, by = "player_id")

#- str(df)
#- names(df)

df1 <- df %>% select(-c(move_positions, move_direction, player_url.x, player_url.y, player_id, player_slug.y,
    residence, birthdate, birth_year, birth_month, birth_day, turned_pro, weight_lbs, height_ft, height_inches)) %>%
    rename(player = player_slug.x, date = week_title, month = week_month, year = week_year, country = flag_code)

df2 <- df1 %>% mutate(date = gsub(".", "-", date, fixed = TRUE)) %>%
    mutate(player = gsub("-", "_", player, fixed = TRUE)) %>%
    mutate(player = gsub("%20", "_", player, fixed = TRUE)) %>%
    mutate(player = gsub("juan_martin_del_potro", "del_potro", player, fixed = TRUE))

df3 <- janitor::clean_names(df2, case = "snake")

#- rm(atp_database_1, atp_database_2, df1, df2)

df4 <- df3 %>% select(date, year, month, country, player, rank_number, ranking_points) %>%
    group_by(date) %>%
    filter((year ==  2010) & (rank_number < 16)) %>%
    group_by(player, date) %>%
    arrange(date) %>% mutate(day = as.numeric(as.Date(date) - 14612)) %>% as.data.frame() %>%
    ungroup()


ggplot(data = df4, aes(x = day, y = rank_number, group = player)) +
  geom_line(aes(color = , alpha = 1), size = 2) +
  geom_point(aes(color = player, alpha = 1), size = 4) +
  scale_y_reverse(breaks = 1:nrow(df4))

my_theme <- function() {

  # Colors
  color.background = "white"
  color.text = "#22211d"

  # Begin construction of chart
  theme_bw(base_size=15) +

    # Format background colors
    theme(panel.background = element_rect(fill=color.background, color=color.background)) +
    theme(plot.background  = element_rect(fill=color.background, color=color.background)) +
    theme(panel.border     = element_rect(color=color.background)) +
    theme(strip.background = element_rect(fill=color.background, color=color.background)) +

    # Format the grid
    theme(panel.grid.major.y = element_blank()) +
    theme(panel.grid.minor.y = element_blank()) +
    theme(axis.ticks       = element_blank()) +

    # Format the legend
    theme(legend.position = "none") +

    # Format title and axis labels
    theme(plot.title       = element_text(color=color.text, size=20, face = "bold")) +
    theme(axis.title.x     = element_text(size=14, color="black", face = "bold")) +
    theme(axis.title.y     = element_text(size=14, color="black", face = "bold", vjust=1.25)) +
    theme(axis.text.x      = element_text(size=10, vjust=0.5, hjust=0.5, color = color.text)) +
    theme(axis.text.y      = element_text(size=10, color = color.text)) +
    theme(strip.text       = element_text(face = "bold")) +

    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}

show.top.n <- 10

p <- ggplot(data = df4, aes(x = day, y = rank_number, group = player)) +
  geom_line(aes(color = player, alpha = 1), size = 2) +
  geom_point(aes(color = player, alpha = 1), size = 4) +
  geom_point(color = "#FFFFFF", size = 1) +
  scale_y_reverse(breaks = 1:show.top.n) +
  scale_x_continuous(breaks = waiver(), minor_breaks = waiver(), expand = c(.05, .05)) +
    geom_text(data = df4 %>% filter(day == "1"),
             aes(label = player, x = 1) , hjust = 0, fontface = "bold", color = "black", size = 4) +
    geom_text(data = df4 %>% filter(day == "358"),
            aes(label = player, x = 350) , hjust = 0.15, fontface = "bold", color = "black", size = 4) +
  coord_cartesian(ylim = c(1,show.top.n)) +
  theme(legend.position = "none") +
  labs(x = "RANKING ATP 2010 MASCULINO POR DÍA",
       y = "Posición",
       title = "ATP MASTER SERIES 1000",
       subtitle = "TENISTAS SEGÚN SU POSICIÓN EN EL RANKING") +
  my_theme() + theme(plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(size = 8,
        hjust = 0.5), plot.title = element_text(hjust = 0.5)) +labs(x = "  RANKING ATP 2010 MASCULINO POR DÍA",
    caption = "Datos obtenidos de ATP Tour")

plotly::ggplotly(p)










