

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
    rename(player = player_slug.x, date = week_title, month = week_month, year = week_year)

df2 <- df1 %>% mutate(date = gsub(".", "-", date, fixed = TRUE)) %>%
    mutate(player = gsub("-", "_", player, fixed = TRUE)) %>%
    mutate(player = gsub("%20", "_", player, fixed = TRUE)) %>%
    mutate(player = gsub("juan_martin_del_potro", "del_potro", player, fixed = TRUE))

df3 <- janitor::clean_names(df2, case = "snake")

#- rm(atp_database_1, atp_database_2, df1, df2)

#- names(df3)

df4 <- df3 %>%
  select(player, date, ranking_points, rank_number, year) %>%
  filter((rank_number < 11) & (year == 2015)) %>% group_by(player, date) %>% ungroup()

img <- jpeg::readJPEG("./imagenes/pista.JPG")

p <- ggplot(df4, aes(rank_number, group = player,
                     fill = as.factor(player), color = as.factor(player))) +

  ggpubr::background_image(img) +

  geom_tile(aes(y = ranking_points/2,
                height = ranking_points,
                width = 0.9), alpha = 0.8, color = NA) +
    geom_text(aes(y = 0, label = paste(player, " ")), vjust = 0.2, hjust = 1) +

  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +

  labs(title='{closest_state}', x = "", y = "RANKING ATP MASCULINO") +
  theme(plot.title = element_text(hjust = 0, size = 22),
        axis.ticks.y = element_blank(),
        axis.text.y  = element_blank(),
        plot.margin = margin(1,1,1,4, "cm")) +
  geom_text(aes(y = ranking_points,label = ranking_points, hjust=0)) +
    transition_states(date, transition_length = 4, state_length = 1) +
  ease_aes('cubic-in-out')

animate(p, fps = 10, duration = 10, width = 800, height = 600)

