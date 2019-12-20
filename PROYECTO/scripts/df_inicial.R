
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

df4 <- df3 %>% select(date, year, month, player, rank_number, ranking_points) %>%
    group_by(date) %>%
    filter((year ==  2010) & (rank_number < 11)) %>%
    group_by(player, date) %>%
    arrange(date) %>% mutate(day = as.numeric(as.Date(date) - 14612)) %>% as.data.frame() %>%
    ungroup()

