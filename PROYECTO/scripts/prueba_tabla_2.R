
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

df1 <- df %>% select(-c(move_positions, move_direction,
      player_url.x, player_url.y, player_id, player_slug.y,
      residence, birthdate, birth_month,
      birth_day, weight_lbs, height_ft, height_inches)) %>%

      rename(player = player_slug.x, date = week_title,
      month = week_month, year = week_year, country = flag_code)

df1 <- df1 %>%
      mutate(date = gsub(".", "-", date, fixed = TRUE)) %>%
      mutate(player = gsub("-", "_", player, fixed = TRUE)) %>%
      mutate(player = gsub("%20", "_", player, fixed = TRUE)) %>%
      mutate(player = gsub("juan_martin_del_potro",
      "del_potro", player, fixed = TRUE))

df2 <- janitor::clean_names(df1, case = "snake")


df3 <- df2 %>% filter((date == "2017-11-20") & (rank_number < 101)) %>% select(rank_number, player, player_age, handedness, backhand)

df4 <- df3 %>% group_by(handedness) %>% summarise(NN = n()) %>% mutate(percent = NN / sum(NN))

knitr::kable(df4)

df3 <- df2 %>% group_by(backhand) %>% summarise(NN = n()) %>% mutate(percent = NN / sum(NN))

knitr::kable(df3)

df3 <- df2 %>% filter((date == "2016-12-26" | date == "2017-11-20" | date == "2015-12-28" | date == "2014-12-29" | date == "2013-12-30") & rank_number < 101)

df4 <- df3 %>%
  group_by(date) %>%
  summarise(Altura_media = mean(height_cm , na.rm = TRUE),
      Peso_medio  = mean(weight_kg, na.rm = TRUE) ) %>% ungroup()

knitr::kable(df4)

df3 <- df2 %>%
  filter((date == "2017-11-20") & (rank_number < 6)) %>%
  mutate(anyos_hasta_convertirse_en_pro = (turned_pro - birth_year)) %>%
  arrange(rank_number) %>%
  select(player, birth_year, turned_pro ,anyos_hasta_convertirse_en_pro)

knitr::kable(df3)











