
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
na.omit(df)

#- str(df)
#- length(df)
#- names(df)

df1 <- df %>% select(-c(move_positions, move_direction, player_url.x, player_url.y, player_id, player_slug.y,
    residence, birthdate, birth_year, birth_month, birth_day, turned_pro, weight_lbs, height_ft, height_inches)) %>%
    rename(player = player_slug.x, date = week_title, month = week_month, year = week_year, country = flag_code)

#- names(df1)

df2 <- df1 %>% mutate(date = gsub(".", "-", date, fixed = TRUE)) %>%
    mutate(player = gsub("-", "_", player, fixed = TRUE)) %>%
    mutate(player = gsub("%20", "_", player, fixed = TRUE)) %>%
    mutate(player = gsub("juan_martin_del_potro", "del_potro", player, fixed = TRUE))

df3 <- janitor::clean_names(df2, case = "snake")

#- rm(atp_database_1, atp_database_2, df1, df2)

#- names(df3)

df4 = df3[ , c(1,2,3,4,10,13,8,6,5,9,11,12,7,14,15,16,17,18)]

df5 <- df4 %>% select(-c(rank_text)) %>% filter(rank_number < 11) %>% arrange(desc(date))

# df6 <- df4 %>% select(-c(rank_text)) %>% filter((rank_number < 11) & (date == "2017-11-20"))

# knitr::kable(df6, format = "html") %>%
# kableExtra::kable_styling(bootstrap_options = c("striped", "hover"))


rpivotTable::rpivotTable(df5, rows = "player", width = "100%", height = "400px")

























