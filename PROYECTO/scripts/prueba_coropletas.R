library(rio)
library(tidyverse)
library(ggplot2)


#- file_to_download <- "rankings_1973-2017.csv"
#- url <- paste0("https://datahub.io/sports-data/atp-world-tour-tennis-data/r/7.html", file_to_download)
#- download.file(url, destfile = file_to_download)

#- file_to_download <- "player_overviews_unindexed_csv.csv"
#- url <- paste0("https://datahub.io/sports-data/atp-world-tour-tennis-data/r/8.html", file_to_download)
#- download.file(url, destfile = file_to_download)


atp_database_1 <- import("./rankings_1973-2017.csv")

atp_database_2 <- import("./player_overviews_unindexed_csv.csv")

df <- full_join(atp_database_1,atp_database_2, by = "player_id")

df <- df %>% drop_na()

#- str(df)
#- names(df)

df1 <- df %>% select(-c(rank_text, move_positions, move_direction, player_url.x, player_url.y, player_id, player_slug.y,
    residence, birthdate, birth_year, birth_month, birth_day, turned_pro, weight_lbs, height_ft, height_inches)) %>%
    rename(player = player_slug.x, date = week_title, month = week_month, year = week_year, country = flag_code)

df2 <- df1 %>% mutate(date = gsub(".", "-", date, fixed = TRUE)) %>%
    mutate(player = gsub("-", "_", player, fixed = TRUE)) %>%
    mutate(player = gsub("%20", "_", player, fixed = TRUE)) %>%
    mutate(player = gsub("juan_martin_del_potro", "del_potro", player, fixed = TRUE))

df3 <- janitor::clean_names(df2, case = "snake")

df3 <- df3 %>%
    mutate(country = gsub("SUI", "CHE", country, fixed = TRUE)) %>%
    mutate(country = gsub("BUL", "BGR", country, fixed = TRUE)) %>%
    mutate(country = gsub("GER", "DEU", country, fixed = TRUE)) %>%
    mutate(country = gsub("CRO", "HRV", country, fixed = TRUE)) %>%
    mutate(country = gsub("RSA", "RUS", country, fixed = TRUE))


df4 <- df3 %>%
    filter((year > 2013) & (rank_number < 1001)) %>%
    arrange(date) %>%
    group_by(player, year) %>%
    slice(n()) %>%
    ungroup()

df5 <- df4 %>%
    arrange(desc(ranking_points))


df6 <- df5 %>%
    group_by(year,country) %>%
    summarise(sum(ranking_points))

df7 <- df6 %>%
    mutate(puntos_pais = `sum(ranking_points)`) %>%
    arrange(desc(puntos_pais))



#rm(df_full)

library("rnaturalearth")
library("rnaturalearthdata")
world <- ne_countries(scale = "medium", returnclass = "sf")
world <- world %>% filter(subregion != "Antarctica") %>% filter(admin != "Greenland")
ggplot() + geom_sf(data = world) + theme_void()
world <- world %>% select(name, iso_a3, geometry)

df_world <- left_join(df7, world, by = c("country" = "iso_a3"))

df8 <- df_world %>% select(country, puntos_pais, geometry)


p <- ggplot() +
    geom_sf(data = world) +
    geom_sf(data = df8, aes(geometry = geometry, fill = puntos_pais)) +
    theme_void()

p + scale_fill_viridis_c(option = "plasma", trans = "sqrt") +
    labs(title = "TOTAL PUNTOS TOP 1000 ATP",
       subtitle = "(diferenciado por país)",
       caption = "Datos obtenidos de ATP World Tour") +
     theme(plot.subtitle = element_text(hjust = 0.5),
    plot.title = element_text(hjust = 0.5)) +
    facet_wrap(vars(year), nrow = 2, ncol = 2) +
    theme(panel.background = element_rect(fill = "deepskyblue2"))











