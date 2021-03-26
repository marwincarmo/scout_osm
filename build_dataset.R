library(tidyverse)
data <- read_rds("data/players.rds")

glimpse(data)

data <- data %>% 
  select(squad_number, full_name, age, specific_position, position, stat_att, stat_def,
         stat_ovr, value, team_name, nationality, league_name) %>% 
  mutate(specific_position = case_when(
    specific_position == 4 ~ "ST",
    specific_position == 8 ~ "LF",
    specific_position == 11 ~ "RF",
    specific_position == 1 ~ "CAM",
    specific_position == 2 ~ "CM",
    specific_position == 5 ~ "CDM",
    specific_position == 7 ~ "LM",
    specific_position == 10 ~ "RM",
    specific_position == 3 ~ "CB",
    specific_position == 9 ~ "LB",
    specific_position == 12 ~ "RB",
    specific_position == 6 ~ "GK"),
    specific_position = fct_reorder(specific_position, position)) %>% 
  mutate(stat_main = case_when(
    position == 1 ~ stat_att,
    position == 2 ~ stat_ovr,
    position %in% c(3,4) ~ stat_def,
  )) %>% 
  mutate(
    est_min = round(value * 1.25),
    est_max = round(value * 1.50), 
    .after = value) %>% 
  mutate(quality = case_when(
    stat_main %in% 50:64 ~ "50-64",
    stat_main %in% 65:74 ~ "65-74",
    stat_main %in% 75:84 ~ "75-84",
    stat_main >= 85 ~ "85+"
  ))

saveRDS(data, "data/data_app-v2")
