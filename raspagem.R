library(tidyverse)
library(xml2)
library(httr)
library(jsonlite)
library(progressr)

u_leagues <- "https://web-api.onlinesoccermanager.com/api/v1/leagueTypes/"

r_leagues <- GET(u_leagues)

leagues <- content(r_leagues, "text") %>% 
  jsonlite::fromJSON(simplifyDataFrame = TRUE) %>% 
  as_tibble() %>% 
  janitor::clean_names()


# extracting ids from leagues that can be scoutable
teams <- leagues %>% 
  filter(is_scoutable == TRUE) %>% 
  group_by(id, team_count) %>% 
  mutate(team_id = list(1:team_count)) %>% 
  unnest(cols = c(team_id)) %>% 
  ungroup() %>% rename(league_id = id) %>% 
  select(league_id, team_id)

get_info <- function(league_id, team_id, prog) {
  if (!missing(prog)) prog()
  Sys.sleep(1)
  # leagues info
  u_leagues <- "https://web-api.onlinesoccermanager.com/api/v1/leagueTypes/"
  ligas <- fromJSON(content(GET(u_leagues), "text"), simplifyDataFrame = TRUE) %>% 
    as_tibble() %>% 
    janitor::clean_names() %>% 
    select(id, name)
  # teams info
  u_teams <- paste0(u_leagues, league_id, "/teams/")
  times <- fromJSON(content(GET(u_teams), "text"), simplifyDataFrame = TRUE) %>% 
    as_tibble() %>% 
    janitor::clean_names() %>% 
    select(league_type_id, name, id)
  # players_info
  u_players <- paste0(u_teams, team_id, "/players")
  jogadores <- fromJSON(content(GET(u_players), "text"), simplifyDataFrame = TRUE) %>% 
    as_tibble() %>% 
    mutate(nat_id = nationality$id,
           nat_code = nationality$code,
           nationality_name = nationality$name) %>% 
    janitor::clean_names() %>% 
    select(-nationality)
  # merging dfs
  liga_time <- left_join(times, ligas, by = c("league_type_id" = "id")) %>% 
    select(-name.y) %>% rename(name = name.x)
  player_info <- left_join(jogadores, liga_time, by = c("team_id" = "id")) %>% 
    rename(player_name = name.x,
           team_name = name.y,
           nationality = nationality_name)
  player_info
}

players_df <- with_progress({
  p <- progressr::progressor(nrow(teams))
  pmap_dfr(teams, ~get_info(.x, .y, p))
})

league_names <- leagues %>% 
  filter(is_scoutable == TRUE) %>% 
  select(id, name) %>% 
  rename(league_name = name)

players_df <- players_df %>%
  left_join(league_names, by = c("league_type_id" = "id"), 
            .keep = FALSE) %>% 
  mutate(age_category = factor(case_when(
    age < 25 ~ "Young",
    age %in% 25:29 ~ "Experienced",
    age > 29 ~ "Veteran"
  ), levels = c("Young", "Experienced", "Veteran")), 
  .after = "age")


glimpse(players_df)
saveRDS(players_df, "data/db_21-03-26.rds")
