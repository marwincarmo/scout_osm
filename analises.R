library(tidyverse)

players <- readRDS("data/players.rds")


players %>% 
  count(nationality, sort = TRUE) %>% 
  print(n = Inf)
levels(players$age_category)

#### Ideias ----

# tabela com nacionalidade, numero de jogadores totais, por categoria de idade
# por posição e por posição específica

# tabela com as informações de todos os jogadores podendo filtrar os campos
# 
# filtrar por faixa de valor com o slider
players %>% count(style)

### Teste ---

library(crosstalk)
library(reactable)

jog <- select(players, squad_number, full_name, age, specific_position, position, stat_att, stat_def,
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
  select(-position)

#saveRDS(jog, "dados_app.rds")
dados <- SharedData$new(jog)

bscols(
  widths = c(3, 9),
  list(
    filter_checkbox("specific_position", "Position", dados, ~specific_position, inline = TRUE, columns = 4),
    filter_slider("age", "Age", dados, ~age, width = "100%", step = 1),
    filter_slider("value", "Value", dados, ~value, width = "100%", step = 100000),
    filter_slider("att", "Attack", dados, ~stat_att, width = "100%", step = 1),
    filter_slider("def", "Defense", dados, ~stat_def, width = "100%", step = 1),
    filter_slider("ovr", "Overall", dados, ~stat_ovr, width = "100%", step = 1),
    filter_select("nationality", "Nationality", dados, ~nationality),
    filter_select("full_name", "Name", dados, ~full_name),
    filter_select("league_name", "League", dados, ~league_name),
    filter_select("team_name", "Team", dados, ~team_name)
  ),
  reactable(dados, minRows = 10,
            columns = list(
    full_name = colDef(name = "Name", minWidth = 150),
    squad_number= colDef(name = "Num", width = 50),
    specific_position= colDef(name = "Pos", width = 50),
    stat_att = colDef(name = "Att", width = 50),
    stat_def = colDef(name = "Def", width = 50),
    stat_ovr = colDef(name = "Ovr", width = 50),
    age= colDef(name = "Age", width = 50),
    value = colDef(name = "Value", 
                   format = colFormat(separators = TRUE, 
                                      locales = "en-US")),
    team_name = colDef(name = "Team"),
    nationality = colDef(name = "Nat"),
    league_name = colDef(name = "League")
  ),
  wrap = FALSE, 
  resizable = TRUE,
  showPageSizeOptions = TRUE, 
  highlight = TRUE,
  paginationType = "jump")
)

# colocar os valores da posição específica como fator tendo como base 
# os níveis de posição