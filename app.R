library(shiny)
library(reactable)
library(tidyverse)

osm_df <- read_rds("data/data_app")

no_filter <- function(input, val) {
  if (is.null(input)) {
    unique(val)
  } else {
    input
  }
}

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(inputId = "position",
                         label = "Player's position",
                         choices = sort(unique(osm_df$specific_position)),
                         inline = TRUE
      ),
     sliderInput(inputId = "att",
                      label = "Player's attack",
                      min = min(osm_df$stat_att),
                      max = max(osm_df$stat_att),
                      value = c(min(osm_df$stat_att),
                                max(osm_df$stat_att)),
                      step = 1
      ),
     sliderInput(inputId = "def",
                      label = "Player's defense",
                      min = min(osm_df$stat_def),
                      max = max(osm_df$stat_def),
                      value = c(min(osm_df$stat_def),
                                max(osm_df$stat_def)),
                      step = 1
      ),
      sliderInput(inputId = "ovr",
                      label = "Player's overall",
                      min = min(osm_df$stat_ovr),
                      max = max(osm_df$stat_ovr),
                      value = c(min(osm_df$stat_ovr),
                                max(osm_df$stat_ovr)),
                      step = 1
      ),
      sliderInput(inputId = "age",
                      label = "Player's age",
                      min = min(osm_df$age),
                      max = max(osm_df$age),
                      value = c(min(osm_df$age),
                                max(osm_df$age)),
                      step = 1
      ),
      sliderInput(inputId = "value",
                      label = "Player's value",
                      min = min(osm_df$value),
                      max = max(osm_df$value),
                      value = c(min(osm_df$value), 
                                max(osm_df$value)),
                      pre = "$", sep = ",",
                      step = 1000
      ),
      selectInput(inputId = "nationality",
                      label = "Player's nationality",
                      choices = sort(unique(osm_df$nationality)),
                      multiple = TRUE
      ),
      selectizeInput(inputId = "name",
                         label = "Player's name",
                         choices = NULL,
                         multiple = TRUE
      ),
      selectInput(inputId = "league",
                      label = "League",
                      choices = sort(unique(osm_df$league_name)),
                      multiple = TRUE
      ),
      selectizeInput(inputId = "team",
                         label = "Team",
                         choices = NULL,
                         multiple = TRUE
      )
    ),
    mainPanel(reactableOutput("players_table"))
  )
)

server <- function(input, output, session){
  
  updateSelectizeInput(session, 'name', 
                       choices = sort(unique(osm_df$full_name)), server = TRUE)
  updateSelectizeInput(session, 'team', 
                       choices = sort(unique(osm_df$team_name)), server = TRUE)
  
  table_df <- osm_df
  nat <- reactive(no_filter(input$nationality, osm_df$nationality))
  pos <- reactive(no_filter(input$position, osm_df$specific_position))
  nome <- reactive(no_filter(input$name, osm_df$full_name))
  liga <- reactive(no_filter(input$league, osm_df$league_name))
  time <- reactive(no_filter(input$team, osm_df$team_name))
  
  output$players_table <- renderReactable({
    
    table_df %>% 
      filter(age %in% seq(min(input$age), max(input$age)),
             value %in% seq(min(input$value), max(input$value)),
             stat_att %in% seq(min(input$att), max(input$att)),
             stat_def %in% seq(min(input$def), max(input$def)),
             stat_ovr %in% seq(min(input$ovr), max(input$ovr)),
             nationality %in% nat(),
             specific_position %in% pos(),
             full_name %in% nome(),
             league_name %in% liga(),
             team_name %in% time()) %>% 
      reactable(minRows = 10,
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
                  nationality = colDef(name = "Nationality"),
                  league_name = colDef(name = "League")
                ),
                wrap = FALSE, 
                showPageSizeOptions = TRUE, 
                highlight = TRUE,
                paginationType = "jump")
  })
}

shinyApp(ui = ui, server = server)