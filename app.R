library(shiny)
library(reactable)
library(tidyverse)
library(shinydashboard)
library(shinyWidgets)

osm_df <- read_rds("data/data_app-v2")

posicoes <- as.character(sort(unique(osm_df$specific_position)))

no_filter <- function(input, val) {
  if (is.null(input)) {
    unique(val)
  } else {
    input
  }
}


body <- dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
  tabItems(
    tabItem(tabName = "scout_tab",
            fluidRow(
              column(width = 3,
                     box(width = NULL,
                       title = "Position",
                       status = "primary",
                       solidHeader = TRUE,
                       collapsible = TRUE,
                       checkboxGroupButtons(inputId = "position",
                                          label = NULL,
                                          choices = posicoes,
                                          #inline = TRUE,
                                          justified = TRUE,
                                          direction = "vertical",
                                          checkIcon = list(
                                            yes = icon("ok", 
                                                       lib = "glyphicon"))
                       )),
                     box(width = NULL,
                       title = "Attributes",
                       status = "primary",
                       solidHeader = TRUE,
                       collapsible = TRUE,
                       sliderInput(inputId = "main",
                                   label = "Main quality",
                                   min = min(osm_df$stat_main),
                                   max = max(osm_df$stat_main),
                                   value = c(min(osm_df$stat_main),
                                             max(osm_df$stat_main)),
                                   step = 1
                                   ),
                       sliderInput(inputId = "age",
                                   label = "Age",
                                   min = min(osm_df$age),
                                   max = max(osm_df$age),
                                   value = c(min(osm_df$age),
                                             max(osm_df$age)),
                                   step = 1
                                   ),
                       sliderInput(inputId = "value",
                                   label = "Value",
                                   min = min(osm_df$value),
                                   max = max(osm_df$value),
                                   value = c(min(osm_df$value), 
                                             max(osm_df$value)),
                                   post = "M", step = 0.1)
                       ),
                     box(width = NULL,
                         title = "Advanced attributes",
                         status = "primary",
                         solidHeader = TRUE,
                         collapsible = T, collapsed = T,
                         sliderInput(inputId = "att",
                                     label = "Attack",
                                     min = min(osm_df$stat_att),
                                     max = max(osm_df$stat_att),
                                     value = c(min(osm_df$stat_att),
                                               max(osm_df$stat_att)),
                                     step = 1
                         ),
                         sliderInput(inputId = "def",
                                     label = "Defense",
                                     min = min(osm_df$stat_def),
                                     max = max(osm_df$stat_def),
                                     value = c(min(osm_df$stat_def),
                                               max(osm_df$stat_def)),
                                     step = 1
                         ),
                         sliderInput(inputId = "ovr",
                                     label = "Overall",
                                     min = min(osm_df$stat_ovr),
                                     max = max(osm_df$stat_ovr),
                                     value = c(min(osm_df$stat_ovr),
                                               max(osm_df$stat_ovr)),
                                     step = 1
                         )),
                     box(width = NULL,
                         title = "Details",
                         status = "primary",
                         collapsible = TRUE,
                         solidHeader = TRUE,
                         selectInput(inputId = "nationality",
                                     label = "Nationality",
                                     choices = sort(unique(osm_df$nationality)),
                                     multiple = TRUE
                                     ),
                         selectizeInput(inputId = "name",
                                        label = "Name",
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
                                        multiple = TRUE)
                         )
                     ),
              column(width = 9,
                     box(width = NULL,
                         reactableOutput("players_table")
                         )
                     )
              )
            ),
    tabItem(tabName = "nationality_tab",
            fluidRow(
              box(width = NULL,
                  reactableOutput("nationality_table"))
              )
            )
    )
  )

ui <- dashboardPage(
  dashboardHeader(title = "OSM Scout"),
  
  dashboardSidebar(
    collapsed = TRUE,
    sidebarMenu(
      menuItem("Scout",
               tabName = "scout_tab",
               icon = icon("binoculars")),
      menuItem("Nationality",
               tabName = "nationality_tab",
               icon = icon("flag"))
    ),
    selectInput(inputId = "age_cat",
                label = "Filter nationality by age",
                choices = sort(unique(osm_df$age_category)),
                multiple = TRUE
    )
  ),
  body
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
  idade <- reactive(no_filter(input$age_cat, osm_df$age_category))
  
  output$players_table <- renderReactable({
    
    table_df %>% 
      filter(age %in% seq(min(input$age), max(input$age)),
             value >= min(input$value), value <= max(input$value),
             stat_main %in% seq(min(input$main), max(input$main)),
             stat_att %in% seq(min(input$att), max(input$att)),
             stat_def %in% seq(min(input$def), max(input$def)),
             stat_ovr %in% seq(min(input$ovr), max(input$ovr)),
             nationality %in% nat(),
             specific_position %in% pos(),
             full_name %in% nome(), #colocar contains pra pegar nomes nao completos
             league_name %in% liga(),
             team_name %in% time()) %>% 
      reactable(minRows = 10,
                defaultPageSize = 10,
                columns = list(
                  full_name = colDef(name = "Name", minWidth = 150,
                                     resizable = T),
                  squad_number= colDef(name = "Num", width = 50),
                  specific_position= colDef(name = "Pos", width = 50),
                  stat_att = colDef(name = "Att", width = 50),
                  stat_def = colDef(name = "Def", width = 50),
                  stat_ovr = colDef(name = "Ovr", width = 50),
                  age= colDef(name = "Age", width = 50),
                  value = colDef(name = "Value", 
                                 format = colFormat(separators = TRUE, 
                                                    locales = "en-US",
                                                    suffix = "M"),
                                 width = 70),
                  range = colDef(name = "Est. Scout Value", width = 130),
                  team_name = colDef(name = "Team",
                                     width = 130,
                                     resizable = TRUE),
                  nationality = colDef(name = "Nationality",
                                       width = 130),
                  league_name = colDef(name = "League"),
                  position = colDef(show = FALSE),
                  stat_main = colDef(show = FALSE),
                  quality = colDef(show = FALSE),
                  age_category = colDef(show = FALSE)
                ),
                wrap = FALSE, 
                showPageSizeOptions = TRUE, 
                highlight = TRUE,
                paginationType = "jump")
  })
  
  output$nationality_table <- renderReactable({
    table_df %>% 
      filter(age_category %in% idade()) %>% 
      group_by(nationality, position) %>% 
      count() %>% 
      pivot_wider(names_from =  position, values_from = n) %>% 
      mutate(across(c(1:4), ~replace(., is.na(.), 0))) %>% 
      mutate(Total = sum(c_across(1:4)), .after = "nationality") %>% 
      rename("Fowards" = `1`,
             "Midfielders" = `2`,
             "Defenders" = `3`,
             "Goalkeepers" = `4`) %>% 
      arrange(desc(Total)) %>% 
      reactable(
        defaultColDef = colDef(align = "center"),
        minRows = 10,
        columns = list(
          nationality = colDef(name = "Nationality",
                               filterable = TRUE,
                               align = "left")
        ),
        wrap = FALSE, 
        showPageSizeOptions = TRUE, 
        highlight = TRUE,
        paginationType = "jump"
      )
  })
}

shinyApp(ui = ui, server = server)