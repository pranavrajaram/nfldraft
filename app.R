library(tidyverse)
library(nflfastR)
library(ggimage)
library(ggthemes)
library(gt)
library(shiny)

draft_picks <- read_csv("https://raw.githubusercontent.com/leesharpe/nfldata/master/data/draft_picks.csv")

rosters <- fast_scraper_roster(1999:2019) 

draft_picks <- draft_picks %>%
  left_join(rosters, by = c("pfr_name" = "full_name"))

draft <- draft_picks %>%
  group_by(pfr_name) %>%
  summarize(name=first(pfr_name),
            team = first(team.x),
            year = first(season.x),
            pick = first(pick),
            position = first(position.x),
            head = first(headshot_url)) %>% 
  arrange(desc(-year)) %>%
  ungroup() %>%
  select(-pfr_name)


  draft[is.na(draft)] <- "http://static.nfl.com/static/content/public/static/img/fantasy/transparent/200x200/BAN815644.png
"


ui <- shinyUI(fluidPage(
  h1("NFL Draft by Team"),
  h5("Select a year and a team, and see their draft class."),
  p("Made by Pranav Rajaram, H/T @LeeSharpeNFL and @nflfastR for data"),
  fluidRow(
    sidebarLayout(
      sidebarPanel(                                     
        selectInput("year","Choose a Year", choices = draft$year),
        selectInput("team", "Choose a Team", choices = sort(draft$team))
      ),
      mainPanel(
        gt_output(outputId = "table")
      )
    )
  )
))

server <- function(input, output) {
  
  selectedData <- reactive({
    draft %>%
      filter(year == input$year) %>%
      filter(team == input$team) %>%
      arrange(pick) %>%
      gt() %>%
      cols_align(align = "center",
                 columns = vars(pick)) %>%
      tab_options(
        data_row.padding = px(1)
      ) %>%
      tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_column_labels(TRUE)
      ) %>%
      tab_header(
        title = md("Draft Picks"),
      ) %>%
      cols_label(
        name = "Name",
        head = "",
        team = "TM",
        year = "Year",
        pick = "Pick",
        position = "Pos"
      ) %>%
      opt_all_caps() %>%
      tab_options(
        table.background.color = "white",
        column_labels.background.color = "purple",
        #row.striping.background_color = "#e0e0e0"
      ) %>%
      opt_row_striping() %>%
      opt_table_font(
        font = list(
          google_font("Calibri"),
          default_fonts()
        )
      ) %>%
      text_transform(
        locations = cells_body(columns = vars(head)),
        fn = function(x){
          gt::web_image(x)
        }
      ) %>%
      data_color(
        columns = vars(pick),
        colors = scales::col_numeric(
          palette = c("#3fc1c9", "white"),
          domain = NULL
        )
      ) %>%
      tab_source_note(
        source_note = gt::html(
          htmltools::tags$a(
            href = "https://twitter.com/_pranavrajaram", 
            target = "_blank", 
            "@_pranavrajaram"
          ) %>% 
            as.character()
        )
      )
  })
  
  output$table <-
    render_gt(expr = selectedData(), height = "100%", width = "100%")
}


shinyApp(ui, server)


