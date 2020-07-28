library(shiny)


df <- read.csv("/home/john/stats_corner/2016/shiny_apps/snake_draft/v2_5/faa_projection_data.csv",
#df <- read.csv("/srv/shiny-server/stats-corner/2016/snake-assistant/faa_projection_data.csv",
               stringsAsFactors = FALSE,
               header = TRUE)
fluidPage(
  headerPanel('Snake Draft Assistant 2.6'),
  sidebarLayout(position = "right",
    sidebarPanel(
      wellPanel(
        h4("Your Next Picks are:"),
        textOutput("next_pick"),
        textOutput("next_pick1"),
        textOutput("next_pick2")
      ),
      wellPanel(
        h4("Drafted Players"),
        selectizeInput("drafted_players", label = "Enter Players as they get Drafted", multiple = TRUE, choices = df$player_team)
      )
    ),
    mainPanel(
        tabsetPanel(
           tabPanel("Quick Start",
                     p("1. Enter your draft and team parameters below."),
                     p("2. As all players, not just yours, get drafted enter their names into the 'Drafted Players' window on the right."),
                     p("3. As you draft players, enter them on the 'Your Team' tab."),
                    column(6,
                      h4("Draft Parameters"),
                      numericInput("first_pick",  label = h6("Round 1 Pick #"), value = 1),
                      numericInput("league_teams",  label = h6("How Many Teams in League?"), value = 10),
                      selectInput("scoring_format", label = h6("Scoring Format"), choices = c("Standard","PPR"), selected = "Standard"),
                      selectInput("extra_pos", label = h6("Additional Positions"), choices = c("FLEX","OP"), selected = "FLEX")
                    ),
                    column(6,
                      h4("Team Parameters"),
                      numericInput("num_qb",   label = "# QB",   value = 1),
                      numericInput("num_rb",   label = "# RB",   value = 2),
                      numericInput("num_wr",   label = "# WR",   value = 3),
                      numericInput("num_te",   label = "# TE",   value = 1),
                      numericInput("num_flex", label = "# FLEX", value = 1),
                      numericInput("num_op",   label = "# OP",   value = 0),
                      numericInput("num_k",    label = "# K",    value = 1),
                      numericInput("num_dst",  label = "# DST",  value = 1)
                    )
          ),
          tabPanel("Recomendations",
                   h4("Value Added (Per Game) and Dropoffs of Best Available (BA) Now and Next Time (BANT)"),
                   radioButtons("one_or_two", label = h4("Recommend Based on One Pick From Now or Two?"),
                                 choices = list("One" = 1, "Two" = 2), 
                                 selected = 1,
                                 inline = T),
                    dataTableOutput("rec_table")
          ),
          tabPanel("Your Team",
                    column(6,
                           h5("Your Team"),
                           selectizeInput("your_team", label = "Enter Players YOU Drafted", multiple = TRUE, choices = df$player_team)
                    ),
                    column(6,
                           h5("Weekly Expected Points From Starting Lineup"),
                           dataTableOutput("optimized_lineup"))
          ),
          tabPanel("About",
                    a("Projection and ADP data downloaded from Fantasy Football Analytics",     
                      href="http://fantasyfootballanalytics.net/"),
                    p("Data last updated on September 4, 2016"),
                    p("Questions? Email me: StatsCorner@gmail.com"),
                    p(""),
                    p("App Updated on 2017-04-06, version 2.6")
          )
        )
      )
    )
)


