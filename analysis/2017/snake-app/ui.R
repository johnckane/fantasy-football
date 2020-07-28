library(shiny)


df <- read.csv("/home/john/stats_corner/2017/snake-app/ffa_data.csv",
#df <- read.csv("/srv/shiny-server/stats-corner/2017/snake-app/ffa_data.csv",
               stringsAsFactors = FALSE,
               header = TRUE)
fluidPage(
  headerPanel('Snake Draft Assistant 3.2'),
  sidebarLayout(position = "right",
    sidebarPanel(
      wellPanel(
        h4("Your Next Picks are:"),
        textOutput("next_pick"),
        textOutput("next_pick1"),
        textOutput("next_pick2")
      )
    ),
    mainPanel(
        tabsetPanel(
           tabPanel("Quick Start",
                     p("1. Enter your draft and  starting lineup parameters below."),
                     p("2. As all players, not just yours, get drafted enter their names into the 'Drafted Players' window on the right."),
                     p("3. As you draft players, enter them on the 'Your Team' tab."),
                    column(6,
                      h4("Draft Parameters"),
                      numericInput("first_pick",  label = h6("Round 1 Pick #"), value = 1),
                      numericInput("league_teams",  label = h6("How Many Teams in League?"), value = 10),
                      selectInput("scoring_format", label = h6("Scoring Format"), choices = c("Standard","PPR"), selected = "Standard"),
                      selectInput("extra_pos", label = h6("Additional Positions"), choices = c("FLEX","OP","None"), selected = "FLEX")
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
          tabPanel("Your Team",
                   column(6,
                          h5("Your Team"),
                          selectizeInput("your_team", label = "Enter Players YOU Drafted", multiple = TRUE, choices = df$player_team)
                   ),
                   column(6,
                          h5("Weekly Expected Points From Starting Lineup"),
                          dataTableOutput("optimized_lineup"))
          ),
          tabPanel("Recomendations",
                   h4("Value Added (Per Game) and Dropoffs of Best Available (BA) Now and Next Time (BANT)"),
                   radioButtons("one_or_two", label = h4("Recommend Based on One Pick From Now or Two?"),
                                 choices = list("One" = 1, "Two" = 2), 
                                 selected = 1,
                                 inline = T),
                    dataTableOutput("rec_table")
          ),
           tabPanel("Do Not Recommend",
                    column(10,
                           h5("Do Not Recommend These Players"),
                           selectizeInput("dnr", label = "Enter Players To Avoid", multiple = TRUE, choices = df$player_team))
                    ),
          tabPanel("Drafted Players",
                  h4("Drafted Players"),
                  selectizeInput("drafted_players", label = "Enter Players as they get Drafted", multiple = TRUE, choices = df$player_team)
                  ),
          tabPanel("Available Players",
                   dataTableOutput("available_players")),
          tabPanel("About",
                    a("Projection and ADP data downloaded from Fantasy Football Analytics",     
                      href="http://fantasyfootballanalytics.net/"),
                    p("Standard Scoring data last updated on August 29, 2017"),
                    p("PPR Scoring data last updated on September 4, 2017"),
                    p("Questions? Email me: StatsCorner@gmail.com"),
                    p(""),
                    p("What's New in version 3.2 (compared to 3.0)?"),
                    p("1. Can specify 'None' in addition to 'FLEX' and 'OP' for additional positions"),
                    p("2. A tab listing ALL available players with Value Added and PPG"),
                    p("3. A 'Do Not Recommend' tab that removes players from your recommendations"),
                    p("4. Added player age to Recommendations table"),
                    p("5. Separate tab for drafted players"),
                    p(""),
                    p("App Updated on 2017-09-04, version 3.2")
          )
        )
      )
    )
)


