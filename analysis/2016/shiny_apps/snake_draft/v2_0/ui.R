library(shiny)


df <- read.csv("/home/john/stats_corner/2016/shiny_apps/snake_draft/v2_0/faa_projection_data.csv",
               stringsAsFactors = FALSE,
               header = TRUE)


shinyUI(fixedPage(
	titlePanel("Snake Draft Assistant 2.0"),
	fixedRow(
		column(3,wellPanel(
			numericInput("picks_made",label = h4("Picks Made"), value = 0)
		  )),
		column(3,wellPanel(
		       h4("Your Next Picks are:"),
		       textOutput("next_pick"),
		       textOutput("next_pick1"),
		       textOutput("next_pick2")
		       )),
		column(6,wellPanel(
		       h4("Your Recommended Positions to Draft Are:"),
		       textOutput("pos_recs")
		       ))
	),
	fixedRow(
	  column(4,wellPanel(
	         h4("Drafted Players"),
	         selectizeInput("drafted_players", label = "Enter Players as they get Drafted", multiple = TRUE, choices = df$player_team)
	          
	  )),
		column(8,
		  tabsetPanel(
		        tabPanel("Quick Start",
		                 p("1. Enter your draft parameters in the next tab once and once only."),
                     p("2. As all players, not just yours, get drafted enter their names into the 'Drafted Players' window."),
		                 p("3. Everytime you make a selection update the 'Picks Made' counter in the upper left")
		        ),
		        tabPanel("Draft Parameters",
		                 numericInput("first_pick",  label = h6("Round 1 Pick #"), value = 1),
		                 numericInput("league_teams",  label = h6("How Many Teams in League?"), value = 10),
		                 selectInput("scoring_format", label = h6("Scoring Format"), choices = c("Standard","PPR"), selected = "Standard")#,
		                 #h5("Staring Lineup"),
		                 #numericInput("starting_qbs", label = h6("QBs"), value = 1),
		                 #numericInput("starting_rbs", label = h6("RBs"), value = 2),
		                 #numericInput("starting_wrs", label = h6("WRs"), value = 2),
		                 #numericInput("starting_tes", label = h6("TEs"), value = 1),
		                 #numericInput("starting_flex", label = h6("Flex: Non-QB"), value = 1),
		                 #numericInput("starting_op", label = h6("OP: Including QB"), value = 0),
		                 #numericInput("starting_dst", label = h6("D/ST"), value = 1),
		                 #numericInput("starting_k", label = h6("K"), value = 1)
		        ),
		        tabPanel("Reccomendations",
		                 h3("PPG and Dropoffs of Best Available (BA) Now, Next Time You Pick, and Two Picks From Now"),
		                 dataTableOutput("rec_table")
		        ),
        		tabPanel("Available Players",
        		         dataTableOutput("available_players")
        		),
		        tabPanel("Lineup Optimizer",
		                 column(6,
		                        p("Column 1")
		                        ),
		                 column(6,
		                        p("Column 2"))),
						tabPanel("About",
						         a("Projection and ADP data downloaded from Fantasy Football Analytics",     
						           href="http://fantasyfootballanalytics.net/"),
						         p("Data lasted updated on 8/15"),
						         p("Questions? Email me: StatsCorner@gmail.com")
						)
	      )
			)
		)
  )
)
