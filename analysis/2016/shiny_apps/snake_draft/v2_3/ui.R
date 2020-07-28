library(shiny)


df <- read.csv("/home/john/stats_corner/2016/shiny_apps/snake_draft/v2_1/faa_projection_data.csv",
#df <- read.csv("/srv/shiny-server/stats-corner/2016/snake-assistant/faa_projection_data.csv",
               stringsAsFactors = FALSE,
               header = TRUE)


shinyUI(fixedPage(
	titlePanel("Snake Draft Assistant 2.3"),
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
		                 selectInput("scoring_format", label = h6("Scoring Format"), choices = c("Standard","PPR"), selected = "Standard"),
		                 selectInput("extra_pos", label = h6("Additional Positions"), choices = c("FLEX","OP"), selected = "FLEX")
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
		        tabPanel("Team Parameters",
		                 h4("Enter the # of Players in Each Position of your Starting Lineup"),
		                 numericInput("num_qb",   label = "# QB",   value = 1),
		                 numericInput("num_rb",   label = "# RB",   value = 2),
		                 numericInput("num_wr",   label = "# WR",   value = 3),
		                 numericInput("num_te",   label = "# TE",   value = 1),
		                 numericInput("num_flex", label = "# FLEX", value = 1),
		                 numericInput("num_op",   label = "# OP",   value = 0),
		                 numericInput("num_k",    label = "# K",    value = 1),
		                 numericInput("num_dst",  label = "# DST",  value = 1)
		        ),
		        tabPanel("Recomendations",
		                 h4("PPG and Dropoffs of Best Available (BA) Now and Next Time (BANT)"),
		                 checkboxGroupInput("pos_to_rec", label = h4("Positions to Recommend"), 
		                                    choices = list("QB" = "QB", "RB" = "RB", "WR" = "WR", "TE" = "TE", "K" = "K", 
		                                                   "DST" = "DST", "FLEX" = "FLEX", "OP" = "OP"),
		                                    selected = c("QB","RB","WR","TE","K","DST","FLEX","OP"),
		                                    inline = T),
		                 checkboxGroupInput("byes_to_filter", label = h4("BYE weeks to exclude:"),
		                                    choices = list("3" = 3, "4" = 4, "5" = 5, "6" = 6, "7" = 7,"8" = 8, "9" = 9,
		                                                   "10" = 10, "11" = 11, "12" = 12, "13" = 13),
		                                    selected = c(),
		                                    inline = T),
		                 radioButtons("one_or_two", label = h4("Recommend Based on One Pick From Now or Two?"),
		                              choices = list("One" = 1, "Two" = 2), 
		                              selected = 1,
		                              inline = T),
		                 dataTableOutput("rec_table")
		        ),
        		tabPanel("Available Players",
        		         dataTableOutput("available_players")
        		),
		        tabPanel("Lineup Optimizer",
		                 p("How to Use:"),
		                 p("1. Enter players you've drafted on the left"),
		                 p("2. Observe how your weekly point totals change based on players drafted and their bye weeks"),
		                 p("3. Example Use Case: compare how weekly totals change depending on players you're considering drafting"),
		                 p("4. This was built with intention of using this feature later in the draft when filling out your bench"),
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
						         p("Data last updated on 9/4"),
						         p("Questions? Email me: StatsCorner@gmail.com"),
						         p(""),
						         p("App Updated on 2016-12-17, version 2.3"),
						         p("What's new in version 2.3 (vs 2.0)?"),
						         p("1. Data update"),
						         p("2. BYE week filters"),
						         p("3. Position filters"),
						         p("4. OP and FLEX recommendations"),
						         p("5. Lineup Optimizer"),
						         p("6. A 'Value Added' variable in recommendations, based on current roster and BYE Weeks")
						)
	      )
			)
		)
  )
)
