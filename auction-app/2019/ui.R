library(shiny)
library(gridExtra)

shinyUI(fluidPage(
            titlePanel("Bad Newz Realtime Draft Optimizer"),            
            # Create a new Row in the UI for selectInputs
            fluidRow(
                column(2, 
                       numericInput("cash", label = h3("Cash remaining"), value = 300)),
                column(2, 
                       numericInput("qb", label = h3("QBs to draft"), value = 3)),
                column(2, 
                       numericInput("rb", label = h3("RBs to draft"), value = 3)),
                column(2,
                       numericInput("wr", label = h3("WRs to draft"), value = 4)),
                column(2,
                       numericInput('te',label = h3("TEs to draft"), value = 2)),
                column(2,
                       numericInput('dst',label = h3("DST to draft"), value = 2)),
                column(2,numericInput('k',label = h3("K to draft"), value = 1)),
                column(2,numericInput('max_byes',label=h3("Max BYEs"),value = 2))
        
            ),
            # Create a new row for the table.
            fluidRow(
                tabsetPanel(
                id = 'dataset',
                tabPanel('Optimal Value',
                         fluidRow(
                           column(12,
                           selectizeInput(
                             'candidate',
                             'Player on Block',
                             choices = data$player,
                             multiple = FALSE#,
#                             selected = NULL
                           ))
                         ),
                         verbatimTextOutput('auction_block_value')),
                tabPanel('Optimal Model', dataTableOutput('table_solution_model')),
                tabPanel("Your Team",
                column(6,
                  h5("My Team"),
                  selectizeInput("my_team", label = "Enter Players YOU Drafted", multiple = TRUE, choices = data$player)
                  ),
                column(6,
                  h5("Weekly Expected Points From Starting Lineup"),
                  dataTableOutput("my_lineup_optimized"))
                ),          
                tabPanel('All Players', dataTableOutput('table_all_players')),
                tabPanel('Unavailable Players',
                         selectizeInput(
                                  'drafted',
                                  'Drafted Players',
                                  choices = data$player,
                                  multiple = TRUE
                                )))
            )
))
