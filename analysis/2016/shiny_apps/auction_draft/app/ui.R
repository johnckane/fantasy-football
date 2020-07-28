library(shiny)
library(gridExtra)

shinyUI(fluidPage(
            titlePanel("Bad Newz Realtime Draft Optimizer"),            
            # Create a new Row in the UI for selectInputs
            fluidRow(
                column(2, 
                       numericInput("cash", label = h3("Cash remaining"), value = 300)),
                column(2, 
                       numericInput("qb", label = h3("QBs to draft"), value = 2)),
                column(2, 
                       numericInput("rb", label = h3("RBs to draft"), value = 2)),
                column(2,
                       numericInput("wr", label = h3("WRs to draft"), value = 3)),
                column(2,
                       numericInput('te',label = h3("TEs to draft"), value = 1)),
                column(2,
                       numericInput('dst',label = h3("DST to draft"), value = 1)),
                column(2,numericInput('k',label = h3("K to draft"), value = 1))
        
            ),
            #fluidRow(
            #    column(4,
            #           selectizeInput(
            #               'drafted',
            #               'Drafted Players',
            #               choices = full_data2$player,
            #               multiple = TRUE
            #           )),
            #    column(4,
            #           selectizeInput(
            #               'my_team',
            #               'Players I Drafted',
            #               choices = full_data2$player,
            #               multiple = TRUE
            #           ))
            #),
                
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
                             choices = full_data2$player,
                             multiple = FALSE#,
#                             selected = NULL
                           ))
                         ),
                         verbatimTextOutput('auction_block_value')),
                tabPanel('Optimal Model', dataTableOutput('table_solution_model')),
               # tabPanel('My Team',dataTableOutput('table_my_team')),
                tabPanel('All Players', dataTableOutput('table_all_players')),
                tabPanel('Unavailable Players',
                         selectizeInput(
                                  'drafted',
                                  'Drafted Players',
                                  choices = full_data2$player,
                                  multiple = TRUE
                                )))
            )
            # ,
            # fluidRow(
            #       h3("Estimated PPG for current roster + optimized roster:"),
            #       textOutput('model_ppg'),
            #       h3("Estimated PPG for current roster + optimized roster (with error)"),
            #       textOutput('error_ppg')
            #     )
            )
)
