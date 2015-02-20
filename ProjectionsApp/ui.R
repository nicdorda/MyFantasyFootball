
shinyUI(navbarPage(title="Fantasy Football Projections", 
                   tabPanel("Stat projections",
                            fluidPage(
                              
                              fluidRow(
                              column(2,
                                     fluidRow(column(6, selectInput("season", "Season:", seasonSelect, selected = selectedYr, selectize = TRUE) ),
                                              column(6, selectInput("weekNo", "Week:", weekSelect, selected = selectedWk, selectize = TRUE))),              
                              wellPanel(uiOutput("selectAnalysts"),
                              uiOutput("fbgUser"),
                              uiOutput("fbgPW"),
                              actionButton("getProj", "Get Projections")
                              )),
                              column(10,
                                     fluidRow(
                                       column(3, uiOutput("viewExpert")),
                                       column(9, selectInput("projPos", "Position", selectPos, width = "100px"))
                                     ),
                            fluidRow(
                              dataTableOutput("statProj")
                            ))))
                            ),
                   tabPanel("Point Projections",
                            fluidPage(fluidRow(
                              column(2,
                                     fluidRow(column(6, selectInput("seasonPts", "Season:", seasonSelect, selected = selectedYr, selectize = TRUE) ),
                                              column(6, selectInput("weekNoPts", "Week:", weekSelect, selected = selectedWk, selectize = TRUE))),
                                    fluidRow(column(12, selectInput("ptsPos", "Position", c(selectPos, "All" = 0), width = "100px"))),
                                    fluidRow(column(12, 
                                                    selectInput("leagueProj", "League", leagueSelect)))
                              ),
                              column(10, dataTableOutput("ptsProjTbl")
                                   )
                            )
          
                            )
                            )
                   ,
                   tabPanel("Daily Leagues",
                             fluidPage(
                               fluidRow(column(2, 
                                                selectInput("dfslLeague", "Select League", leagueSelect[which(unlist(leagueSelect) %in% ffLeagues$leagueCode[ffLeagues$dfsLeague == TRUE])]),
                                                selectInput("dfsSeason", "Season:", seasonSelect),
                                                selectInput("dfsWeekNo", "Week:", weekSelect) ,
                                                fileInput("salaryFile", "Upload Salaries", accept=c('text/csv', 
                                                                                                    'text/comma-separated-values,text/plain', 
                                                                                                    '.csv')) #,
                                               #actionButton("runOptimal", "Optimize lineup")
                                              ),
                                        column(5, tags$h4("Optimal Value Lineup"), dataTableOutput("dfsLineupVal")),
                                        column(5, tags$h4("Optimal Points Lineup"), dataTableOutput("dfsLineupPts"))
                               )
                              )
                   )
                   
                  # ,
                  # tabPanel("DFS Leagues",
                  #            tabPanel("Victiv"  
                  #            ),
                  #            tabPanel("FanDuel"
                  #                     )
                              )
                   
                   #)
)
