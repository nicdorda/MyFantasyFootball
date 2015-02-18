
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
                            ),
                   tabPanel("Rankings"
                   ),
                   tabPanel("League Setup",
                            fluidPage(fluidRow(
                              column(3, uiOutput("viewLeague")),
                              column(3, 
                                     inputPanel(
                                      uiOutput("leagueName"),
                                     uiOutput("leagueCode"),
                                     uiOutput("dfsLeague"),
                                     uiOutput("leagueType"),
                                     splitLayout(cellWidths = c("75%","75%"), 
                                        actionButton("saveLeague", "Save League"),
                                        uiOutput("editSavePoints"))),
                                     inputPanel(
                                     uiOutput("ptsForm"))
                                     ),
                              column(6, tabsetPanel(
                                        tabPanel("Yardage points", dataTableOutput("leagueYdPts"), value = "yds"),
                                        tabPanel("Multiplier points", dataTableOutput("leagueMultPts"), value = "mult")
                                        , id="scoring")
                                     ))
                              )
                   )
                   #,
                   #navbarMenu("DFS Leagues",
                  #            tabPanel("Victiv"  
                  #            ),
                  #            tabPanel("FanDuel"
                  #                     )
                  #            )
                   
                   )
)
