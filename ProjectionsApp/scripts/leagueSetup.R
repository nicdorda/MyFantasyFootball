

leagueVal <- reactiveValues(updLeague = FALSE, ptsEdit =FALSE)

dbLeagues <- reactive({
  updLeague <- leagueVal$updLeague
  ffSqlQry("SELECT * FROM ffLeagues")
})


leagueData <- reactive({
  
  selectedLeague <- ffSqlQry(paste("SELECT * FROM ffLeagues WHERE leagueCode ='", input$selectLeague, "'", sep =""))

  return(selectedLeague)
})

### Generating the output for the League form

## Select input to find the league
## Taking a dependency on the reactive value updLeague to contol updating the in input
output$viewLeague <- renderUI ({
  updLeague <- leagueVal$updLeague

  leagueSelect <- dbLeagues()$leagueCode
  names(leagueSelect) <- dbLeagues()$leagueName

  if(updLeague)
    leagueVal$updLeague <- FALSE
  
  selectInput("selectLeague", "Select league:", c(leagueSelect, "New League" = "new"), selected = leagueSelect[1])
})

## Textbox for the league name
output$leagueName <- renderUI({
  if(nrow(leagueData())>0){
    textInput("leagueName", "League Name:", leagueData()$leagueName)
  }
  else {
    textInput("leagueName", "League Name:")
  }
})

## Textbox for the league code 
output$leagueCode <- renderUI({
  if(nrow(leagueData())>0){
    textInput("leagueCode", "League Code:", leagueData()$leagueCode)
  }
  else {
    textInput("leagueCode", "League Code:")
  }
})

## Checkbox to identify if it is a DFS league
output$dfsLeague <- renderUI({
  if(nrow(leagueData())>0){
    checkboxInput("dfsLeague", "DFS League", leagueData()$dfsLeague)
  }
  else {
    checkboxInput("dfsLeague", "DFS League", FALSE)
  }
})

## Select input to identify the league type
output$leagueType <- renderUI({
  if(nrow(leagueData())>0){
    selectInput("leagueType", "League Type", lgTypeList, selected = lgTypeList[which(names(lgTypeList) == leagueData()$leagueType)])
  }
  else {
    selectInput("leagueType", "League Type", lgTypeList, selected = lgTypeList[1])
  }
})

## The list of yardage points
output$leagueYdPts <- renderDataTable({
  data <- ydScoring[ydScoring$leagueId == leagueData()$leagueId, -which(names(ydScoring) %in% c("leagueId", "ydScoringId"))]
  data <- merge(x=playerPositions[,c("posId", "posCode")], y=data, by = "posId")
  data$posId <- NULL
  head(data)},
  options = list(paging = FALSE, searching = FALSE)
)

## The list of multiplier points
output$leagueMultPts <- renderDataTable({
  data <- multScoring[multScoring$leagueId == leagueData()$leagueId, -which(names(multScoring) %in% c("leagueId", "multplId"))]
  
  data <- merge(x=playerPositions[,c("posId", "posCode")], y=data, by = "posId", all.y = TRUE)
  print(nrow(data))
  data$posId <- NULL
  data}
  ,
  options = list(paging = FALSE, searching = FALSE))

## The 'Edit Points' button will only show for existing leagues
output$editSavePoints <- renderUI({
  if(nrow(leagueData())>0) {
    btnLabel = ifelse(leagueVal$ptsEdit, "Save Pts", "Edit Pts")
    btnId = ifelse(leagueVal$ptsEdit, "savePts", "editPts")
  }
  
  actionButton(btnId, btnLabel)
  
})

output$ptsForm <- renderUI({
  if(leagueVal$ptsEdit){
    switch(input$scoring,
           "yds" = fluidRow(
             column(6,selectInput("ptsPos", "Position",selectPos)),
             column(6,selectInput("ptsType", "Type", c("Total" = "total", "Rush" = "rush", "Pass" = "pass")))
             ),
           "mult" = fluidRow(
                  column(6,selectInput("multPos", "Position",selectPos, selected=1)),
                  column(6,uiOutput("multCol"))
                  
                  )
    )
  }
})
output$multCol <- renderUI({
  print(names(selectPos)[1])
  dataCols <- getColNames(paste(tolower(names(selectPos)[as.numeric(input$multPos)]), "Projections", sep=""))
  dataCols <- dataCols[-which(dataCols %in% c("projId", "playerId", "projAnalystId", "dataScrapeId", "nobs"))]
  dataCols <- dataCols[-grep("Yd", dataCols)]
  selectInput("multCol", "Field", dataCols)
})
### Observers to handle button clicks

## Saving and updating league
observe({
  if (input$saveLeague == 0){
    return()
  }
  else {
    isolate({
      if(nrow(leagueData())>0){
        lgId <- leagueData()$leagueId
        sqlString <- sprintf("UPDATE ffLeagues SET leagueName = '%s', leagueCode = '%s', dfsLeague = %d, leagueType = '%s' where leagueId = %d",
                             input$leagueName, input$leagueCode, as.numeric(input$dfsLeague), names(lgTypeList)[which(lgTypeList == input$leagueType)], lgId)
      }
      else {
        
        insertVals <- c(paste("'",c(input$leagueName, input$leagueCode, names(lgTypeList)[which(lgTypeList == input$leagueType)]), "'", sep =""), 
                        as.numeric(input$dfsLeague))
        sqlString <- paste("INSERT INTO ffLeagues(leagueName, leagueCode, leagueType, dfsLeague) VALUES(", paste(insertVals, collapse = ", "), ")")
      }
      print(paste("SQL:", sqlString))
      ffSqlQry(sqlString)
      leagueVal$updLeague <- TRUE
    })
  }
})

observe({
  if(length(input$editPts) == 0){
    return()
  }
  if(input$editPts == 0){
    return()
  }
  else {
    
    isolate({
      print("Edit Points")
      leagueVal$ptsEdit <- TRUE
      
      })
  }  
})

observe({
  if(length(input$savePts) == 0){
    return()
  }
  if(input$savePts == 0){
    return()
  }
  else {
    
    isolate({
      print("Save points")
      leagueVal$ptsEdit <- FALSE
      
    })
  }  
})