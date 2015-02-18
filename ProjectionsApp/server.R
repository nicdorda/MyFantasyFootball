library(shiny)
leagueSelect <- leagueSelect
shinyServer(function(input, output, session) {
  ## Reading the functions used to read and write to database
  source(paste(getwd(), "/functions/dbfuncs.R", sep=""))
  
  ## Reactive variable that will fetch the id of the data scrape from the database.
  scrapeId <- reactive({
    scrapeQry <- paste("SELECT dataScrapeId FROM dataScrapes WHERE weekNo =", as.character(input$weekNo), "AND seasonYear =", as.character(input$season))
    sid <- ffSqlQry(scrapeQry)$dataScrapeId
    updTable <- reactVal$updTable
    ifelse(length(sid) >0, sid, 0)
  })
  
  ## Reactive values for the app
  reactVal <- reactiveValues(updTable = FALSE, updLeague = FALSE)
  
  
  output$selectAnalysts <- renderUI({
    sid <- scrapeId()
    sqlQry <- paste("SELECT distinct projAnalystId FROM (",
          "SELECT projAnalystId, dataScrapeId FROM qbProjections UNION SELECT projAnalystId, dataScrapeId FROM rbProjections UNION",
          "SELECT projAnalystId, dataScrapeId FROM wrProjections UNION SELECT projAnalystId, dataScrapeId FROM teProjections UNION",
          "SELECT projAnalystId, dataScrapeId FROM kProjections UNION SELECT projAnalystId, dataScrapeId FROM defProjections) a",
          "WHERE dataScrapeId =", as.character(sid))
      
    selectedAnalysts <- ffSqlQry(sqlQry)$projAnalystId
    
    if(input$weekNo == 0){
      
      checkboxGroupInput("siteAnalysts", "Analysts:", 
                         selectAnalysts[selectAnalysts %in% siteAnalysts$projAnalystId[siteAnalysts$seasonProj]], 
                         selected = selectedAnalysts, inline = FALSE)
    } else{
      checkboxGroupInput("siteAnalysts", "Analysts:", 
                         selectAnalysts[selectAnalysts %in% siteAnalysts$projAnalystId[siteAnalysts$weekProj]],
                         selected = selectedAnalysts, inline = FALSE)
    }
  })
  
  output$viewExpert <- renderUI({
    if(input$weekNo == 0){
      selectInput("viewProjExprt", "Analyst", allAnalysts[allAnalysts %in% siteAnalysts$projAnalystId[siteAnalysts$seasonProj]], 
                  selected = which(names(allAnalysts) == "Aggregate"), width = '300px') 
    } else {
      selectInput("viewProjExprt", "Analyst", allAnalysts[allAnalysts %in% siteAnalysts$projAnalystId[siteAnalysts$weekProj]], 
                  selected = which(names(allAnalysts) == "Aggregate"), width = '300px')
    }
    
  })
  
  ## Below is the rendering of the User Name and password inputs for Footballguys.com
  output$fbgUser <- renderUI({
     if(length(input$siteAnalysts) >0){
     if(any(input$siteAnalysts %in% footballGuys)){
     textInput("fbgUser", "FBG Login:")
    }}
   })
  output$fbgPW <- renderUI({
   if(length(input$siteAnalysts) >0){    
     if(any(input$siteAnalysts %in% footballGuys)){
       passwordInput("fbgPW", "FBG Password")
     }}
 })
 
 #### This is where the data retrieval is called.
  observe({
    if (input$getProj == 0){
        return()
    }else{
        isolate({
            weekNo <- input$weekNo
            season <- input$season
            fbgUserName <- input$fbgUser
            fbgPassWord <- input$fbgPW

            projAnalysts <- projSiteInfo[which(projSiteInfo$projAnalystId %in% as.numeric(input$siteAnalysts)), ]
            
            rawProj <- suppressWarnings(apply(projAnalysts, 1, fetchData, weekNo, season, fbgUserName, fbgPassWord))
            
            posList <- as.list(selectPos)
            
            print("Combining data")
            allProj <- suppressWarnings(lapply(posList, combProj, rawProj))
            
            
            withProgress(message = "Aggregation", detail = "Will take while ...", value = 0, {
              print("Writing projections to DB")
              
              writeProj2DB(weekNo, season, allProj, input$siteAnalysts)
              print("Getting all projections for the week")
              allProj <- lapply(posList, readProjFromDB, weekNo, season)
              
            
              print("Calculating median, average and variance")
              projStats <- suppressWarnings(lapply(posList, aggrProj, allProj))
              
              print("Writing aggregates to DB")
              lapply(posList, addAgg2DB, projStats, weekNo, season)
              allProj <- lapply(posList, readProjFromDB, weekNo, season)
              rm(rawProj, projStats)
              
                
                
            })
            
            withProgress(message= "Calculations", detail = " ... Patience", value = 0, {
              projPts <- lapply(posList, calcPts, allProj)
              leagPts <- lapply(posList, unpivotPts, projPts)
            })
            
            ## Add data to database
            withProgress(message = "Writing to DB", value = 0, {
              writePts2DB(weekNo, season, leagPts)
            })
            
            withProgress(message = "Getting ECR Ranks", value = 0, {
              rnkPosList <- list(qb = "QB", rb = "RB", wr = "WR", te = "TE", pk = "K", def = "DST")
              allRanks <- lapply(lgTypeList, function(lg)lapply(rnkPosList, function(pL)getRanks(pL, lg, weekNo)))
              
              writeRanks2DB(allRanks, weekNo, season)
            })
        })}
    
    reactVal$updTable <- TRUE
    
    })
 
 
  ### Generate the data table with the stat projections.
  output$statProj <- renderDataTable({
    sid <- scrapeId()
    plPos <- playerPositions$posCode[playerPositions$posId == input$projPos]
    removeCols <- c("playerNfl", "dataScrapeId", "projAnalystId", "nobs", "projId", "retTD") #, "paAtt", "paCmp", "ruAtt")

    dbTbl <- paste(tolower(plPos), "Projections", sep="")
    
    dataQry <- paste("SELECT * FROM", dbTbl ,"WHERE dataScrapeId =", as.character(sid), "AND projAnalystId =", 
                     as.character(input$viewProjExprt)) 
    
    data <- ffSqlQry(dataQry)
    
    data <- as.data.frame(lapply(data, function(x)ifelse(!is.nan(x) & is.numeric(x), round(x,1),x)))
    data <- merge(x= dbPlayers[,c("playerNfl", "playerName", "playerTeam")], y=data, by.x = "playerNfl", by.y = "playerId")
    data <- data[,-which(names(data) %in% removeCols)]
    names(data)[1] <- "Player Name"
    names(data)[2] <- "Team"
    
    data$Player <- paste(data$"Player Name", data$Team, sep =", ")
    data["Player Name"] <- NULL
    data$Team <- NULL
    
    data <- data[,c("Player", names(data)[which(names(data) != "Player")])]
    data <- switch(plPos, 
                   "QB" = data[with(data, order(-paYd)),],
                   "RB" = data[with(data, order(-ruYd)),],
                   "WR" = data[with(data, order(-reYd)), ],
                   "TE" = data[with(data, order(-reYd)), ],
                   "K" = data[with(data, order(-FGM)), ],
                   "DEF" = data[with(data, order(ptsAllow)), ]
      )
    
    updTable <- isolate({reactVal$updTable})
    if(updTable)
      reactVal$updTable <- FALSE
  data
  } 
  )
  
 output$ptsProjTbl <- renderDataTable({
   scrapeQry <- paste("SELECT dataScrapeId FROM dataScrapes WHERE weekNo =", as.character(input$weekNoPts), "AND seasonYear =", as.character(input$seasonPts))
   sid <- ffSqlQry(scrapeQry)$dataScrapeId
   lgType <- ffLeagues[ffLeagues$leagueCode == input$leagueProj, "leagueType"]
   
   lgCode = paste("'", input$leagueProj, "'", sep="")
   ptsQry <- paste("SELECT playerId, projPts, minPts, maxPts, cvPts from projLeaguePts where leagueCode =", lgCode,
                   "AND dataScrapeId =", as.character(sid))
   
   data <- ffSqlQry(ptsQry)
   
   riskQry <- paste("SELECT playerId, riskVal from riskValues where leagueCode =", lgCode,
                   "AND dataScrapeId =", as.character(sid))
   riskValues <- ffSqlQry(riskQry)
   print(nrow(riskValues))
   
   data <- merge(x=data, y=riskValues, by = "playerId")
   
   data$conf.Int <- paste(as.character(data$minPts), as.character(data$maxPts), sep = " - ")

   posName <- names(selectPos)[as.numeric(input$ptsPos)]
   pName <- paste("'", posName, "'", sep = "")
   
   data <- merge(x= dbPlayers[,c("playerNfl", "playerName", "playerTeam", "playerPos")], y=data, by.x = "playerNfl", by.y = "playerId")
   print(input$leagueProj)
   if(!as.logical(ffLeagues[ffLeagues$leagueCode == input$leagueProj, "dfsLeague"])){
     vorQry <- paste("SELECT vorPts, flxVor, posCode from leagueVorBaseline WHERE dataScrapeId = ", as.character(sid), "AND leagueCode =", lgCode)
     vorPts <- ffSqlQry(vorQry)
     
     data <- merge(x =data, y= vorPts, by.x ="playerPos", by.y ="posCode")
     
     data$VBD <- data$projPts - data$vorPts
     data$flxVBD <- data$projPts - data$flxVor
     data$vorPts <- NULL
     data$flxVor <- NULL
   }
   if(input$ptsPos !=0){
    
    data$Player <- paste(data$playerName, data$playerTeam, sep =", ")
    lgType <- paste("'", ifelse(as.numeric(input$ptsPos) %in% c(2, 3, 4), lgType , "std"), "'", sep="")
    rankQry <- paste("SELECT playerId, ecrRank from ecrRank where leagueType =", lgType,
                      "AND dataScrapeId =", as.character(sid), "AND posId =", as.character(input$ptsPos))
    ecrRanks <- ffSqlQry(rankQry)
    
    data <- merge(x=data, y=ecrRanks, by.x = "playerNfl", by.y ="playerId", all.x = TRUE)
    
    if(input$ptsPos %in% c(1,5,6)){
      data$flxVBD <- NULL
    }
    
    data <- data[data$playerPos == posName, ]
    data <- data[with(data, order(-projPts, maxPts)), ]
    data$Rank <- max(rank(data$projPts, ties.method="first")) - rank(data$projPts, ties.method="first") +1
    
    
    tryCatch(data$tier <- 10 - Mclust(data$projPts, G=9)$classification, error=function(e)print("Error Finding tier"))
    if(exists("tier", data)){
    tierDf <- data.frame(tier = unique(data$tier), tierRank = rank(unique(data$tier)))
    
    data <- merge(x = data, y= tierDf, by = "tier")
    data$tier <- paste("Tier", data$tierRank)
    data$tierRank <-  NULL}
    data <- data[,c("Player", names(data)[which(names(data) != "Player")])]
    
   } else {
     print("all Pos")
     data <- data[with(data, order(-projPts, maxPts)), ]
     data$Rank <- max(rank(data$projPts, ties.method="first")) - rank(data$projPts, ties.method="first") +1
     data$Player <- paste(data$playerName, data$playerPos, data$playerTeam, sep =", ")
     data <- data[,c("Player", names(data)[which(names(data) != "Player")])]
   }
   
   data$playerName <- NULL
   data$playerTeam <- NULL
   data$playerNfl <- NULL
   data$playerPos <- NULL
   data$minPts <- NULL
   data$maxPts <- NULL
   data[with(data, order(Rank)), ]
   
 })
 
 source(paste(getwd(),"/scripts/leagueSetup.R", sep =""), local = TRUE)
 
})