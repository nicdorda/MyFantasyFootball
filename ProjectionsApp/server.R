library(shiny)
library(Rglpk)
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
                  selected = which(names(allAnalysts) == "Average"), width = '300px') 
    } else {
      selectInput("viewProjExprt", "Analyst", allAnalysts[allAnalysts %in% siteAnalysts$projAnalystId[siteAnalysts$weekProj]], 
                  selected = which(names(allAnalysts) == "Average"), width = '300px')
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
   
   
   data <- merge(x=data, y=riskValues, by = "playerId")
   
   data$conf.Int <- paste(as.character(data$minPts), as.character(data$maxPts), sep = " - ")

   posName <- names(selectPos)[as.numeric(input$ptsPos)]
   pName <- paste("'", posName, "'", sep = "")
   
   data <- merge(x= dbPlayers[,c("playerNfl", "playerName", "playerTeam", "playerPos")], y=data, by.x = "playerNfl", by.y = "playerId")
   
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
 
 output$dfsLineupVal<- renderDataTable({
   if(is.null(input$salaryFile))
     return(NULL)
   data <- optimalLineup()[optimalLineup()$type =="Value",  c("posCode", "Player", "Salary", "projPts", "H.Value")]
   total <- data.frame(posCode = NA, Player = "Total", Salary = sum(data$Salary), projPts = sum(data$projPts), H.Value = sum(data$H.Value))
   data <- rbind(data,total)
   names(data) <- c("Position", "Player", "Salary", "Points", "H.Value")
   data$H.Value <- round(data$H.Value,2)
   data
 }, options = list(paging = FALSE, searching = FALSE))
 
 output$dfsLineupPts<- renderDataTable({
   if(is.null(input$salaryFile))
     return(NULL)
   data <- optimalLineup()[optimalLineup()$type =="Points",  c("posCode", "Player", "Salary", "projPts", "H.Value")]
   total <- data.frame(posCode = NA, Player = "Total", Salary = sum(data$Salary), projPts = sum(data$projPts), H.Value = sum(data$H.Value))
   data <- rbind(data,total)
   names(data) <- c("Position", "Player", "Salary", "Points", "H.Value")
   data$H.Value <- round(data$H.Value,2)
   data
 }, options = list(paging = FALSE, searching = FALSE))
optimalLineup <- reactive({
  
  salFile <- input$salaryFile
  
  if(is.null(salFile))
    return(NULL)
  
  sal <- read.csv(salFile$datapath, stringsAsFactors = FALSE )
  sal$Player <- getPlayerName(sal$Player)
  
  scrapeQry <- paste("SELECT dataScrapeId FROM dataScrapes WHERE weekNo =", as.character(input$dfsWeekNo), "AND seasonYear =", as.character(input$dfsSeason))
  scrapeId <- ffSqlQry(scrapeQry)$dataScrapeId
  
  sal$Pos[sal$Pos == "D"] <- "DEF"
  sal <- merge(x = sal, y= dbPlayers[, c("playerName", "playerNfl")], by.x = "Player", by.y = "playerName", all.x = TRUE)
  
  sal$Salary <- as.numeric(gsub("\\$|,", "", sal$Salary))
  
  ptsQry <- paste("SELECT playerId, projPts FROM projPts where dataScrapeId = ", scrapeId, "AND projAnalystId = 17 AND leagueCode = '" , input$dfslLeague, "'", sep="" )
  
  projPts <- ffSqlQry(ptsQry)
  
  sal <- merge(x=sal, y=projPts, by.x = "playerNfl", by.y ="playerId", all.x = TRUE)
  sal$projPts[is.na(sal$projPts)] <- 0
  
  sal$H.Value <- (sal$projPts^sqrt(3)) / sal$Salary * 2000
  sal <- merge(x = playerPositions[, c("posId", "posCode")], y=sal, by.x = "posCode", by.y = "Pos")
  sal <- sal[with(sal, order(posId,-projPts)),]
  
  QBPos <- as.numeric(sal$posCode=="QB")
  RBPos <- as.numeric(sal$posCode=="RB")
  WRPos <- as.numeric(sal$posCode=="WR")
  TEPos <- as.numeric(sal$posCode=="TE")
  PKPos <- as.numeric(sal$posCode=="K")
  TDPos <- as.numeric(sal$posCode=="DEF")
  Salary <- sal$Salary
  Points <- sal$projPts
  HValue <- sal$H.Value
  Lineup <- c(rep(0, length(Salary)))
  
  mat_data <- cbind(QBPos, RBPos, WRPos, TEPos, PKPos, TDPos, Salary)
  
  dmatrix <- t(matrix(mat_data, ncol=7))
  
  rhs <- c(1,2,3,1,1,1,60000)
  dir <- c(rep("==",6), "<=")
  max <- TRUE
  types <- c(rep("I", length(Salary)))
  
  bounds <- list(lower = list(ind = 1:length(Salary), val = rep(0, length(Salary))),
                 upper = list(ind = 1:length(Salary), val = rep(1, length(Salary))))
  
  result <- Rglpk_solve_LP(HValue, dmatrix, dir, rhs, bounds, types, max)
  
  val_players <- sal[as.logical(result$solution), c("posCode", "Player", "Salary", "projPts", "H.Value")]
  val_players$type = "Value"
  
  
  result <- Rglpk_solve_LP(Points, dmatrix, dir, rhs, bounds, types, max)
  
  pts_players <- sal[as.logical(result$solution), c("posCode", "Player", "Salary", "projPts", "H.Value")]
  pts_players$type <- "Points"
  resultdf <- rbind(val_players,pts_players)
  
  return(resultdf)
 })
})