## File: fetchData.R
## Date: 2/14/2015
## Author: Dennis Andersen [andersen.dennis@live.com]
## -------------------------------------------------------------------------------------
## Notes:
## This function is uses in apply on a table that contation the data for a data scrape.
## The data that is in the data row is has been read from the database and included the 
## url and parameters to contstruct the url
## Calls readData to actually read the HTML table
## The reference tables used are read from the database at the startup of the app from
## the global.R file
#########################################################################################

fetchData <- function(dataRow, weekNo, season, fbgUser, fbgPW){
  # This is the list of columns that will be removed as they are not used in further calculations down the stretch
  removeCols <- c("FPTS", "Status", "Opp", "Team", "DefRank", "ovrRank", "posRank", "nmfCI", "blkPu", "blkFG", "blkPAT", 
                  "puRtTD", "kiRtTD", "ydsAllow", "stFum", "pctOwned", "projRank", "actRank", "Rank", "paTgt", "fumTD", 
                  "FGMi", "XPMi", "paCmpPct", "paYAtt", "ruAvg", "reAvg", "fumFor", paste("X", 1:50, sep=""), 
                  paste("V", 1:50, sep=""))
  
  # Creating the sequence of page for the position aht tis currently being handled in the dataRow
  pgSeq <- seq(from = as.numeric(dataRow["startPge"]), to = as.numeric(dataRow["endPge"]), by = as.numeric(dataRow["stepPge"]))
  
  # Finding the position Id from the database used to identify the positions
  posId <- playerPositions$posId[playerPositions$posCode == dataRow["posCode"]]
  
  # Retrieving columnames and typed from the database.
  colNames <- sitePosTableCols$colName[sitePosTableCols$posId == posId & sitePosTableCols$dataSiteId == as.numeric(dataRow["projDataSiteId"]) & sitePosTableCols$colPeriod == ifelse(weekNo == 0, "Season", "Week")]
  colTypes <- sitePosTableCols$colType[sitePosTableCols$posId == posId & sitePosTableCols$dataSiteId == as.numeric(dataRow["projDataSiteId"]) & sitePosTableCols$colPeriod == ifelse(weekNo == 0, "Season", "Week")]
  
  # Retrieving the index of the column containing the playner name for each site
  nameCol <- ifelse(weekNo == 0, projDataSites$nameColSeason[projDataSites$projDataSiteId == as.numeric(dataRow["projDataSiteId"])],
                    projDataSites$nameColWeek[projDataSites$projDataSiteId == as.numeric(dataRow["projDataSiteId"])])
  
  # Reading the relevant URL with parameter place holders
  siteUrl <- ifelse(weekNo == 0, dataRow["seasonUrl"], dataRow["weekUrl"])

  # Initializing a data frame to be used  
  projData <- data.frame()
  
  # Finding the name of the Projection Analyst for the progress message
  srcName = siteAnalysts$analystName[siteAnalysts$projAnalystId == as.numeric(dataRow["projAnalystId"])]
  posName = as.character(dataRow["posCode"])
  
  # Utilizing progress to identify the progress of the data as data retrieval will take time with multiple sources
  withProgress( message = paste("Data from", srcName), detail = posName, value = 0, {
  
  if(length(colNames)>1){
    for(pg in pgSeq){
      
      # Replacing parameters in the url with actual values
      tmpUrl <- siteUrl 
      tmpUrl <- gsub("{$WeekNo}", as.character(weekNo), tmpUrl, fixed = TRUE)
      tmpUrl <- gsub("{$PgeID}", as.character(pg), tmpUrl, fixed = TRUE)
      tmpUrl <- gsub("{$Season}", as.character(season), tmpUrl, fixed = TRUE)
      
      # There may be rows needed to be removed. Information is read from database reference table
      rows2Remove <- siteRowsRemove$rowRemove[siteRowsRemove$sitePosId == as.numeric(dataRow["sitePosId"])]
      
      # Now read the data
      siteData <- readData(tmpUrl, removeRow = rows2Remove, nameCol = nameCol, colTypes = colTypes, fbgUser, fbgPW)
      
      # Making sure we have data columns to start working on data
      if(length(siteData) > 1){
        # Setting columns
        names(siteData)[1:length(colNames)] <- c(colNames)
        
        # Split the Pass Attempted/Completed column for sites that have those
        if(exists("cmpAtt", siteData))
        {
          siteData[["cmpAtt"]][siteData[["cmpAtt"]] == "0"] <- "0/0"
          siteData[["cmpAtt"]][siteData[["cmpAtt"]] == "--"] <- "0/0"
          siteData[["cmpAtt"]][siteData[["cmpATt"]] == "--/--"] <- "0/0"
          cmpAttElem <- unlist(strsplit(siteData[["cmpAtt"]], "/", fixed = TRUE))
          cmpAttRows <- length(cmpAttElem) / 2
          
          cmpAtt <- matrix(cmpAttElem, nrow = cmpAttRows, byrow = TRUE)
          siteData["paAtt"] <- as.numeric(cmpAtt[,2])
          siteData["paCmp"] <- as.numeric(cmpAtt[,1])
          
          siteData["cmpAtt"] <- NULL
        }
        
        # Combining different blocked kicks into the one fields
        if(exists("blkPu", siteData)){
          siteData["blkKick"] <- siteData["blkFG"] + siteData["blkPu"] + siteData["blkPAT"] 
        }
        
        # Putting punt and kick return TDs under return TDs
        if(exists("puRtTD", siteData)){
          siteData["retTD"] <- siteData["puRtTD"] + siteData["kiRtTD"]
        }
        
        # Adding the analyst Id to the data to identify source
        siteData$projAnalystId <- dataRow["projAnalystId"]
        
        # Removing columns that are not needed
        if(length(which(names(siteData) %in% removeCols))>0){
          siteData <- siteData[, -which(names(siteData) %in% removeCols)]
        }
        
        # Finding the name of the site that the analyst is associated with
        siteName <- projDataSites$projSiteName[projDataSites$projDataSiteId == as.numeric(dataRow["projDataSiteId"])]
        
        # The idCol is the name of the column in the dbPlayers table that indicates the player Id for that player.
        # That column will in turn map to the player'd Id at NFL.com
        idCol <- switch(siteName,
                        "Yahoo" = "yahooId",
                        "Footballguys" = "fbgId",
                        "CBS" = "cbsId",
                        "FOX" = "foxId",
                        "FFToday" = "fftId",
                        "ESPN" = "None",
                        "FantasyPros" = "None",
                        "NumberFire" = "None",
                        "NFL" = "nflId"
                        )
        
        # Merging with player data from NFL.com to find the playerId.
        if(dataRow["projDataSiteId"] != nflSiteId){
          if(idCol %in% names(dbPlayers) & dataRow["posCode"] != "DEF" & "playerId" %in% names(siteData) ){
            siteData <- merge(x=siteData, y= dbPlayers[,c("playerNfl",  idCol)], by.x="playerId", by.y = idCol)
          }else{
            siteData <- merge(x=siteData, y= dbPlayers[dbPlayers$playerPos == posName, c("playerNfl", "playerName")], by.x="Player", by.y = "playerName")
          }
          
          # Removing names as extra player Id column
          siteData$playerId <- siteData$playerNfl
          siteData$playerNfl <- NULL
          if(exists("Player", siteData)){
            siteData$Player <- NULL
          } 
        }
      projData <- rbind.fill(projData, siteData)
      incProgress(1/length(pgSeq))
      }
    }
  }
  })
  return(projData)
}

