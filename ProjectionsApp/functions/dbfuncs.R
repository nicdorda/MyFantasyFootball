## File:	dbFunctions.R
## Date:	2/12/2015
## Author:	Dennis Andersen [andersen.dennis@live.com]
## -------------------------------------------------------
## Notes:
## This script contains functions related to data operations against a SQL Server
## using the RODBC package:
## - appdendSqlTable uses the sqlSave function to append a data frame to an existing table
## - getColNames reads the column names from an existing table
## - writeProj2DB is used to write Fantasy Football projections to the database
## - writePts2DB is used to write projected points to the database
## - readProjFromDB is used to read the projections from the database for a specific scrape
## - addAgg2DB adds the aggregated values to the database
## - writeRanks2DB add the ECR ranks to the databse for a scrape
###########################################################################################
library(RODBC)
## This is the name of the ODBC data source that is configured on the system
ODBC_SOURCE <- "FantasyDB"

appendSqlTable <- function(tblName, dataFrame, faster = TRUE){
	ffdb <- odbcConnect(ODBC_SOURCE)
	sqlSave(ffdb, dataFrame, tblName, rownames = FALSE, append = TRUE, fast = faster)
	odbcClose(ffdb)
}

updateSqlTable <- function(tblName, dataFrame, tblKey = NULL){
  ffdb <- odbcConnect(ODBC_SOURCE)
  sqlUpdate(ffdb, dataFrame, tblName, index = tblKey)
  odbcClose(ffdb)
}

getColNames <- function(tblName){
	ffdb <- odbcConnect(ODBC_SOURCE )
	colNames <- sqlColumns(ffdb, tblName)$COLUMN_NAME
	odbcClose(ffdb)
	return(colNames)
}

ffSqlQry <- function(qryString){
  ffdb <- odbcConnect(ODBC_SOURCE)
  qryResult <- sqlQuery(ffdb, qryString, stringsAsFactors = FALSE)
  odbcClose(ffdb)
  return(qryResult)
}

writeProj2DB <- function(weekNo, season, projData, dataAnalysts){
  
	# Find the scrapeId for the weekNo and season provided
	qryString <- paste("SELECT dataScrapeId FROM dataScrapes WHERE weekNo =", as.character(weekNo), 
				"AND seasonYear =", as.character(season))
	
	scrapeId <- ffSqlQry(qryString)$dataScrapeId
	scrapeId <- ifelse(length(scrapeId) > 0, scrapeId, 0)
  
	# Update data on the current data scrape and delete related data
  if(scrapeId != 0)
  {
    updScrape <- ffSqlQry(paste("Select * from dataScrapes where dataScrapeId = ", as.character(scrapeId)))
    updScrape$scrapeDate <- Sys.time()
    updateSqlTable("dataScrapes", updScrape, "dataScrapeId")
    
    for(posName in names(projData)){
      tblName <- paste(tolower(posName), "Projections", sep ="")
      delString <- paste("DELETE FROM", tblName, "WHERE dataScrapeId =", as.character(scrapeId), 
                         "AND projAnalystId in (", paste(c(dataAnalysts, 17:19), collapse = ","), ")")
      ffSqlQry(delString)
    }	    
  } else {
    # Insert a new scrape record and retrieve the new scrape Id
    newScrape <- data.frame(weekNo = weekNo, seasonYear = season, scrapeDate = Sys.time())
    appendSqlTable("addScrapes", newScrape)
    
    scrapeId <- ffSqlQry(qryString)$dataScrapeId
    
  }


	# Add projections to respective tables
	for(posName in names(projData)){
		tblName <- paste("add", posName, "Proj", sep ="")
		colNames <- getColNames(tblName)
	
    sqlData <- projData[[posName]]

    if(nrow(sqlData) >0){
      sqlData$dataScrapeId <- scrapeId
  		if(!("nobs" %in% names(sqlData))){
        sqlData$nobs <- NA
  		}
      appendSqlTable(tblName, sqlData[, colNames])
    }
		rm(sqlData)
		incProgress(1/24)
	}		
}

writePts2DB <- function(weekNo, season, ptsData){
	
	# Find the scrapeId for the weekNo and season provided
	qryString <- paste("SELECT dataScrapeId FROM dataScrapes WHERE weekNo =", as.character(weekNo), 
				"AND seasonYear =", as.character(season))
	
	scrapeId <- ffSqlQry(qryString)$dataScrapeId
	scrapeId <- ifelse(length(scrapeId) > 0, scrapeId, 0)

	# Delete the current points in the database if they exists
	delString <- paste("DELETE FROM projPts WHERE dataScrapeId =", as.character(scrapeId))
		ffSqlQry(delString)

	# Insert the new data
	for(posName in names(ptsData)){
		tblName <- "addProjPts"
		colNames <- getColNames(tblName)
		
		sqlData <- ptsData[[posName]]
    
		if(nrow(sqlData) >0){
	    sqlData$dataScrapeId <- scrapeId
      appendSqlTable(tblName, sqlData[!is.na(as.numeric(sqlData$projPts)) , colNames], fast = FALSE)
		}
  	rm(sqlData)
    incProgress(1/6)
	}	
}

readProjFromDB <- function(pos, weekNo, season){
  posCode <- playerPositions$posCode[playerPositions$posId == pos]
  # Find the scrapeId for the weekNo and season provided
  qryString <- paste("SELECT dataScrapeId FROM dataScrapes WHERE weekNo =", as.character(weekNo), 
                     "AND seasonYear =", as.character(season))
  
  scrapeId <- ffSqlQry(qryString)$dataScrapeId
  
  projTable <- paste(tolower(posCode), "Projections", sep="")
  colNames <- getColNames(projTable)
  
  colNames <- colNames[-which(colNames %in% c("projId", "dataScrapeId"))]
  posData <- ffSqlQry(paste("SELECT", paste(colNames, collapse =", "), "FROM", projTable, "WHERE dataScrapeId =", as.character(scrapeId) ))
  incProgress(1/24)
  return(posData)
}

addAgg2DB<- function(pos, fromDataList, weekNo, season){
  # Find the scrapeId for the weekNo and season provided
  qryString <- paste("SELECT dataScrapeId FROM dataScrapes WHERE weekNo =", as.character(weekNo), 
                     "AND seasonYear =", as.character(season))
  
  scrapeId <- ffSqlQry(qryString)$dataScrapeId
  scrapeId <- ifelse(length(scrapeId) > 0, scrapeId, 0)
  
  
  posCode <- playerPositions$posCode[playerPositions$posId == pos]
  
  for(nme in names(fromDataList[[posCode]])){
    tblName <- paste("add", posCode, "Proj", sep ="")
    colNames <- getColNames(tblName)
    
    sqlData <- as.data.frame(fromDataList[[posCode]][[nme]])
    
    if(nrow(sqlData) >0 ){
      sqlData$dataScrapeId <- scrapeId
            for(cName in colNames[-(which(colNames %in% names(sqlData)))]){
        sqlData[cName] <- NA
      }
      appendSqlTable(tblName, sqlData[, colNames])
    }
    rm(sqlData)
    
  }		
  incProgress(1/24)
}

writeRanks2DB <- function(ranksData, weekNo, season){
  # Find the scrapeId for the weekNo and season provided
  qryString <- paste("SELECT dataScrapeId FROM dataScrapes WHERE weekNo =", as.character(weekNo), 
                     "AND seasonYear =", as.character(season))
  
  scrapeId <- ffSqlQry(qryString)$dataScrapeId
  scrapeId <- ifelse(length(scrapeId) > 0, scrapeId, 0)
  colNames <- getColNames("addEcrRank")
  
  delString <- paste("DELETE FROM ecrRank WHERE dataScrapeId =", as.character(scrapeId))
  
  ffSqlQry(delString)
  
  
  for(lType in names(ranksData)){
    for(p in names(ranksData[[lType]])){
      if(nrow(ranksData[[lType]][[p]]) > 0){
        sqlData <- ranksData[[lType]][[p]]
        sqlData$dataScrapeId <- as.numeric(scrapeId)
        sqlData$leagueType <- lType
        
        appendSqlTable("addEcrRank", sqlData[, colNames], fast = FALSE)
        incProgress(1/24)
      }
    }
  }
}