## File: myData.R
## Date: 02/16/2015
## Author: Dennis Andersen [andersen.dennis@live.com]
## -----------------------------------------------------------------------------------------------------------------------------
## Note:
## Fetching data from the SQL Server database to initialize the Projections application
#################################################################################################################################
ffdb <- odbcConnect("FantasyDB")
dbPlayers <- sqlQuery(ffdb, "Select playerId as playerNfl, playerName, playerPos, playerTeam, cbsId, yahooId, foxId, fbgId, fftId from Players where playerStatus <> 'HIS'", 
                      stringsAsFactors = FALSE)
weekList          <- sqlQuery(ffdb, "SELECT * FROM weekLookup", stringsAsFactors = FALSE)
seasonList        <- sqlQuery(ffdb, "SELECT * FROM seasonYears", stringsAsFactors = FALSE)
siteAnalysts      <- sqlQuery(ffdb, "Select pa.*, ps.projSiteName FROM projAnalysts pa left JOIN projDataSites ps on ps.projDataSiteId = pa.projDataSiteId", stringsAsFactors = FALSE)
playerPositions   <- sqlQuery(ffdb, "Select * from playerPositions", stringsAsFactors = FALSE)
dataScrapes       <- sqlQuery(ffdb, "Select * from dataScrapes", stringsAsFactors = FALSE)
projDataSites     <- sqlQuery(ffdb, "Select * from projDataSites", stringsAsFactors = FALSE)
projSiteInfo      <- sqlQuery(ffdb, "SELECT * FROM projSiteInfo", stringsAsFactors = FALSE)
sitePosTableCols  <- sqlQuery(ffdb, "SELECT * FROM sitePosTableCols", stringsAsFactors = FALSE)
siteRowsRemove    <- sqlQuery(ffdb, "SELECT * FROM siteRowsRemove", stringsAsFactors = FALSE)
bonusYds          <- sqlQuery(ffdb, "SELECT * from bonusYds", stringsAsFactors = FALSE)
ffLeagues         <- sqlQuery(ffdb, "Select * from ffLeagues", stringsAsFactors = FALSE)
multScoring       <- sqlQuery(ffdb, "Select * from multScoring", stringsAsFactors = FALSE)
ydScoring         <- sqlQuery(ffdb, "Select * from ydScoring", stringsAsFactors = FALSE)
leagueTypes       <- sqlQuery(ffdb, "Select * from leagueTypes", stringsAsFactors = FALSE)
nameCorrect       <- sqlQuery(ffdb, "Select * from NameCorrections", stringsAsFactors = FALSE)
teamCorrect       <- sqlQuery(ffdb, "Select * from NFLTeams", stringsAsFactors = FALSE)
odbcClose(ffdb)

rownames(projSiteInfo) <- paste(projSiteInfo$posCode, as.character(projSiteInfo$projAnalystId), sep="_")
## Defining the content for the Analyst input selections
## First setting the logical values for the analysts with weekly and/or season projections
siteAnalysts$weekProj <- as.logical(siteAnalysts$weekProj)
siteAnalysts$seasonProj <- as.logical(siteAnalysts$seasonProj)

## multAnalysts will contain names of project sites with multiple analysts
multAnalysts <- count(siteAnalysts, "projSiteName")$projSiteName[count(siteAnalysts, "projSiteName")$freq >1]

footballGuys <- siteAnalysts$projAnalystId[siteAnalysts$projSiteName == "Footballguys"]
## Creating the list of analyst ids and assigning names. If the analyst is from a site with multiple analysts
## then the name will be suffixed with the site name
allAnalysts <- siteAnalysts$projAnalystId
names(allAnalysts) <- paste(siteAnalysts$analystName, 
                            ifelse(siteAnalysts$projSiteName %in% multAnalysts, 
                                   paste(",", siteAnalysts$projSiteName),
                                   ""), sep ="")

## Removing the "analyst" associated with the calculated values 
selectAnalysts <- allAnalysts[allAnalysts %in% siteAnalysts$projAnalystId[!(siteAnalysts$analystName %in% c("Aggregate", "Average", "Variance"))]]

## Finding the latest data scrape
latestScrape = dataScrapes[which.max(dataScrapes$scrapeDate), ]


#### Setting the values for the select boxes for week and season.
weekSelect <- weekList$weekId
names(weekSelect) <- weekList$weekName
seasonSelect <- seasonList$seasonYear
selectedWk <- ifelse(is.null(dim(latestScrape)), 0, latestScrape$weekNo)
selectedYr <- ifelse(is.null(dim(latestScrape)), min(seasonList$seasonYear), latestScrape$seasonYear)

selectPos <- playerPositions$posId
names(selectPos) <- playerPositions$posCode

nflSiteId <- projDataSites$projDataSiteId[projDataSites$projSiteName == "NFL"]
varId <- siteAnalysts$projAnalystId[siteAnalysts$analystName == "Variance"]

leagueList <- as.list(ffLeagues$leagueId)
names(leagueList) <- ffLeagues$leagueCode

leagueSelect <- as.list(ffLeagues$leagueCode)
names(leagueSelect) <- ffLeagues$leagueName

lgTypeList <- as.list(leagueTypes$leagueTypeName)
names(lgTypeList) <- leagueTypes$leagueType
