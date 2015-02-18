saveProj2DB <- function(projData, weekNo, season){
  dataScrape <- data.frame("weekNo" = weekNo, "seasonYear" = season, "scrapeDate" = Sys.time())

  ffdb <- odbcConnect("FantasyDB")
  dbAnalysts <- sqlFetch(ffdb, "projAnalysts")
  dbScrapes <- sqlFetch(ffdb, "dataScrapes")

  # Delete current data
  scrapeId <- dbScrapes[dbScrapes$weekNo == weekNo & dbScrapes$seasonYear == season, "dataScrapeId"]  
  if(length(scrapeId)>0){
    sqlQuery(ffdb, paste("DELETE FROM dataScrapes where dataScrapeId =", as.character(scrapeId)))
    sqlQuery(ffdb, paste("DELETE FROM qbProjections where dataScrapeId =", as.character(scrapeId)))
    sqlQuery(ffdb, paste("DELETE FROM rbProjections where dataScrapeId =", as.character(scrapeId)))
    sqlQuery(ffdb, paste("DELETE FROM wrProjections where dataScrapeId =", as.character(scrapeId)))
    sqlQuery(ffdb, paste("DELETE FROM teProjections where dataScrapeId =", as.character(scrapeId)))
    sqlQuery(ffdb, paste("DELETE FROM kProjections where dataScrapeId =", as.character(scrapeId)))
    sqlQuery(ffdb, paste("DELETE FROM defProjections where dataScrapeId =", as.character(scrapeId)))
  }
  
  # Create new scrape record in the db table
  sqlSave(ffdb, dataScrape, tablename = "addScrapes", append = TRUE, rownames = FALSE, nastring = "NULL")
  dbScrapes <- sqlFetch(ffdb, "dataScrapes")
  scrapeId <- dbScrapes[dbScrapes$weekNo == weekNo & dbScrapes$seasonYear == season, "dataScrapeId"]
  
  for(nme in names(projData)){ 
    # These data frames ensure same column structure as the db tables
    qbProj <- data.frame(playerId = as.numeric(), paAtt = as.numeric(), paCmp = as.numeric(), paYd = as.numeric(), paTD = as.numeric(), paInt = as.numeric(), 
                         ruAtt = as.numeric(), ruYd= as.numeric(), ruTD = as.numeric(), fumLost = as.numeric(), twoPts = as.numeric(), retTD = as.numeric(), 
                         nobs = as.numeric())
    rbProj <- data.frame(playerId = as.numeric() , ruAtt = as.numeric(), ruYd = as.numeric(), ruTD = as.numeric(), paRec = as.numeric(), 
                         reYd = as.numeric(), reTD = as.numeric(), fumLost = as.numeric(), twoPts = as.numeric(), retTD = as.numeric(), nobs = as.numeric())
    wrProj <- data.frame(playerId = as.numeric(), paRec = as.numeric(), reYd = as.numeric(), reTD = as.numeric(), ruAtt = as.numeric(), 
                         ruYd = as.numeric(), ruTD = as.numeric(), fumLost = as.numeric(), twoPts = as.numeric(), retTD = as.numeric(), nobs = as.numeric())
    teProj <- data.frame(playerId = as.numeric(), paRec = as.numeric(), reYd = as.numeric(), reTD = as.numeric(), ruAtt = as.numeric(), 
                         ruYd = as.numeric(), ruTD = as.numeric(), fumLost = as.numeric(), twoPts = as.numeric(), retTD = as.numeric(), nobs = as.numeric())
    kProj <- data.frame(playerId = as.numeric(), FGA = as.numeric(), FGM = as.numeric(), FG0019 = as.numeric(), FG2029 = as.numeric(), FG3039 = as.numeric(),
                        FG4049 = as.numeric(), FG5099 = as.numeric(), XPM = as.numeric(), nobs = as.numeric())
    defProj <- data.frame(playerId = as.numeric(), defInt = as.numeric(), defSacks = as.numeric(), fumRec = as.numeric(), defSafety = as.numeric(), defTD = as.numeric(),
                         blkKick = as.numeric(), retTD = as.numeric(), ptsAllow = as.numeric(), nobs = as.numeric())
    
    # Changing the name of the player Id column for the aggregate values
    if(nme %in% c("agg", "avg", "var")){
      names(projData[[nme]]$qb)[1]  <- "playerId"
      names(projData[[nme]]$rb)[1]  <- "playerId"
      names(projData[[nme]]$wr)[1]  <- "playerId"
      names(projData[[nme]]$te)[1]  <- "playerId"
      names(projData[[nme]]$k)[1]   <- "playerId"
      names(projData[[nme]]$def)[1] <- "playerId"
    }
  
    # Removing columns that are not in the data frames
    qbCols <- which(names(projData[[nme]]$qb) %in% names(qbProj))
    rbCols <- which(names(projData[[nme]]$rb) %in% names(rbProj))
    wrCols <- which(names(projData[[nme]]$wr) %in% names(wrProj))
    teCols <- which(names(projData[[nme]]$te) %in% names(teProj))
    kCols <- which(names(projData[[nme]]$k) %in% names(kProj))
    defCols <- which(names(projData[[nme]]$def) %in% names(defProj))
  
    # Inserting data in the data frames
    qbProj <- rbind.fill(qbProj, projData[[nme]]$qb[,qbCols])
    rbProj <- rbind.fill(rbProj, projData[[nme]]$rb[,rbCols])
    wrProj <- rbind.fill(wrProj, projData[[nme]]$wr[,wrCols])
    teProj <- rbind.fill(teProj, projData[[nme]]$te[,teCols])
    kProj <- rbind.fill(kProj, projData[[nme]]$k[,kCols])
    defProj <- rbind.fill(defProj, projData[[nme]]$def[,defCols])
  
    # Getting the analyst Id from the database
    projAnalystId <- dbAnalysts[dbAnalysts$analystId == nme, "projAnalystId"]
  
    # Insert new data
    if(nrow(qbProj)>0){
      qbProj$scrapeId <- scrapeId
      qbProj$projAnalystId <- projAnalystId
      names(qbProj) <- c("playerId", "passAtt", "passCmp", "passYds", "passTD", "passInt", "rushAtt", "rushYds", 
                          "rushTD", "fumbLost", "twoPts", "rtrnTD", "nobs", "dataScrapeId", "projAnalystId")
      sqlSave(ffdb, qbProj, tablename = "addQBProj", append = TRUE, rownames = FALSE)
    }
 
    if(nrow(rbProj)>0 ){
      rbProj$scrapeId <- scrapeId
      rbProj$projAnalystId <- projAnalystId
      names(rbProj) <- c("playerId", "rushAtt", "rushYds", "rushTD", "passRec", "recYds", "recTD", "fumbLost", 
                          "twoPts", "rtrnTD", "nobs",  "dataScrapeId", "projAnalystId")
      sqlSave(ffdb, rbProj, tablename = "addRBProj", append = TRUE, rownames = FALSE)
    }
 
    if(nrow(wrProj)>0){
      wrProj$scrapeId <- scrapeId
      wrProj$projAnalystId <- projAnalystId
      names(wrProj) <- c("playerId", "passRec", "recYds", "recTD", "rushAtt", "rushYds", "rushTD", "fumbLost", 
                          "twoPts", "rtrnTD", "nobs",  "dataScrapeId", "projAnalystId")
      sqlSave(ffdb, wrProj, tablename = "addWRProj", append = TRUE, rownames = FALSE)
    }
 
    if(nrow(teProj)>0){
      teProj$scrapeId <- scrapeId
      teProj$projAnalystId <- projAnalystId
      names(teProj) <- c("playerId", "passRec", "recYds", "recTD", "rushAtt", "rushYds", "rushTD", "fumbLost", 
                          "twoPts", "rtrnTD", "nobs", "dataScrapeId", "projAnalystId")
      sqlSave(ffdb, teProj, tablename = "addTEProj", append = TRUE, rownames = FALSE)
    }
    if(nrow(kProj)>0){
      kProj$dataScrapeId <- scrapeId
      kProj$projAnalystId <- projAnalystId
      sqlSave(ffdb, kProj, tablename = "addKProj", append = TRUE, rownames = FALSE)
    }
 
    if(nrow(defProj)>0){
      defProj$dataScrapeId <- scrapeId
      defProj$projAnalystId <- projAnalystId
      sqlSave(ffdb, defProj, tablename = "addDefProj", append = TRUE, rownames = FALSE)
    }
  }

  odbcClose(ffdb)
}