## File: getEcrRanks.R
## Date: 02/16/2015
## Author: Dennis Andersen [andersen.dennis@live.com]
## --------------------------------------------------------------
## Note:
## The getRanks reads Expert Consensus rankings by position. Most importantly the sdRank from this scrape is used
## as part of the calculations of risk Values for players.
#################################################################################################################

getRanks <- function(posName, leagueType, weekNo){
  ecrRank <- data.frame( playerId = as.numeric(), posId = as.character(), ecrRank = as.numeric(), bestRank = as.numeric(), worstRank = as.numeric(),
                        avgRank = as.numeric(), sdRank = as.numeric())
  url_base <- "http://www.fantasypros.com/nfl/rankings"
  if(weekNo != 0){
  url_path <- paste("/", ifelse(leagueType != "std", paste(tolower(leagueType), "-", sep = ""),""), tolower(posName), ".php", sep = "") 
  } else {
    url_path <- paste("/", ifelse(leagueType != "std", paste(tolower(leagueType), "-", sep = ""),""), tolower(posName), "-cheatsheets.php", sep = "") 
  }
  rnks <-readHTMLTable(paste(url_base, url_path, sep = ""), stringsAsFactors = FALSE)$data
    
    if((posName %in% c("QB", "K", "DST") & leagueType == "std") | posName %in% c("RB", "WR", "TE")){
      names(rnks) <- c("ecrRank", "Player", "bestRank", "worstRank", "avgRank", "sdRank")
      rnks <- rnks[grep("[0-9]$", rnks$ecrRank), ]
      rnks$Player <- getPlayerName(rnks$Player)
      if(posName == "DST"){
        for(r in 1:nrow(teamCorrect))
        {
          rnks$Player[rnks$Player == paste(teamCorrect[r,]$TeamArea, teamCorrect[r,]$TeamName)] <- teamCorrect[r,]$TeamName
        }
      }
      posName <- ifelse(posName == "DST", "DEF", posName)
      posId <- playerPositions[playerPositions$posCode == posName, "posId"]
      rnks$posId = posId
      rnks <- rnks[, which(names(rnks) %in% c("Player", names(ecrRank)))]
      
      rnks <- merge(x=rnks, y=dbPlayers[,c("playerNfl", "playerName")], by.x= "Player", by.y = "playerName")
      
      rnks$playerId <- rnks$playerNfl
      rnks$playerNfl <- NULL
      rnks$Player <- NULL
      rnks<-as.data.frame(apply(rnks, 2, function(x)as.numeric(x)))
      incProgress(1/24)
      ecrRank <- rbind.fill(ecrRank, rnks)
      ecrRank <- ecrRank[with(ecrRank, order(ecrRank)),]
    }
  return(ecrRank)
}


