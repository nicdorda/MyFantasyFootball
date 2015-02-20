##==================================================================================================
## File: getNames.R
## Author: Dennis Andersen [andersen.dennis@live.com]
## Date: 1/19/2015
## -------------------------------------------------------------------------------------------------
## Note:
## This script contains two functions: getPlayerName and readData
## getPlayerName takes a player name column from one of the online projection sites: CBS, ESPN, FOX, 
## NFL, FFToday, and Football guys and strips the informaation down to the player name only.
##
## readData takes a url from one of the sites mentioned above and returns the projections from the 
## page with player information as stripped by getPlayerName, but also adds a player id for the site
## based on the player links on the page.
##===================================================================================================
library(XML)
library(httr)

getPlayerName <- function(playerCol){
  playerCol <- gsub("49ers", "Niners", playerCol, fixed = TRUE)
  playerCol <- gsub("New York NYG", "Giants", playerCol, fixed = TRUE)
  playerCol <- gsub("New York NYJ", "Jets", playerCol, fixed = TRUE)
  playerCol <- gsub("New York.+\\(NYG", "Giants", playerCol)
  playerCol <- gsub("New York.+\\(NYJ", "Jets", playerCol)
  playerCol <- gsub("New York Giants", "Giants", playerCol)
  playerCol <- gsub("New York Jets", "Jets", playerCol)
  playerCol <- gsub("New England Patriots", "Patriots", playerCol)
  playerCol <- gsub("New England", "Patriots", playerCol)
  playerCol <- gsub("New Orleans Saints", "Saints", playerCol)
  playerCol <- gsub("New Orleans", "Saints", playerCol)
  
  playerCol <- gsub("Questionable|Probable|Injured Reserve|Out|SSPD|Final|View|Videos|News|Video|(N|n)ote|(N|n)otes|(P|p)layer|^No new|New |\\s+((P|Q|O|D|S)$|IR|EXE|SUS|PUP|DNP|LP)|\\s(P|Q|O|D|S)\\s|^\\[(Q|P|O|D|S)\\]\\s|(P|Q|O|D|S|IR)$", "", playerCol)
  playerCol <- gsub(" Jr.| Sr.| Jr| Sr| III", "", playerCol)
  playerCol <- gsub("(B(AL|al)|B(UF|uf)|C(HI|hi)|C(IN|in)|C(LE|le)|D(AL|al)|D(EN|en)|D(ET|et)|GB|H(OU|ou)|I(ND|nd)|J(AC|ac)|J(AX|ax)|KC|K(AN|an)|NO|O(AK|ak)|P(IT|it)|P(HI|hi)|NYG|NYJ|NE|S(EA|ea)|A(TL|tl)|A(RI|ri)|M(IA|ia)|SD|S(T|t)(L|l)|C(AR|ar)|SF|T(EN|en)|W(AS|as)|TB|M(IN|in)|W(SH|sh)) ", "", playerCol)
  playerCol <- gsub(",\\s(B(AL|al)|B(UF|uf)|C(HI|hi)|C(IN|in)|C(LE|le)|D(AL|al)|D(EN|en)|D(ET|et)|GB|H(OU|ou)|I(ND|nd)|J(AC|ac)|J(AX|ax)|KC|K(AN|an)|NO|O(AK|ak)|P(IT|it)|P(HI|hi)|NYG|NYJ|NE|S(EA|ea)|A(TL|tl)|A(RI|ri)|M(IA|ia)|SD|S(T|t)(L|l)|C(AR|ar)|SF|T(EN|en)|W(AS|as)|TB|M(IN|in)|W(SH|sh))", "", playerCol)
  playerCol <- gsub("(B(AL|al)|B(UF|uf)|C(HI|hi)|C(IN|in)|C(LE|le)|D(AL|al)|D(EN|en)|D(ET|et)|GB|H(OU|ou)|I(ND|nd)|J(AC|ac)|J(AX|ax)|KC|K(AN|an)|NO|O(AK|ak)|P(IT|it)|P(HI|hi)|NYG|NYJ|NE|S(EA|ea)|A(TL|tl)|A(RI|ri)|M(IA|ia)|SD|S(T|t)(L|l)|C(AR|ar)|SF|T(EN|en)|W(AS|as)|TB|M(IN|in)|W(SH|sh))$", "", playerCol)
  playerCol <- gsub("BAL|BUF|CHI|CIN|CLE|DAL|DEN|DET|GB|HOU|IND|JAC|JAX|KC|KAN|NO|OAK|PIT|PHI|NYG|NYJ|NE|SEA|ATL|ARI|MIA|SD|STL|CAR|SF|TEN|WAS|TB|MIN|WSH", "", playerCol)
  
  playerCol <- gsub("[^a-zA-Z \\.\\-]", "", playerCol)
  playerCol <- gsub("Niners", "49ers", playerCol, fixed = TRUE)
  playerCol <- gsub(" {2,99}", "", playerCol)
  playerCol <- gsub("vs$", "", playerCol)
  playerCol <- gsub("(W|L)$", "", playerCol)
  
  playerCol <- gsub("RBTE$|RBWR$|TERB$|WRRB$|WRTE$|TEWR$|QBRB$|RBQB$|QBWR$|WRQB$|TEQB$|QBTE$|QB$|RB$|WR$|TE$|K$|DEF$|DST$|FA$| FA|DST D", "", playerCol)
  playerCol <- gsub("^\\s+|\\s$", "", playerCol)
  
  playerCol <- gsub("\\-$", "", playerCol)
  playerCol <- gsub(" - DEF(W|L)$", "", playerCol)
  for(n in 1:nrow(nameCorrect)){
    playerCol[playerCol == nameCorrect[n,]$NameFrom] <- nameCorrect[n,]$NameTo
  }
  
  for(n in 1:nrow(teamCorrect)){
    playerCol[playerCol == teamCorrect[n,]$TeamArea] <- teamCorrect[n,]$TeamName
    playerCol[playerCol == paste(teamCorrect[n,]$TeamArea, teamCorrect[n,]$TeamName)] <- teamCorrect[n,]$TeamName
  }
  return(playerCol)
}

readData <- function(inpUrl, removeRow = 0, nameCol = 1, colTypes = NA, fbgUserName, fbgPassword){

  dataSrc <- projDataSites$projSiteName[grep(parseURI(inpUrl)$server, projDataSites$weekUrl, fixed=TRUE)]
  
  if(dataSrc == "Footballguys"){

    dataPge <- POST(
      handle = handle("http://subscribers.footballguys.com"),
      path = "amember/login.php",
      body = list(amember_login = fbgUserName,
                  amember_pass = fbgPassword,
                  amember_redirect_url = inpUrl)
    )
    
  }
  
  
  dataTable <- tryCatch(
                switch(dataSrc,
                      "CBS" = readHTMLTable(inpUrl, stringsAsFactors = FALSE, skip.rows = removeRow, colClasses = colTypes)[7]$`NULL`,
                      "FOX" = readHTMLTable(inpUrl, stringsAsFactors = FALSE, colClasses = colTypes)$playerTable,
                      "ESPN" = readHTMLTable(inpUrl, stringsAsFactors = FALSE, skip.rows = removeRow, colClasses = colTypes)$playertable_0,
                      "NFL" = readHTMLTable(inpUrl, stringsAsFactors = FALSE, colClasses = colTypes)$`NULL`,
                      "FFToday" = readHTMLTable(inpUrl, stringsAsFactors = FALSE, skip.rows = removeRow, colClasses = colTypes)[11]$`NULL`,
                      "Footballguys" = readHTMLTable(content(dataPge), stringsAsFactors = FALSE, colClasses = colTypes)$`NULL`,
                      "Yahoo" = readHTMLTable(inpUrl, stringsAsFactors = FALSE, header = FALSE, colClasses = colTypes)[2]$`NULL`,
                      "NumberFire" = readHTMLTable(inpUrl, stringsAsFactors = FALSE, header = FALSE, colClasses = colTypes)$`complete-projection`,
                      "FantasyPros" = readHTMLTable(inpUrl, stringsAsFactors = FALSE, colClasses = colTypes)$data
                        ),
        error = function(e)NULL
        )
  
  if(is.null(nrow(dataTable))){
    return()
  }
  names(dataTable)[nameCol] <- "Player"
  if(dataSrc == "CBS"){
    if(length(grep("Pages:", dataTable$Player, fixed = TRUE))>0){
      dataTable <- dataTable[-grep("Pages:", dataTable$Player, fixed = TRUE),]
    }
    
  }
  
  if(dataSrc == "Footballguys"){
    pgeLinks <-  tryCatch(
                    getHTMLLinks(content(dataPge)),
                    error = function(e)NULL)
  }else{
    pgeLinks <- tryCatch(
                    getHTMLLinks(inpUrl),
                    error = function(e)NULL)
  }
  
  playerId <- switch(dataSrc,
                     "CBS" = unique(gsub("[^0-9]", "", 
                                         pgeLinks[grep("/fantasyfootball/players/playerpage/[0-9]{3,6}", pgeLinks)]
                     )),
                     "FOX" = unique(gsub("[^0-9]","",
                                         pgeLinks[grep("/fantasy/football/commissioner/Players/Profile.aspx", pgeLinks)]
                     )),
                     "NFL" = unique(gsub("[^0-9]","",
                                         pgeLinks[grep("playerId=[0-9]{3,7}$", pgeLinks)]
                     )),
                     "FFToday" = unique(gsub("[^0-9]","",
                                             gsub("LeagueID=[0-9]{1,6}", "", 
                                                  pgeLinks[grep("/stats/players/", pgeLinks)]
                                             )
                     )),
                     "Yahoo" = unique(gsub("[^0-9]","",
                                           pgeLinks[grep("http://sports.yahoo.com/nfl/players/[0-9]{3,6}", pgeLinks)]
                     )),
                     "Footballguys" = gsub("../players/player-all-info.php?id=","",pgeLinks[grep("player-all-info.php?", pgeLinks)], fixed = TRUE)
  )
  
  
  if(length(playerId) > 0 & length(dataTable$Player) > 0){
    
    dataTable$playerId <- ifelse(is.na(as.numeric(playerId)), playerId, as.numeric(playerId))
    
  }
  
  if(!is.null(dim(dataTable))){
      dataTable[,nameCol] <- getPlayerName(getPlayerName(getPlayerName(dataTable[, nameCol])))
  }
  names(dataTable)<- gsub("[^a-zA-Z0-9]", "", names(dataTable))
  
  return(dataTable)
}

