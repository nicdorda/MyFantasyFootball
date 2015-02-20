## File: calcPts.R
## Date: 02/16/2015
## Author: Dennis Andersen [andersen.dennis@live.com]
## -------------------------------------------------------
## Note:
## This script contains three functions to calculate points. All functions derive data from a database table read
## in the myData.R file
## - ydPts calculates points for yardage. The function can distinguish point for total, rush and receiving yards
##   assuming that only total or rush and receiving yards are included. It also adds bonuns pts for yards.
## - multPts calculates points for other stats such as TDs, receptions and interceptions. 
## - defPts calculates points for pts allowed for a defense. This function needs to be updated as new leagues are
##   added
##################################################################################################################

ydPts <- function(dataRow, posId, leagueId = 1){
  toYdMult <- ydScoring[ydScoring$leagueId == leagueId & ydScoring$posId == posId & ydScoring$ydType == "total", "ydMult"]
  ruYdMult <- ydScoring[ydScoring$leagueId == leagueId & ydScoring$posId == posId & ydScoring$ydType == "rush", "ydMult"]
  paYdMult <- ydScoring[ydScoring$leagueId == leagueId & ydScoring$posId == posId & ydScoring$ydType == "pass", "ydMult"]
  
  toPrYds <- ydScoring[ydScoring$leagueId == leagueId & ydScoring$posId == posId & ydScoring$ydType == "total", "perYds"]
  ruPrYds <- ydScoring[ydScoring$leagueId == leagueId & ydScoring$posId == posId & ydScoring$ydType == "rush", "perYds"]
  paPrYds <- ydScoring[ydScoring$leagueId == leagueId & ydScoring$posId == posId & ydScoring$ydType == "pass", "perYds"]

  rushYds <- ifelse(is.na(dataRow["ruYd"]), 0, dataRow["ruYd"])
  passYds <- ifelse(posId == 1, dataRow["paYd"], dataRow["reYd"])
  passYds <- ifelse(is.na(passYds), 0, passYds)

  passYds <- as.numeric(passYds)
  rushYds <- as.numeric(rushYds)
  analystId <- dataRow["projAnalystId"]
  if(length(toYdMult) > 0){
      pts <- ifelse(analystId != varId, toYdMult * trunc((passYds+ rushYds)/toPrYds), toYdMult^2 * (passYds+ rushYds)/toPrYds^2)
    } else {
      pts <- ifelse(analystId != varId, paYdMult * trunc(passYds/paPrYds) + ruYdMult * trunc(rushYds/ruPrYds), 
                    paYdMult^2 * passYds/paPrYds^2 + ruYdMult^2 * rushYds/ruPrYds^2)
    }
  
  if(leagueId %in% bonusYds$leagueId){
    toBonusYd <- bonusYds$perYds[bonusYds$leagueId == leagueId & bonusYds$posId == posId & bonusYds$ydType == "total"]
    ruBonusYd <- bonusYds$perYds[bonusYds$leagueId == leagueId & bonusYds$posId == posId & bonusYds$ydType == "rush"]
    paBonusYd <- bonusYds$perYds[bonusYds$leagueId == leagueId & bonusYds$posId == posId & bonusYds$ydType == "pass"]
    
    toBonusType <- bonusYds$onceBonus[bonusYds$leagueId == leagueId & bonusYds$posId == posId & bonusYds$ydType == "total"]
    ruBonusType <- bonusYds$onceBonus[bonusYds$leagueId == leagueId & bonusYds$posId == posId & bonusYds$ydType == "rush"]
    paBonusType <- bonusYds$onceBonus[bonusYds$leagueId == leagueId & bonusYds$posId == posId & bonusYds$ydType == "pass"]
    
    toBonusMult <- bonusYds$bonusMult[bonusYds$leagueId == leagueId & bonusYds$posId == posId & bonusYds$ydType == "total"]
    ruBonusMult <- bonusYds$bonusMult[bonusYds$leagueId == leagueId & bonusYds$posId == posId & bonusYds$ydType == "rush"]
    paBonusMult <- bonusYds$bonusMult[bonusYds$leagueId == leagueId & bonusYds$posId == posId & bonusYds$ydType == "pass"]
    
    if(length(toBonusMult)>0){
      bonusPts <- ifelse(toBonusType == 0, toBonusMult * trunc((passYds+rushYds)/toBonusYd),
                         toBonusMult *  ((passYds+rushYds) >= toBonusYd))
    } else {
      bonusPts <- ifelse(paBonusType == 0, paBonusMult * trunc(passYds/paBonusYd),
                         paBonusMult *  (passYds >= paBonusYd)) + 
                  ifelse(ruBonusType == 0, ruBonusMult * trunc(rushYds/ruBonusYd),
                         ruBonusMult *  (rushYds >= ruBonusYd))
    }
    pts <- pts + bonusPts
  }
  return(as.numeric(pts))
}


multPts <- function(dataRow, posId, leagueId = 1){
  
  pts <- 0
  colNames <- names(dataRow)[which(names(dataRow) %in% multScoring$multField[multScoring$leagueId == leagueId])]
  
  for(dataCol in colNames){
    colVal <- ifelse(is.na(dataRow[dataCol]), 0, as.numeric(dataRow[dataCol]))
    valMult <- as.numeric(multScoring$multValue[multScoring$posId == posId & multScoring$leagueId == leagueId & 
                                                  multScoring$multField == dataCol])
    pts <- pts + colVal * valMult * ifelse(as.numeric(dataRow["projAnalystId"]) == varId, valMult, 1)
  }
  
  if(posId == 6){
    pts <- pts + ifelse(as.numeric(dataRow["projAnalystId"]) == varId, defPts(dataRow["ptsAllow"], leagueId)^2, defPts(dataRow["ptsAllow"], leagueId))
    
  }
  return(as.numeric(pts))
}

defPts <- function(ptsAllow, leagueId){
  ptsSqlQry <- paste("SELECT thresHold, ptsScore from ptsAllowScore where leagueId =", leagueId)
  
  ptsScore <- ffSqlQry(ptsSqlQry)
  
  is.season <- (ptsAllow > 100)
  is.season <- ifelse(is.na(is.season), TRUE, is.season)
  if(is.season == TRUE){
    ptsAllow <- ptsAllow/16
  }
  
  for(r in 1:nrow(ptsScore)){
    if(ptsAllow <= ptsScore$thresHold[r]){
      pts <- ptsScore$ptsScore[r]
      break()
    }
  }
  
  if(is.season == TRUE){
    pts = pts *16
  }
  return(as.numeric(pts))
}