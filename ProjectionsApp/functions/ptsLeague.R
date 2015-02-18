## File: ptsLeague.R
## Date: 02/16/2015
## Author: Dennis Andersen [andersen.dennis@live.com]
## -------------------------------------------------
## Note:
## This script contains two functions that are best used with lapply
## - calcPts which is used to calculate points for each league defined in the the leagueList
## - leaguePts which calcuclates the total points for each player 
##############################################################################################
library(reshape)
calcPts <- function(pos, projData){
  posCode <- playerPositions$posCode[playerPositions$posId == pos]
  data <- projData[[posCode]]
  
  Pts <- as.data.frame(t(apply(data,1,function(rowData)unlist(lapply(leagueList, leaguePts, pos, rowData)))))
  
  names(Pts) <- names(leagueList)
  data <- cbind(data, Pts)
  incProgress(1/12)
  return(data[,c("playerId", "projAnalystId", "nobs", names(leagueList))])
}

leaguePts <- function(leagueId, posId, data){
  pts <- ifelse(posId <= 4, ydPts(data, posId, leagueId) + multPts(data, posId, leagueId),multPts(data, posId, leagueId))
  return(format(pts, scientific = FALSE)  )
}

unpivotPts <- function(pos, ptsData){
  posCode <- playerPositions$posCode[playerPositions$posId == pos]
  idCols <- c("playerId", "projAnalystId", "nobs")
  data <- melt(ptsData[[posCode]], id= idCols)
  names(data) <- c(idCols, "leagueCode", "projPts")
  data$projPts <- sapply(as.numeric(as.character(data$projPts)), function(x)format(x, scientific = FALSE))
  incProgress(1/12)
  return(data[data$projPts > 0,])
}