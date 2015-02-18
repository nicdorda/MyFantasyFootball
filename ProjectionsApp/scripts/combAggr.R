
h.l <- function(x){
  tryCatch({
    wilcox.test(x,na.action="na.exclude",conf.int=TRUE)
  },
  error=function(e){
    return(list(estimate=median(x),conf.int=c(median(x)-sd(x),median(x)+sd(x))))
  })
}

combProj <- function(pos, rawProj){
  posCode <- playerPositions$posCode[playerPositions$posId == pos]
  posProj <- switch(posCode, 
                    "QB" = data.frame(playerId = as.character(), paAtt = as.numeric(), paCmp = as.numeric(), paYd = as.numeric(), paTD = as.numeric(), paInt = as.numeric(),
                                      ruAtt = as.numeric(), ruYd = as.numeric(), ruTD = as.numeric(), fumLost = as.numeric(), twoPts = as.numeric(), retTD = as.numeric(), projAnalystId = as.numeric()),
                    "RB" = data.frame(playerId = as.character(), ruAtt = as.numeric(), ruYd = as.numeric(), ruTD = as.numeric(), fumLost = as.numeric(), paRec = as.numeric(), 
                                      reYd = as.numeric(), reTD = as.numeric(), twoPts = as.numeric(), retTD = as.numeric(), projAnalystId = as.numeric()),
                    "WR" = data.frame(playerId = as.character(), ruAtt = as.numeric(), ruYd = as.numeric(), ruTD = as.numeric(), fumLost = as.numeric(), paRec = as.numeric(), 
                                      reYd = as.numeric(), reTD = as.numeric(), twoPts = as.numeric(), retTD = as.numeric(), projAnalystId = as.numeric()),
                    "TE" = data.frame(playerId = as.character(), ruAtt = as.numeric(), ruYd = as.numeric(), ruTD = as.numeric(), fumLost = as.numeric(), paRec = as.numeric(), 
                                      reYd = as.numeric(), reTD = as.numeric(), twoPts = as.numeric(), retTD = as.numeric(), projAnalystId = as.numeric()),
                    "K" = data.frame(playerId = as.character(), FGA = as.numeric(), FGM = as.numeric(), FG0019 = as.numeric(), FG2029= as.numeric(), FG3039 = as.numeric(),
                                     FG4049 = as.numeric(), FG5099 = as.numeric(), XPM = as.numeric(), XPA = as.numeric(), projAnalystId = as.numeric()),
                    "DEF" = data.frame(playerId = as.character(), defInt = as.numeric(), defSacks = as.numeric(), fumRec = as.numeric(), defSafety = as.numeric(), defTD = as.numeric(), 
                                       blkKick = as.numeric(), retTD = as.numeric(), ptsAllow = as.numeric(), projAnalystId = as.numeric()) 
  )
  
  
  for(analystPos in names(rawProj[grep(posCode, names(rawProj))])){
    
    posProj <- rbind.fill(posProj, rawProj[[analystPos]][,which(names(rawProj[[analystPos]]) %in% names(posProj))])
  }
  return(posProj)
}


aggrProj <- function(pos, allProj){
  
  posCode <- playerPositions$posCode[playerPositions$posId == pos]
  projDt <- data.table(allProj[[posCode]])
  setkey(projDt, playerId)
  
  medId <- siteAnalysts$projAnalystId[siteAnalysts$analystName == "Aggregate"]
  avgId <- siteAnalysts$projAnalystId[siteAnalysts$analystName == "Average"]
  varId <- siteAnalysts$projAnalystId[siteAnalysts$analystName == "Variance"]
   
  if(posCode %in% c("RB", "WR", "TE")){
    statProj <- list(median = projDt[, list(ruAtt =  h.l(ruAtt)$estimate, ruYd =  h.l(ruYd)$estimate, ruTD =  h.l(ruTD)$estimate, fumLost =  h.l(fumLost)$estimate, paRec = h.l(paRec)$estimate,
                                            reYd = h.l(reYd)$estimate, reTD = h.l(reTD)$estimate, twoPts =  h.l(twoPts)$estimate, retTD =  h.l(retTD)$estimate, nobs = length(ruYd), 
                                            projAnalystId = medId), by = key(projDt)],
                     average = projDt[, list(ruAtt = mean(ruAtt, na.rm = TRUE), ruYd = mean(ruYd, na.rm = TRUE), ruTD = mean(ruTD, na.rm = TRUE), fumLost = mean(fumLost, na.rm = TRUE), 
                                             paRec = mean(paRec, na.rm = TRUE), reYd = mean(reYd, na.rm = TRUE), reTD = mean(reTD, na.rm = TRUE),  twoPts = mean(twoPts, na.rm = TRUE), 
                                             retTD = mean(retTD, na.rm = TRUE), nobs = length(ruYd), projAnalystId = avgId), by = key(projDt)],
                     variance = projDt[, list(ruAtt = 1.4826^2*mad(ruAtt, na.rm = TRUE)^2, ruYd = 1.4826^2*mad(ruYd, na.rm = TRUE)^2, ruTD = 1.4826^2*mad(ruTD, na.rm = TRUE)^2, 
                                              fumLost = 1.4826^2*mad(fumLost, na.rm = TRUE)^2, paRec = 1.4826^2*mad(paRec, na.rm = TRUE)^2, reYd = 1.4826^2*mad(reYd, na.rm = TRUE)^2, 
                                              reTD = 1.4826^2*mad(reTD, na.rm = TRUE)^2,  twoPts = 1.4826^2*mad(twoPts, na.rm = TRUE)^2, retTD = 1.4826^2*mad(retTD, na.rm = TRUE)^2, nobs = length(ruYd), 
                                              projAnalystId = varId), by = key(projDt)]
                     )
  } else {
  statProj <- switch(posCode,
                     "QB" = list(median = projDt[, list(paAtt =  h.l(paAtt)$estimate, paCmp =  h.l(paCmp)$estimate, paYd =  h.l(paYd)$estimate, paTD =  h.l(paTD)$estimate, paInt =  h.l(paInt)$estimate,
                                                 ruAtt =  h.l(ruAtt)$estimate, ruYd =  h.l(ruYd)$estimate, ruTD =  h.l(ruTD)$estimate, fumLost =  h.l(fumLost)$estimate, twoPts =  h.l(twoPts)$estimate,
                                                 retTD =  h.l(retTD)$estimate, nobs = length(paAtt), projAnalystId = medId), by = key(projDt)],
                                 average =projDt[, list(paAtt = mean(paAtt, na.rm = TRUE), paCmp = mean(paCmp, na.rm = TRUE), paYd = mean(paYd, na.rm = TRUE), paTD = mean(paTD, na.rm = TRUE), 
                                                        paInt = mean(paInt, na.rm = TRUE), ruAtt = mean(ruAtt, na.rm = TRUE), ruYd = mean(ruYd, na.rm = TRUE), ruTD = mean(ruTD, na.rm = TRUE),
                                                        fumLost = mean(fumLost, na.rm = TRUE), twoPts = mean(twoPts, na.rm = TRUE), retTD = mean(retTD, na.rm = TRUE), nobs = length(paAtt), 
                                                        projAnalystId = avgId), by = key(projDt)],
                                 variance =projDt[, list(paAtt = 1.4826^2*mad(paAtt, na.rm = TRUE)^2, paCmp = 1.4826^2*mad(paCmp, na.rm = TRUE)^2, paYd = 1.4826^2*mad(paYd, na.rm = TRUE)^2, paTD = 1.4826^2*mad(paTD, na.rm = TRUE)^2, 
                                                         paInt = 1.4826^2*mad(paInt, na.rm = TRUE)^2, ruAtt = 1.4826^2*mad(ruAtt, na.rm = TRUE)^2, ruYd = 1.4826^2*mad(paYd, na.rm = TRUE)^2, ruTD = 1.4826^2*mad(ruTD, na.rm = TRUE)^2,
                                                         fumLost = 1.4826^2*mad(fumLost, na.rm = TRUE)^2, twoPts = 1.4826^2*mad(twoPts, na.rm = TRUE)^2, retTD = 1.4826^2*mad(retTD, na.rm = TRUE)^2, nobs = length(paAtt), 
                                                         projAnalystId = varId), by = key(projDt)]
                                 ),
                     "K" = list(median = projDt[, list(XPM = h.l(XPM)$estimate, FGM = h.l(FGM)$estimate, FG0019 = h.l(FG0019)$estimate, FG2029 = h.l(FG2029)$estimate,
                                                       FG3039 = h.l(FG3039)$estimate, FG4049 = h.l(FG4049)$estimate, FG5099 = h.l(FG5099)$estimate, nobs = length(XPM),
                                                       projAnalystId = medId), by = key(projDt)],
                                average = projDt[, list(XPM = mean(XPM, na.rm = TRUE), FGM = mean(FGM, na.rm = TRUE), FG0019 = mean(FG0019, na.rm = TRUE), FG2029 = mean(FG2029, na.rm = TRUE), FG3039 = mean(FG3039, na.rm = TRUE), 
                                                         FG4049 = mean(FG4049, na.rm = TRUE), FG5099 = mean(FG5099, na.rm = TRUE), nobs = length(XPM), projAnalystId = avgId),
                                                  by = key(projDt)],
                                variance = projDt[, list(XPM = 1.4826^2*mad(XPM, na.rm = TRUE)^2, FGM = 1.4826^2*mad(FGM, na.rm = TRUE)^2, FG0019 = 1.4826^2*mad(FG0019, na.rm = TRUE)^2,
                                                         FG2029 = 1.4826^2*mad(FG2029, na.rm = TRUE)^2, FG3039 = 1.4826^2*mad(FG3039, na.rm = TRUE)^2,FG4049 = 1.4826^2*mad(FG4049, na.rm = TRUE)^2,
                                                         FG5099 = 1.4826^2*mad(FG5099, na.rm = TRUE)^2, nobs = length(XPM), projAnalystId = varId), 
                                                  by = key(projDt)]
                                ),
                     "DEF" = list(median = projDt[, list(ptsAllow = h.l(ptsAllow)$estimate, defSacks = h.l(defSacks)$estimate, defSafety = h.l(defSafety)$estimate, 
                                                         defInt = h.l(defInt)$estimate, fumRec = h.l(fumRec)$estimate, blkKick = h.l(blkKick)$estimate, 
                                                         defTD = h.l(defTD)$estimate, retTD = h.l(retTD)$estimate, nobs = length(ptsAllow), projAnalystId = medId), 
                                                  by = key(projDt)],
                                  average = projDt[, list(ptsAllow = mean(ptsAllow, na.rm = TRUE), defSacks = mean(defSacks, na.rm = TRUE), defSafty = mean(defSafety, na.rm = TRUE), defInt = mean(defInt, na.rm = TRUE),
                                                          fumRec = mean(fumRec, na.rm = TRUE), blkKick = mean(blkKick, na.rm = TRUE), defTD = mean(defTD, na.rm = TRUE), retTD = mean(retTD, na.rm = TRUE), nobs = length(ptsAllow), 
                                                          projAnalystId = avgId), by = key(projDt)],
                                  variance = projDt[, list(ptsAllow = 1.486^2*mad(ptsAllow, na.rm = TRUE)^2, defSacks = 1.486^2*mad(defSacks, na.rm = TRUE)^2, defSafety = 1.486^2*mad(defSafety, na.rm = TRUE)^2,
                                                           defInt = 1.486^2*mad(defInt, na.rm = TRUE)^2, fumRec = 1.486^2*mad(fumRec, na.rm = TRUE)^2, blkKick = 1.486^2*mad(blkKick, na.rm = TRUE)^2, 
                                                           defTD = 1.486^2*mad(defTD, na.rm = TRUE)^2, retTD = 1.486^2*mad(retTD, na.rm = TRUE)^2, nobs = length(ptsAllow), projAnalystId = varId),
                                                    by = key(projDt)]
                                  )

  )
  
  }
  incProgress(1/24)
  return(statProj)
  
}

addAgg<- function(pos, fromDataList, toData){
  posCode <- playerPositions$posCode[playerPositions$posId == pos]
  
  newData <- toData[[posCode]]
  for(nme in names(fromDataList[[posCode]])){
    
    newData <- rbind.fill(newData, fromDataList[[posCode]][[nme]] )  
  }
  incProgress(1/24)
  return(newData)
}


