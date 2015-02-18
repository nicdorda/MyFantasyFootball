library(XML)
library(httr)
library(plyr)
library(data.table)
library(mclust)
library(RODBC)


source(paste(getwd(), "/scripts/myData.R", sep=""), local = TRUE)
source(paste(getwd(), "/functions/fetchData.R", sep=""))
source(paste(getwd(), "/functions/getNames.R", sep=""))
source(paste(getwd(), "/scripts/combAggr.R", sep=""))
source(paste(getwd(), "/functions/ptsLeague.R", sep=""))
source(paste(getwd(), "/functions/calcPts.R", sep=""))
source(paste(getwd(), "/functions/getEcrRanks.R", sep=""))


