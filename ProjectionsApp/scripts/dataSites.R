dataSites <- list(
######
  cbs = list(name = "CBS",
             weekUrl = "http://fantasynews.cbssports.com/fantasyfootball/stats/weeklyprojections/{$Pos}/{$WeekNo}/{$SrcID}/standard?&print_rows=9999",
             seasonUrl = "http://fantasynews.cbssports.com/fantasyfootball/stats/weeklyprojections/{$Pos}/season/{$SrcID}/standard?&print_rows=9999",
             nameColSeason = 1,
             nameColWeek = 1,
             posData =  list(
               qb = list(
                 name = "QB",
                 id = -1,
                 pgSeq = c(1:1),
                 rowsRemove = c(1,2), 
                 weekCols = c("Player", "paAtt", "paCmp", "paYd", "paTD", "paInt", "paCmpPct", "paYAtt", "ruAtt", "ruYd", "ruAvg", "ruTD", "fumLost", "FPTS"),
                 seasonCols = c("Player", "paAtt", "paCmp", "paYd", "paTD", "paInt", "paCmpPct", "paYAtt", "ruAtt", "ruYd", "ruAvg", "ruTD", "fumLost", "FPTS"),
                 weekTypes = c("character", rep("numeric",13)),
                 seasonTypes = c("character", rep("numeric",13))  
                 ),
               rb = list(
                 name = "RB",
                 id = -1,
                 pgSeq = c(1:1),
                 rowsRemove = c(1,2), 
                 weekCols = c("Player", "ruAtt", "ruYd", "ruAvg", "ruTD", "paRec", "reYd", "reAvg", "reTD", "fumLost", "FPTS"  ),
                 seasonCols = c("Player", "ruAtt", "ruYd", "ruAvg", "ruTD", "paRec", "reYd", "reAvg", "reTD", "fumLost", "FPTS"  ),
                 weekTypes =  c("character", rep("numeric",10)),
                 seasonTypes =  c("character", rep("numeric",10))
                 ),
               wr = list(
                 name = "WR",
                 id = -1,
                 pgSeq = c(1:1),
                 rowsRemove = c(1,2), 
                 weekCols = c("Player", "paRec", "reYd", "reAvg", "reTD", "fumLost", "FPTS" ),
                 seasonCols = c("Player", "paRec", "reYd", "reAvg", "reTD", "fumLost", "FPTS" ),
                 weekTypes = c("character", rep("numeric",6)),
                 seasonTypes = c("character", rep("numeric",6))
                 ),
               te = list(
                 name = "TE",
                 id = -1,
                 pgSeq = c(1:1),
                 rowsRemove = c(1,2), 
                 weekCols = c("Player", "paRec", "reYd", "reAvg", "reTD", "fumLost", "FPTS" ),
                 seasonCols = c("Player", "paRec", "reYd", "reAvg", "reTD", "fumLost", "FPTS" ),
                 weekTypes = c("character", rep("numeric",6)),
                 seasonTypes = c("character", rep("numeric",6))
                 ),
               k = list(
                 name = "K",
                 id = -1,
                 pgSeq = c(1:1),
                 rowsRemove = c(1), 
                 weekCols = c("Player", "FGM", "FGA", "XPM", "FPTS"),
                 seasonCols = c("Player", "FGM", "FGA", "XPM", "FPTS"),
                 weekTypes = c("character", rep("numeric", 4)),
                 seasonTypes = c("character", rep("numeric", 4))
                 ),
               def = list(
                 name = "DST",
                 id = -1,
                 pgSeq = c(1:1),
                 rowsRemove = c(1), 
                 weekCols = c("Player", "defInt", "fumRec", "fumFor", "defSacks", "defTD", "defSafety", "ptsAllow", "ydsAllow", "FPTS"),
                 seasonCols = c("Player", "defInt", "fumRec", "fumFor", "defSacks", "defTD", "defSafety", "ptsAllow", "ydsAllow", "FPTS"),
                 weekTypes = c("character", rep("numeric", 9)),
                 seasonTypes = c("character", rep("numeric", 9))
                 )
             )
             ),
##### 
yahoo = list(name = "Yahoo",
               weekUrl = "http://football.fantasysports.yahoo.com/f1/39345/players?status=A&pos={$Pos}&cut_type=9&stat1=S_PW_{$WeekNo}&myteam=0&sort=PTS&sdir=1&count={$PgeID}",
               seasonUrl = "http://football.fantasysports.yahoo.com/f1/39345/players?&sort=PTS&sdir=1&status=A&pos={$Pos}&cut_type=9&stat1=S_PS_{$Season}&jsenabled=1&count={$PgeID}",
               nameColSeason = 2,
               nameColWeek = 2,
               posData =  list(
                 qb = list(
                   name = "QB",
                   id = -1,
                   pgSeq = seq(from=0, to=125, by=25),
                   rowsRemove = NA, 
                   weekCols = c("X1", "Player", "X2", "X3", "FPTS", "pctOwned", "projRank", "actRank",  "paYd", "paTD", "paInt",
                                "ruAtt", "ruYd", "ruTD", "paTgt", "paRec", "reYd", "reTD", "retTD", "twoPts", "fumLost"),
                   seasonCols = c("X1", "Player", "X2", "X3", "FPTS", "pctOwned", "projRank", "actRank",  "paYd", "paTD", "paInt", 
                                  "ruAtt", "ruYd", "ruTD", "paTgt", "paRec", "reYd", "reTD", "retTD", "twoPts", "fumLost"),
                   weekTypes =  c(rep("character",8), rep("numeric",13)),
                   seasonTypes = c(rep("character",8), rep("numeric",13))
                 ),
                 rb = list(
                   name = "RB",
                   id = -1,
                   pgSeq = seq(from=0, to=275, by=25),
                   rowsRemove = NA, 
                   weekCols = c("X1", "Player", "X2", "X3", "FPTS", "pctOwned", "projRank", "actRank",  "paYd", "paTD", "paInt",
                                "ruAtt", "ruYd", "ruTD", "paTgt", "paRec", "reYd", "reTD", "retTD", "twoPts", "fumLost"),
                   seasonCols = c("X1", "Player", "X2", "X3", "FPTS", "pctOwned", "projRank", "actRank",  "paYd", "paTD", "paInt",
                                  "ruAtt", "ruYd", "ruTD", "paTgt", "paRec", "reYd", "reTD", "retTD", "twoPts", "fumLost"),
                   weekTypes =  c(rep("character",8), rep("numeric",13)),
                   seasonTypes = c(rep("character",8), rep("numeric",13))
                 ),
                 wr = list(
                   name = "WR",
                   id = -1,
                   pgSeq = seq(from=0, to=425, by=25),
                   rowsRemove = NA, 
                   weekCols = c("X1", "Player", "X2", "X3", "FPTS", "pctOwned", "projRank", "actRank",  "paYd", "paTD", "paInt",
                                "ruAtt", "ruYd", "ruTD", "paTgt", "paRec", "reYd", "reTD", "retTD", "twoPts", "fumLost"),
                   seasonCols = c("X1", "Player", "X2", "X3", "FPTS", "pctOwned", "projRank", "actRank",  "paYd", "paTD", "paInt", 
                                  "ruAtt", "ruYd", "ruTD", "paTgt", "paRec", "reYd", "reTD", "retTD", "twoPts", "fumLost"),
                   weekTypes =  c(rep("character",8), rep("numeric",13)),
                   seasonTypes = c(rep("character",8), rep("numeric",13))
                 ),
                 te = list(
                   name = "TE",
                   id = -1,
                   pgSeq = seq(from=0, to=225, by=25),
                   rowsRemove = NA, 
                   weekCols = c("X1", "Player", "X2", "X3", "FPTS", "pctOwned", "projRank", "actRank",  "paYd", "paTD", "paInt", 
                                "ruAtt", "ruYd", "ruTD", "paTgt", "paRec", "reYd", "reTD", "retTD", "twoPts", "fumLost"),
                   seasonCols = c("X1", "Player", "X2", "X3", "FPTS", "pctOwned", "projRank", "actRank",  "paYd", "paTD", "paInt",
                                  "ruAtt", "ruYd", "ruTD", "paTgt", "paRec", "reYd", "reTD", "retTD", "twoPts", "fumLost"),
                   weekTypes =  c(rep("character",8), rep("numeric",13)),
                   seasonTypes = c(rep("character",8), rep("numeric",13))
                 ),
                 k = list(
                   name = "K",
                   id = -1,
                   pgSeq = seq(from=0, to=50, by=25),
                   rowsRemove = NA, 
                   weekCols = c("X1", "Player", "X2", "X3", "FPTS", "pctOwned", "projRank", "actRank", "FG0019", "FG2029", "FG3039", "FG4049", "FG5099", "XPM", "X4"),
                   seasonCols = c("X1", "Player", "X2", "X3", "FPTS", "pctOwned", "projRank", "actRank", "FG0019", "FG2029", "FG3039", "FG4049", "FG5099", "XPM", "X4"),
                   weekTypes = c(rep("character",8), rep("numeric",7)),
                   seasonTypes = c(rep("character",8), rep("numeric",7))
                 ),
                 def = list(
                   name = "DEF",
                   id = -1,
                   pgSeq = seq(from=0, to=25, by=25),
                   rowsRemove = NA, 
                   weekCols = c("X1", "Player", "X2", "X3", "FPTS", "pctOwned", "projRank", "actRank", "ptsAllow", "defSacks", "defSafety",  "defInt", "fumRec", "defTD", "blkKick", "retTD", "X4"),
                   seasonCols = c("X1", "Player", "X2", "X3", "FPTS", "pctOwned", "projRank", "actRank", "ptsAllow", "defSacks", "defSafety",  "defInt", "fumRec", "defTD", "blkKick", "retTD", "X4"),
                   weekTypes =  c(rep("character",8), rep("numeric",9)), 
                   seasonTypes =  c(rep("character",8), rep("numeric",9))
                 )
               )
  ),
#####
espn = list(name = "ESPN",
            weekUrl = "http://games.espn.go.com/ffl/tools/projections?&slotCategoryId={$PosID}&scoringPeriodId={$WeekNo}&seasonId={$Season}&startIndex={$PgeID}",
            seasonUrl = "http://games.espn.go.com/ffl/tools/projections?&seasonTotals=true&seasonId={$Season}&slotCategoryId={$PosID}&startIndex={$PgeID}",
            nameColSeason = 2,
            nameColWeek = 1,
            posData =  list(
              qb = list(
                name = "QB",
                id = 0,
                pgSeq = seq(from = 0, to = 160, by=40),
                rowsRemove = 1, 
                weekCols = c("Player", "Opp", "Status", "cmpAtt", "paYd", "paTD", "paInt", "ruAtt", "ruYd", "ruTD", "paRec", "reYd", "reTD", "FPTS"),
                seasonCols = c("Rank", "Player", "cmpAtt", "paYd", "paTD", "paInt", "ruAtt", "ruYd", "ruTD", "paRec", "reYd", "reTD", "FPTS"), 
                weekTypes =  c(rep("character",4), rep("numeric",10)),
                seasonTypes = c("numeric", rep("character",2), rep("numeric",10))
              ),
              rb = list(
                name = "RB",
                id = 2,
                pgSeq = seq(from = 0, to = 400, by=40),
                rowsRemove = 1, 
                weekCols = c("Player", "Opp", "Status", "cmpAtt", "paYd", "paTD", "paInt", "ruAtt", "ruYd", "ruTD", "paRec", "reYd", "reTD", "FPTS"),
                seasonCols = c("Rank", "Player", "cmpAtt", "paYd", "paTD", "paInt", "ruAtt", "ruYd", "ruTD", "paRec", "reYd", "reTD", "FPTS"), 
                weekTypes =  c(rep("character",4), rep("numeric",10)),
                seasonTypes = c("numeric", rep("character",2), rep("numeric",10))
              ),
              wr = list(
                name = "WR",
                id = 4,
                pgSeq = seq(from = 0, to = 520, by=40),
                rowsRemove = 1, 
                weekCols = c("Player", "Opp", "Status", "cmpAtt", "paYd", "paTD", "paInt", "ruAtt", "ruYd", "ruTD", "paRec", "reYd", "reTD", "FPTS"),
                seasonCols = c("Rank", "Player", "cmpAtt", "paYd", "paTD", "paInt", "ruAtt", "ruYd", "ruTD", "paRec", "reYd", "reTD", "FPTS"), 
                weekTypes =  c(rep("character",4), rep("numeric",10)),
                seasonTypes = c("numeric", rep("character",2), rep("numeric",10))
              ),
              te = list(
                name = "TE",
                id = 6,
                pgSeq = seq(from = 0, to = 280, by=40),
                rowsRemove = 1, 
                weekCols = c("Player", "Opp", "Status", "cmpAtt", "paYd", "paTD", "paInt", "ruAtt", "ruYd", "ruTD", "paRec", "reYd", "reTD", "FPTS"),
                seasonCols = c("Rank", "Player", "cmpAtt", "paYd", "paTD", "paInt", "ruAtt", "ruYd", "ruTD", "paRec", "reYd", "reTD", "FPTS"), 
                weekTypes =  c(rep("character",4), rep("numeric",10)),
                seasonTypes = c("numeric", rep("character",2), rep("numeric",10))
              )
              )
),
#####
nfl = list(name = "NFL",
           weekUrl ="http://fantasy.nfl.com/research/projections?position={$PosID}&sort=projectedPts&statCategory=projectedStats&statSeason={$Season}&statType=weekProjectedStats&offset={$PgeID}&statweek={$WeekNo}",
           seasonUrl ="http://fantasy.nfl.com/research/projections?position={$PosID}&sort=projectedPts&statCategory=projectedStats&statSeason={$Season}&statType=seasonProjectedStats&offset={$PgeID}",
           nameColSeason = 1, 
           nameColWeek = 1, 
           posData = list(
             qb = list(
               name = "QB",
               id = 1,
               pgSeq = seq(from=1, by=25, length=5),
               rowsRemove = NA,
               weekCols = c("Player", "Opp", "paYd", "paTD", "paInt", "ruYd", "ruTD", "reYd", "reTD", "fumTD", "twoPts", "fumLost", "FPTS"),
               seasonCols = c("Player", "Opp", "gmsPlay", "paYd", "paTD", "paInt", "ruYd", "ruTD", "reYd", "reTD", "fumTD", "twoPts", "fumLost", "FPTS"),
               weekTypes = c(rep("character",2), rep("numeric",11)),
               seasonTypes = c(rep("character",2), rep("numeric",12))
             ),
             rb = list(
               name = "RB",
               id = 2,
               pgSeq = seq(from=1, by=25, length=9),
               rowsRemove = NA,
               weekCols = c("Player", "Opp", "paYd", "paTD", "paInt", "ruYd", "ruTD", "reYd", "reTD", "fumTD", "twoPts", "fumLost", "FPTS"),
               seasonCols = c("Player", "Opp", "gmsPlay", "paYd", "paTD", "paInt", "ruYd", "ruTD", "reYd", "reTD", "fumTD", "twoPts", "fumLost", "FPTS"),
               weekTypes = c(rep("character",2), rep("numeric",11)),
               seasonTypes = c(rep("character",2), rep("numeric",12))
             ),
             wr = list(
               name = "WR",
               id = 3,
               pgSeq = seq(from=1, by=25, length=11),
               rowsRemove = NA,
               weekCols = c("Player", "Opp", "paYd", "paTD", "paInt", "ruYd", "ruTD", "reYd", "reTD", "fumTD", "twoPts", "fumLost", "FPTS"),
               seasonCols = c("Player", "Opp", "gmsPlay", "paYd", "paTD", "paInt", "ruYd", "ruTD", "reYd", "reTD", "fumTD", "twoPts", "fumLost", "FPTS"),
               weekTypes = c(rep("character",2), rep("numeric",11)),
               seasonTypes = c(rep("character",2), rep("numeric",12))
             ),
             te = list(
               name = "TE",
               id = 4,
               pgSeq = seq(from=1, by=25, length=7),
               rowsRemove = NA,
               weekCols = c("Player", "Opp", "paYd", "paTD", "paInt", "ruYd", "ruTD", "reYd", "reTD", "fumTD", "twoPts", "fumLost", "FPTS"),
               seasonCols = c("Player", "Opp", "gmsPlay", "paYd", "paTD", "paInt", "ruYd", "ruTD", "reYd", "reTD", "fumTD", "twoPts", "fumLost", "FPTS"),
               weekTypes = c(rep("character",2), rep("numeric",11)),
               seasonTypes = c(rep("character",2), rep("numeric",12))
             ),
             k = list(
               name = "K",
               id = 7,
               pgSeq = seq(from=1, by=25, length=2),
               rowsRemove = NA,
               weekCols = c("Player", "Opp", "XPM", "FG0019", "FG2029", "FG3039", "FG4049", "FG5099", "FPTS"),
               seasonCols = c("Player", "Opp", "gmsPlay", "XPM", "FG0019", "FG2029", "FG3039", "FG4049", "FG5099", "FPTS"),
               weekTypes =  c(rep("character",2), rep("numeric",7)),
               seasonTypes =  c(rep("character",2), rep("numeric",8))
             ),
             def = list(
               name = "DEF",
               id = 8,
               pgSeq = seq(from=1, by=25, length=2),
               rowsRemove = NA,
               weekCols = c("Player", "Opp", "defSacks", "defInt", "fumRec", "defSafety", "defTD", "retTD", "ptsAllow", "FPTS"),
               seasonCols = c("Player", "Opp", "gmsPlay",  "defSacks", "defInt", "fumRec", "defSafety", "defTD", "retTD", "ptsAllow", "FPTS"),
               weekTypes =  c(rep("character",2), rep("numeric",8)),
               seasonTypes =  c(rep("character",2), rep("numeric",9))
             )
             )
  ),
#####
fox = list(name = "FOX",
           weekUrl = "http://www.foxsports.com/fantasy/football/commissioner/Research/Projections.aspx?page={$PgeID}&position={$PosID}&split=4&playerSearchStatus=1",
           seasonUrl = "http://www.foxsports.com/fantasy/football/commissioner/Research/Projections.aspx?page={$PgeID}&position={$PosID}&split=3&playerSearchStatus=1",
           nameColSeason = 1,
           nameColWeek = 1,
           posData = list(
             qb = list(
               name = "QB",
               id = 8,
               pgSeq = c(1:6),
               rowsRemove = NA,
               weekCols = c("Player", "X1", "paTD", "paYd", "paAtt", "paCmp", "paInt", "ruTD", "ruYd", "ruAtt","twoPts", "fumTD", "fumLost", "FPTS"),
               seasonCols = c("Player", "X1", "paTD", "paYd", "paAtt", "paCmp", "paInt", "ruTD", "ruYd", "ruAtt","twoPts", "fumTD", "fumLost", "FPTS"),
               weekTypes = c(rep("character", 2), rep("numeric",12)),
               seasonTypes = c(rep("character", 2), rep("numeric",12))
             ),
             rb = list(
               name = "RB",
               id = 16,
               pgSeq = c(1:12),
               rowsRemove = NA,
               weekCols = c("Player", "X1",  "ruTD", "ruYd", "ruAtt", "reTD", "reYd", "paRec", "twoPts", "fumTD", "fumLost", "FPTS"),
               seasonCols = c("Player", "X1",  "ruTD", "ruYd", "ruAtt", "reTD", "reYd", "paRec", "twoPts", "fumTD", "fumLost", "FPTS"),
               weekTypes = c(rep("character", 2), rep("numeric",10)),
               seasonTypes = c(rep("character", 2), rep("numeric",10))
             ),
             wr = list(
               name = "WR",
               id = 1,
               pgSeq = c(1:18),
               rowsRemove = NA,
               weekCols = c("Player", "X1", "reTD", "reYd", "paRec", "ruTD", "ruYd", "ruAtt", "twoPts", "fumTD", "fumLost", "FPTS"),
               seasonCols = c("Player", "X1", "reTD", "reYd", "paRec", "ruTD", "ruYd", "ruAtt", "twoPts", "fumTD", "fumLost", "FPTS"),
               weekTypes = c(rep("character", 2), rep("numeric",10)),
               seasonTypes = c(rep("character", 2), rep("numeric",10))
             ),
             te = list(
               name = "TE",
               id = 4,
               pgSeq = c(1:9),
               rowsRemove = NA,
               weekCols = c("Player", "X1", "reTD", "reYd", "paRec", "ruTD", "ruYd", "ruAtt", "twoPts", "fumTD", "fumLost", "FPTS"),
               seasonCols = c("Player", "X1", "reTD", "reYd", "paRec", "ruTD", "ruYd", "ruAtt", "twoPts", "fumTD", "fumLost", "FPTS"),
               weekTypes = c(rep("character", 2), rep("numeric",10)),
               seasonTypes = c(rep("character", 2), rep("numeric",10))
             ),
             k = list(
               name = "PK",
               id = 64,
               pgSeq = c(1:3),
               rowsRemove = NA,
               weekCols = c("Player", "Status", "FGM", "FGMi", "XPM", "XPMi", "twoPts", "fumTD", "fumLost", "FPTS"),
               seasonCols = c("Player", "Status", "FGM", "FGMi", "XPM", "XPMi", "twoPts", "fumTD", "fumLost", "FPTS"),
               weekTypes = c(rep("character", 2), rep("numeric",8)),
               seasonTypes = c(rep("character", 2), rep("numeric",8))
             ),
             def = list(
               name = "DEF",
               id = 32768,
               pgSeq = c(1:2),
               rowsRemove = NA,
               weekCols = c("Player", "Status", "defTD", "fumRec", "defSacks", "defInt", "defSafety", "ptsAllow", "puRtTD", "kiRtTD", "blkFG", "blkPu", "blkPAT", "stFum", "FPTS"),
               seasonCols = c("Player", "Status", "defTD", "fumRec", "defSacks", "defInt", "defSafety", "ptsAllow", "puRtTD", "kiRtTD", "blkFG", "blkPu", "blkPAT", "stFum", "FPTS"),
               weekTypes = c(rep("character", 2), rep("numeric",13)),
               seasonTypes = c(rep("character", 2), rep("numeric",13))
             )
             )
  ),
#####
fft = list(name = "FFToday",
           weekUrl = "http://www.fftoday.com/rankings/playerwkproj.php?Season={$Season}&GameWeek={$WeekNo}&LeaugeID=1&PosID={$PosID}&cur_page={$PgeID}",
           seasonUrl = "http://www.fftoday.com/rankings/playerproj.php?Season={$Season}&LeaugeID=1&PosID={$PosID}&cur_page={$PgeID}",
           nameColSeason = 2,
           nameColWeek = 2,
           posData = list(
             qb = list(
               name = "QB",
               id = 10,
               pgSeq = 0,
               rowsRemove = 1,
               weekCols =  c("X1", "Player", "Team", "Opp",  "paCmp", "paAtt", "paYd", "paTD", "paInt", "ruAtt", "ruYd", "ruTD", "FPTS"),
               seasonCols =  c("X1", "Player", "Team", "Bye",  "paCmp", "paAtt", "paYd", "paTD", "paInt", "ruAtt", "ruYd", "ruTD", "FPTS"),
               weekTypes = coltypes <- c(rep("character", 4), rep("numeric",9)),
               seasonTypes = coltypes <- c(rep("character", 4), rep("numeric",9))
             ),
             rb = list(
               name = "RB",
               id =20,
               pgSeq = c(0,1),
               rowsRemove = 1,
               weekCols =  c("X1", "Player", "Team", "Opp", "ruAtt", "ruYd", "ruTD", "paRec", "reYd", "reTD", "FPTS"),
               seasonCols =  c("X1", "Player", "Team", "Bye", "ruAtt", "ruYd", "ruTD", "paRec", "reYd", "reTD", "FPTS"),
               weekTypes = c(rep("character", 4), rep("numeric",7)),
               seasonTypes = c(rep("character", 4), rep("numeric",7))
             ),
             wr = list(
               name = "WR",
               id = 30,
               pgSeq = 0:2,
               rowsRemove = 1,
               weekCols =  c("X1", "Player", "Team", "Opp", "paRec", "reYd", "reTD", "FPTS"),
               seasonCols =  c("X1", "Player", "Team", "Bye", "paRec", "reYd", "reTD", "ruAtt", "ruYd", "ruTD",  "FPTS"),
               weekTypes =  c(rep("character", 4), rep("numeric",4)),
              seasonTypes =  c(rep("character", 4), rep("numeric",7))
               ),
             te =list(
               name = "TE",
               id = 40,
               pgSeq = 0:1,
               rowsRemove = 1,
               weekCols =  c("X1", "Player", "Team", "Opp", "paRec", "reYd", "reTD", "FPTS"),
               seasonCols =  c("X1", "Player", "Team", "Opp", "paRec", "reYd", "reTD", "FPTS"),
               weekTypes =  c(rep("character", 4), rep("numeric",4)),
               seasonTypes =   c(rep("character", 4), rep("numeric",4))
             ),
             k = list(
               name = "K",
               id = 80,
               pgSeq = 0,
               rowsRemove = 1,
               weekCols = c("X1", "Player", "Team", "Opp", "FGM", "FGMi", "XPM", "XPMi", "FPTS"),
               seasonCols = c("X1", "Player", "Team", "Bye", "FGM", "FGA", "fgPct", "XPM", "XPA", "FPTS"),
               weekTypes = c(rep("character", 4), rep("numeric",5)),
               seasonTypes = c(rep("character", 4), rep("numeric",6))
           ),
           def = list(
             name = "DEF",
             id = 99,
             pgSeq = 0,
             rowsRemove = 1,
             weekCols = NA,
             seasonCols = c("X1", "Player", "Bye", "defSacks", "fumRec", "defInt", "defTD", "ptsAllow", "paYPG", "ruYPG", "defSafety", "retTD","FPTS"),
             weekTypes = NA,
             seasonTypes = c(rep("character", 3), rep("numeric",10))
           )
           )
),
nmf = list(name = "NumberFire",
           weekUrl = "http://www.numberfire.com/nfl/fantasy/fantasy-football-projections/{$PosID}",
           seasonUrl = NA,
           nameColSeason = NA,
           nameColWeek = 1,
           posData = list(
             qb = list(
               name = "QB",
               id = "qb",
               pgSeq = 1,
               rowsRemove = NA,
               weekCols = c("Player", "Team", "DefRank", "ovrRank", "posRank", "cmpAtt", "paYd", "paTD", "paInt", "ruAtt", "ruYd", "ruTD", "nmfCI", "FPTS", paste("V", 1:24, sep="")),
               seasonCols = NA,
               weekTypes = c(rep("character", 3), rep("numeric", 2), "character", rep("numeric", 6), "character", "numeric", rep(c("numeric", "character", "numeric"),8)),
               seasonType = NA
               ),
             rb = list(
               name = "RB",
               id = "rb",
               pgSeq = 1,
               rowsRemove = NA,
               weekCols = c("Player", "Team", "DefRank", "ovrRank", "posRank", "ruAtt", "ruYd", "ruTD", "paRec", "reYd", "reTD", "nmfCI", "FPTS", paste("V", 1:24, sep="")),
               seasonCols = NA,
               weekTypes = c(rep("character", 3), rep("numeric", 2),  rep("numeric", 6), "character", "numeric", rep(c("numeric", "character", "numeric"),8)),
               seasonTypes = NA
                 ),
             wr = list(
               name = "WR",
               id = "wr",
               pgSeq = 1,
               rowsRemove = NA,
               weekCols = c("Player", "Team", "DefRank", "ovrRank", "posRank", "ruAtt", "ruYd", "ruTD", "paRec", "reYd", "reTD", "nmfCI", "FPTS", paste("V", 1:24, sep="")),
               seasonCols = NA,
               weekTypes = c(rep("character", 3), rep("numeric", 2),  rep("numeric", 6), "character", "numeric", rep(c("numeric", "character", "numeric"),8)),
               seasonTypes = NA
             ),
             te = list(
               name = "TE",
               id = "te",
               pgSeq = 1,
               rowsRemove = NA,
               weekCols = c("Player", "Team", "DefRank", "ovrRank", "posRank", "ruAtt", "ruYd", "ruTD", "paRec", "reYd", "reTD", "nmfCI", "FPTS", paste("V", 1:24, sep="")),
               seasonCols = NA,
               weekTypes = c(rep("character", 3), rep("numeric", 2),  rep("numeric", 6), "character", "numeric", rep(c("numeric", "character", "numeric"),8)),
               seasonTypes = NA
             ),
             k = list(
               name = "K",
               id = "k",
               pgSeq = 1,
               rowsRemove = NA,
               weekCols = c("Player", "Team", "DefRank", "ovrRank", "posRank", "XPM", "FGA", "FGM", "FG0019", "FG2029", "FG3039", "FG4049", "FG5099", "nmfCI", "FPTS", paste("V", 1:24, sep="")),
               seasonCols = NA,
               weekTypes = c(rep("character", 3), rep("numeric", 2),  rep("numeric", 8), "character", "numeric", rep(c("numeric", "character", "numeric"),8)),
               seasonTypes = NA
             ),
             def = list(
               name = "DEF",
               id = "d",
               pgSeq = 1,
               rowsRemove = NA,
               weekCols = c("Player", "Team", "posRank", "ptsAllow", "ydsAllow", "defSacks", "defInt", "fumRec", "defTD",  "nmfCI", "FPTS", paste("V", 1:24, sep="")),
               seasonCols = NA,
               weekTypes = c(rep("character", 2), rep("numeric", 7), "character", "numeric", rep(c("numeric", "character", "numeric"),8)),
               seasonTypes = NA
             )
             )
  ),
#####
ffp = list(name = "FantasyPros",
           weekUrl = "http://www.fantasypros.com/nfl/projections/{$PosID}",
           seasonUrl = NA,
           nameColSeason = NA,
           nameColWeek = 1,
           posData = list(
             qb = list(
               name = "QB",
               id = "qb",
               pgSeq = 1,
               rowsRemove = NA,
               weekCols = c("Player", "paAtt", "paCmp", "paYd", "paTD", "paInt", "ruAtt", "ruYd", "ruTD", "fumLost", "FPTS"),
               seasonCols = NA,
               weekTypes = c("character", rep("numeric", 10)),
               seasonType = NA
             ),
             rb = list(
               name = "RB",
               id = "rb",
               pgSeq = 1,
               rowsRemove = NA,
               weekCols = c("Player", "ruAtt", "ruYd", "ruTD", "paRec", "reYd", "reTD", "fumLost", "FPTS"),
               seasonCols = NA,
               weekTypes = c("character", rep("numeric", 8)),
               seasonTypes = NA
             ),
             wr = list(
               name = "WR",
               id = "wr",
               pgSeq = 1,
               rowsRemove = NA,
               weekCols = c("Player", "ruAtt", "ruYd", "ruTD", "paRec", "reYd", "reTD", "fumLost", "FPTS"),
               seasonCols = NA,
               weekTypes = c("character", rep("numeric", 8)),
               seasonTypes = NA
             ),
             te = list(
               name = "TE",
               id = "te",
               pgSeq = 1,
               rowsRemove = NA,
               weekCols = c("Player", "paRec", "reYd", "reTD", "fumLost", "FPTS"),
               seasonCols = NA,
               weekTypes = c("character", rep("numeric", 5)),
               seasonTypes = NA
             ),
             k = list(
               name = "K",
               id = "k",
               pgSeq = 1,
               rowsRemove = NA,
               weekCols = c("Player", "FGM", "FGA", "XPM", "FPTS"),
               seasonCols = NA,
               weekTypes = c("character", rep("numeric", 4)),
               seasonTypes = NA
             )
           )
),
fbg = list(name = "Footballguys",
           weekUrl = "http://subscribers.footballguys.com/myfbg/myweeklycheatsheet.php?who={$SrcID}&pos={$PosID}&profile=0&week={$WeekNo}",
           seasonUrl = "http://subscribers.footballguys.com/myfbg/myviewprojections.php?projforwhat={$PosID}&projector={$SrcID}&profile=0",
           nameColSeason = 2,
           nameColWeek = 2,
           posData = list(
             qb = list(
               name = "QB",
               id = "qb",
               pgSeq = 1,
               rowsRemove = NA,
               weekCols = c("Rank", "Player", "Team", "Opp", "paCmp", "paAtt", "paYd", "paTD", "paInt", "ruAtt", "ruYd", "ruTD", "FPTS", "X1"),
               seasonCols = c("Rank", "Player", "Team", "Age", "Exp", "paCmp", "paAtt", "cmpPct", "paYd", "paYAt", "paTD", "paInt", "ruAtt", "ruYd", "ruTD", "FPTS", "X1"),
               weekTypes = c("numeric", rep("character", 3), rep("numeric", 10)),
               seasonTypes = c("numeric", rep("character", 2), rep("numeric", 14))
             ),
             rb = list(
               name = "RB",
               id = "rb",
               pgSeq = 1,
               rowsRemove = NA,
               weekCols = c("Rank", "Player", "Team", "Opp",  "ruAtt", "ruYd", "ruTD", "paRec", "reYd", "reTD", "FPTS", "X1"),
               seasonCols = c("Rank", "Player", "Team", "Age", "Exp", "ruAtt", "ruYd", "ruYdA", "ruTD", "paRec", "reYd", "reTD", "FPTS", "X1"),
               weekTypes = c("numeric", rep("character", 3), rep("numeric", 8)),
               seasonTypes = c("numeric", rep("character", 2), rep("numeric", 11))
             ),
             wr = list(
               name = "WR",
               id = "wr",
               pgSeq = 1,
               rowsRemove = NA,
               weekCols = c("Rank", "Player", "Team", "Opp",  "ruAtt", "ruYd", "ruTD", "paRec", "reYd", "reTD", "FPTS", "X1"),
               seasonCols = c("Rank", "Player", "Team", "Age", "Exp", "ruAtt", "ruYd", "ruTD", "paRec", "reYd",  "ydsPRe", "reTD", "FPTS", "X1"),
               weekTypes = c("numeric", rep("character", 3), rep("numeric", 8)),
               seasonTypes = c("numeric", rep("character", 2), rep("numeric", 11))
             ),
             te = list(
               name = "TE",
               id = "te",
               pgSeq = 1,
               rowsRemove = NA,
               weekCols = c("Rank", "Player", "Team", "Opp", "paRec", "reYd", "reTD", "FPTS", "X1"),
               seasonCols = c("Rank", "Player", "Team", "Age", "Exp", "paRec", "reYd",  "ydsPRe", "reTD", "FPTS", "X1"),
               weekTypes = c("numeric", rep("character", 3), rep("numeric", 5)),
               seasonTypes = c("numeric", rep("character", 2), rep("numeric", 8))
             ),
             k = list(
               name = "K",
               id = "pk",
               pgSeq = 1,
               rowsRemove = NA,
               weekCols = c("Rank", "Player", "Team", "Opp", "FGM", "FGA", "XPM", "XPA", "FPTS", "X1"),
               seasonCols = c("Rank", "Player", "Team", "Age", "FGM", "FGA", "fgPct", "XPM", "XPA", "FPTS", "X1"),
               weekTypes = c("numeric", rep("character", 3), rep("numeric", 6)),
               seasonTypes = c("numeric", rep("character", 2), rep("numeric", 8))
             ),
             def = list(
               name = "DEF",
               id = "td",
               pgSeq = 1,
               rowsRemove = NA,
               weekCols = c("Rank", "Player", "Team", "Opp", "defSacks", "fumRec", "defInt", "defTD", "ydsAllow", "ptsAllow", "FPTS", "X1"),
               seasonCols = c("Rank", "Player", "defSack", "fumRec", "defInt", "defTD", "ptsPGm", "ydsPGm", "FPTS", "X1"),
               weekTypes = c("numeric", rep("character", 3), rep("numeric", 8)),
               seasonTypes = c("numeric", "character", rep("numeric", 8))
             )
           )
)
#####
)