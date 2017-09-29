library(dplyr)
library(zoo)
library(sqldf)

gamelogs <- select(gamelogs, -FG_PCT,-PLAYER_ID, -TEAM_ID, -GAME_ID, -FGA_PCT, -FG3_PCT, -WL)
gamelogs <- select(gamelogs, -GAME_ID, -WL, -FG_PCT, -FG3_PCT, -FT_PCT, -PF)
gamelogs <- select(gamelogs, -OREB, -DREB, -PLUS_MINUS, -VIDEO_AVAILABLE)
gamelogs <- select(gamelogs, -PLAYER_ID, -TEAM_ID)


#Fandual calc
gamelogs$FD <- 2*gamelogs$FGM+gamelogs$FG3M+gamelogs$FTM+1.2*gamelogs$REB+1.5*gamelogs$AST+2*gamelogs$BLK+2*gamelogs$STL-gamelogs$TOV

#add column and add if game was home or away
gamelogs$home_away <- ifelse(grepl(" @ ", gamelogs$MATCHUP), 'A', 'H')

#Get Oppt
gamelogs$Left <- substr(gamelogs$MATCHUP, 1,3)
gamelogs$Right <- substr(gamelogs$MATCHUP,7, 12)

gamelogs$Left <- NULL
gamelogs$OPPT <- gsub("\\.", "", gamelogs$Right)
gamelogs$Right <- NULL

#Converting columns to factors
gamelogs$PLAYER_NAME <- as.factor(gamelogs$PLAYER_NAME)
gamelogs$TEAM_ABBREVIATION <- as.factor(gamelogs$TEAM_ABBREVIATION)
gamelogs$TEAM_NAME <- as.factor(gamelogs$TEAM_NAME)
gamelogs$home_away <- as.factor(gamelogs$home_away)
gamelogs$OPPT <- as.factor(gamelogs$OPPT)

#order player by game date
gamelogs <- gamelogs[order(gamelogs$PLAYER_NAME, gamelogs$GAME_DATE),]

#Get the moving average of the past 3 games
avg.last.3 <- function (x) if (length(x) < 3) rep(NA, length(x)) else rollmeanr(x, 3, fill = NA)

res <- gamelogs %>% group_by(PLAYER_NAME) %>% arrange(GAME_DATE) %>%
  mutate(Avg3.Pts=avg.last.3(FD)) %>%
  ungroup() %>% arrange(PLAYER_NAME,GAME_DATE)
gamelogs <- res[,c(colnames(res)[1:21], "Avg3.Pts")]

gamelogs$over_proj <- ifelse(gamelogs$FD < gamelogs$Avg3.Pts, "Over", "Under")   

gamelogs$Projected <- gamelogs$Avg3.Pts
gamelogs$Avg3.Pts <- NULL
gamelogs$Projected <-format(round(gamelogs$Projected, 2), nsmall = 2)

#ifelse statement to feel in column with bit
#gamelogs$NewPoints <- gamelogs[is.na(gamelogs$Projected)]
#gamelogs$NewPoints <- ifelse(is.na(gamelogs$Projected)==0,gamelogs$FD,gamelogs$Projected)
#gamelogs$NewPoints <- NULL


gamelogs$PTS_DIFF <- (gamelogs$Projected-gamelogs$FD)
gamelogs$PTS_DIFF <-format(round(gamelogs$PTS_DIFF, 2), nsmall = 2)

gamelogs$over_proj <- as.factor(gamelogs$over_proj)
gamelogs$Projected <- as.numeric(gamelogs$Projected)
gamelogs$PTS_DIFF <- as.numeric(gamelogs$PTS_DIFF)

str(gamelogs)

team_name_cleanup <- sqldf('select distinct
                            case when tm = "CHO" THEN "CHA"
                            ELSE tm end as Tm
                           from positions order by Tm')


df <- sqldf('select b.Pos, a.* 
            from gamelogs a 
            left join positions b on a.PLAYER_NAME = B.Player and a.TEAM_ABBREVIATION = b.Tm')

null_pos <- sqldf('select distinct b.Pos, a.PLAYER_NAME, A.TEAM_ABBREVIATION, b.Tm 
                  from gamelogs a 
                  left join positions b on a.PLAYER_NAME = B.Player and a.TEAM_ABBREVIATION = b.Tm
                  where b.Pos is null')



head(gamelogs)
#write.csv(gamelogs, file = "C:/Users/williarl/Documents/R Training/Gamelogs/updatedgames.csv", fileEncoding = "UTF-16LE")
