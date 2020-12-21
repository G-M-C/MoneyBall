library(dplyr)
library(ggplot2)

batting <- read.csv('Batting.csv')
print(head(batting))

batting$BA <- batting$H/batting$AB
batting$OBP <- (batting$H + batting$BB + batting$HBP)/(batting$AB + batting$BB + batting$HBP + batting$SF)
batting$X1B <- batting$H - batting$X2B - batting$X3B - batting$HR
batting$SLG <- ((1 * batting$X1B) + (2 * batting$X2B) + (3 * batting$X3B) + (4 * batting$HR) ) / batting$AB
str(batting)

sal <- read.csv('Salaries.csv')
batting <- subset(batting,yearID >= 1985)
print(summary(batting))

sal_batting <- merge(batting,sal,by=c('playerID','yearID'))
print(summary(sal_batting))


lost_players <- subset(sal_batting,playerID %in% c('giambja01','damonjo01','saenzol01') )
lost_players <- subset(lost_players,yearID == 2001)
lost_players <- lost_players[,c('playerID','H','X2B','X3B','HR','OBP','SLG','BA','AB')]
print(head(lost_players))

avail.players <- filter(sal_batting,yearID==2001)

plt <- ggplot(avail.players,aes(x=OBP,y=salary)) + geom_point()
print(plt)

avail.players <- filter(avail.players,salary<8000000,OBP>0)
avail.players <- filter(avail.players,AB >= 500)

possible <- head(arrange(avail.players,desc(OBP)),10)
possible <- possible[,c('playerID','OBP','AB','salary')]
print(possible[2:4,])




