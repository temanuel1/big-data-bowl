install.packages("nflverse")
library(nflverse)
library(tidyverse)
#This first section of code will be to explore the data, just to get some ideas of what to do
#make sure your directory call is right
games <- read.csv('NFL/games.csv')
players <- read.csv('NFL/players.csv')
plays <- read.csv('NFL/plays.csv')
tackles <- read.csv('NFL/tackles.csv')
tracking_week_1 <- read.csv('NFL/tracking_week_1.csv')
tracking_week_2 <- read.csv('NFL/tracking_week_2.csv')
tracking_week_3 <- read.csv('NFL/tracking_week_3.csv')
tracking_week_4 <- read.csv('NFL/tracking_week_4.csv')
tracking_week_5 <- read.csv('NFL/tracking_week_5.csv')
tracking_week_6 <- read.csv('NFL/tracking_week_6.csv')
tracking_week_7 <- read.csv('NFL/tracking_week_7.csv')
tracking_week_8 <- read.csv('NFL/tracking_week_8.csv')
tracking_week_9 <- read.csv('NFL/tracking_week_9.csv')

#created a player data frame with tackling statistics
player_tackles <- tackles %>% group_by(nflId) %>% summarize(Ptackles = sum(tackle), Passists = sum(assist), PforcedFumble = sum(forcedFumble), PmissedTackles = sum(pff_missedTackle))
player_tackles['tacklePercentage'] <- ((player_tackles[c('Ptackles')]))/((player_tackles[c('Ptackles')])+(player_tackles[c('PmissedTackles')]))

player_tackles <- merge(player_tackles, players, by=c('nflId'))

#created data frame that takes the tracking data at the instance of a tackle
week_1_tackles <- tracking_week_1 %>% filter(event == 'tackle')


#created a data frame that merges the play by play data with the instance of a tackle in week 1
tackles_pbp_1 <- merge(week_1_tackles, plays, by=c('playId', 'gameId'))

#I don't think we need these columns, there are probably more we can drop. The less fat the better
clean_tackles_pbp_1 <- subset(tackles_pbp_1, select = -c(yardlineNumber, foulName1, foulName2, foulNFLId1, foulNFLId2, jerseyNumber))

#remove some now unneeded tables to clean up global environment
rm(tracking_week_1, week_1_tackles)

#can repeat this process for every week to make a clean version of it for every week

# this is the code for the table branch
