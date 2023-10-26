install.packages("nflverse")
library(nflverse)
library(tidyverse)
#This first section of code will be to explore the data, just to get some ideas of what to do
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

player_tackles <- tackles %>% group_by(nflId) %>% summarize(Ptackles = sum(tackle), Passists = sum(assist), PforcedFumble = sum(forcedFumble), PmissedTackles = sum(pff_missedTackle))
player_tackles['tacklePercentage'] <- ((player_tackles[c('Ptackles')]))/((player_tackles[c('Ptackles')])+(player_tackles[c('PmissedTackles')]))

player_tackles <- merge(player_tackles, players, by=c('nflId'))

week_1_tackles <- tracking_week_1 %>% filter(event == 'tackle')
