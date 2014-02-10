library(ggplot2)
library(lubridate)
library(tagStats)
library(scales)
library(plyr)
library(reshape2)
setwd("~/Documents/GA/Data_Science/NCAA")

season_results = read.csv('regular_season_results.csv', header = T)
sample = read.csv('sample_submission.csv', header = T)
seasons = read.csv('seasons.csv', header = T)
teams = read.csv('teams.csv', header = T)
tourney_results = read.csv('tourney_results.csv', header = T)
tourney_seeds = read.csv('tourney_seeds.csv', header = T)
tourney_slots = read.csv('tourney_slots.csv', header = T)

head(season_results)
head(sample)
head(seasons)
head(teams)
head(tourney_results)
head(tourney_seeds)
head(tourney_slots)

# data exploration
# unique pairings
# 356*356

length(unique(as.numeric(paste0(season_results$wteam,season_results$lteam))))

summary(data.frame(table(as.numeric(paste0(season_results$wteam,season_results$lteam))))[,2])
# frequency of games played between teams
hist(data.frame(table(as.numeric(paste0(season_results$wteam,season_results$lteam))))[,2])

#plot win loss ratio of seeds, or pts, or avg pts per game of seeds
# win rate vs certain teams
# if no previous matchup, then high seed wins
# if 1 match up, then high seed wins
# if >1 match up, then winning team wins
# in the case of a tie, higher seeded team wins (or look at scores... step 2)
# if both seeds are the same, then look at win loss ratio
# if both seed are the same and win loss ratio is the same, then look at points
# if no information, then random guess

# creating data set
matchup = list()
for(i in 1:(nrow(teams)-1)){
  for(j in (i+1):nrow(teams)){
    matchup[[paste0(teams$id[i],teams$id[j])]] = c(paste0(teams$id[i],teams$id[j]),teams$id[i],teams$id[j])
  }
}
matchup[[1]]
matchup[['501502']]
matchup = do.call('rbind',matchup)
matchup = data.frame(matchup)
names(matchup) = c('match','team1','team2')
#names(matchup) = c('team1','team2','match')
head(matchup)
matchup$match[1:5]
as.numeric(as.character(matchup$match))[1:5]
as.numeric(as.character(matchup$team1))[1:5]
as.numeric(matchup$team2)[1:5]

matchup[,1] = as.numeric(as.character(matchup[,1]))
matchup[,2] = as.numeric(as.character(matchup[,2]))
matchup[,3] = as.numeric(as.character(matchup[,3]))

# getting lowest seeding
seeding = aggregate(as.numeric(substr(tourney_seeds$seed,2,3)), by = list(tourney_seeds$team), FUN = min)
head(seeding)
names(seeding) = c('team1','seed')
matchup = merge(x = matchup, y = seeding, by = "team1", all.x=TRUE)
names(seeding) = c('team2','seed')
matchup = merge(x = matchup, y = seeding, by = "team2", all.x=TRUE)
matchup = matchup[order(matchup$team1),]
names(matchup) = c('team2','team1','match','seed1','seed2')
matchup[is.na(matchup)] = 100
head(matchup)
#matchup = merge(x = matchup, y = seeding, by = "team1", all.x=TRUE)

# season wins and losses
season_results$yay = 1
wins = aggregate(season_results$yay, by = list(season_results$wteam), FUN = sum)
head(wins)
losses = aggregate(season_results$yay, by = list(season_results$lteam), FUN = sum)
head(losses)

names(wins) = c('team1','wins')
matchup = merge(x = matchup, y = wins, by = "team1", all.x=TRUE)
names(wins) = c('team2','wins')
matchup = merge(x = matchup, y = wins, by = "team2", all.x=TRUE)

names(losses) = c('team1','losses')
matchup = merge(x = matchup, y = losses, by = "team1", all.x=TRUE)
names(losses) = c('team2','losses')
matchup = merge(x = matchup, y = losses, by = "team2", all.x=TRUE)

head(matchup)
names(matchup) = c('team2','team1','match','seed1','seed2','team1_wins','team2_wins','team1_losses','team2_losses')
matchup[is.na(matchup)] = 0

## quick seed to wins plot, or win ratio, wins/(wins+losses)
names(teams) = c('team1','name')
names(seeding) = c('team1','seed')
some_data = merge(x = teams, y = seeding, by = "team1", all.x=TRUE)
some_data[is.na(some_data)] = 17
names(wins) = c('team1','wins')
some_data = merge(x = some_data, y = wins, by = "team1", all.x=TRUE)
names(losses) = c('team1','losses')
some_data = merge(x = some_data, y = losses, by = "team1", all.x=TRUE)
some_data[is.na(some_data)] = 0
some_data$ratio = some_data$wins/some_data$losses
head(some_data)
plot(some_data$seed, some_data$ratio)
abline(lm(some_data$ratio~some_data$seed), col = 'blue')
summary(lm(some_data$ratio~some_data$seed))

some_data_avg_wins = aggregate(some_data$wins, by = list(some_data$seed), FUN = mean)
some_data_avg_losses = aggregate(some_data$losses, by = list(some_data$seed), FUN = mean)
names(some_data_avg_wins) = c('seed','wins')
names(some_data_avg_losses) = c('seed','losses')
head(some_data_avg_wins)
head(some_data_avg_losses)
some_data_avg = merge(x = some_data_avg_wins, y = some_data_avg_losses, by = "seed")

ggplot(some_data) + geom_point(aes(x = some_data$wins, y = some_data$losses, color = factor(some_data$seed)), size = 10) + theme_minimal() +
  theme(axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20),
      title = element_text(size = 20), strip.text = element_text(size = 20), plot.title = element_text(size = rel(2))) +
  theme(legend.title = element_text(size = 20, face = "bold"), legend.text = element_text(size = 20)) +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(labels = comma) +
  labs(fill = 'Seed', title = 'Wins vs Losses by Seeding', x = 'Wins', y = 'Losses', color = 'Seed')
  #scale_y_continuous(limits = c(0.05, .25)) +
  #geom_smooth(aes(x = some_data$wins, y = some_data$losses, colour = factor(some_data$seed)), se = F, method = "loess", show_guide = FALSE, lwd = .5)
  
ggplot(some_data_avg) + geom_point(aes(x = some_data_avg$wins, y = some_data_avg$losses, color = factor(some_data_avg$seed)), size = 20) + theme_minimal() +
theme(axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20),
      title = element_text(size = 20), strip.text = element_text(size = 20), plot.title = element_text(size = rel(2))) +
  theme(legend.title = element_text(size = 20, face = "bold"), legend.text = element_text(size = 20)) +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(labels = comma) +
  labs(fill = 'Seed', title = 'Average Wins vs Losses by Seeding', x = 'Wins', y = 'Losses', color = 'Seed')

## add an average seed point on the plot with another geom_point, but this time much bigger

matchup_plot = unique(matchup$seed1[matchup$seed1<17], matchup$team1_wins[matchup$seed1<17])

ggplot(matchup) + geom_point(aes(x = matchup$seed1[matchup$seed1<17], y = matchup$team1_wins[matchup$seed1<17]))

## quick seed to losses plot

## maybe another team# to wins plot

# records
season_results$match = 0
for(i in 1:nrow(season_results)){
  season_results$match[i] = paste0(min(season_results$wteam[i],season_results$lteam[i]),max(season_results$wteam[i],season_results$lteam[i]))
}


season_results$match = as.numeric(as.character(season_results$match))

# save current R data

season_results$match = as.numeric(as.character(paste0(min(season_results$wteam,season_results$lteam),max(season_results$wteam,season_results$lteam))))

head(season_results)
class(matchup$team1)

head(tourney_seeds)
tourney_seeds


as.numeric(substr(tourney_seeds$seed,2,3)[1])