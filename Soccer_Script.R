
library(readr)
results <- read_csv("results.csv")
View(results)

results <- results

str(results)

library(tidyverse)


results$tournament <- as.factor(results$tournament)

unique(results$tournament)


#Question 1
#Create a winners column based on the result
results$winner <- ifelse(results$home_score > results$away_score, results$home_team, results$away_team)

results$winner <- ifelse(results$home_score == results$away_score, "Draw", results$winner)


wins_by_country <- results %>%
  group_by(winner) %>%
  filter(winner != "Draw") %>%
  count() %>%
  arrange(desc(n))

wins_by_country$n %>%
  summary()

#Filtered all of the countries that had greater than 60 wins (the median)
most_successful_countries <- wins_by_country %>%
  filter(n > 60.0)

most_successful_countries <- most_successful_countries %>%
  arrange(desc(n))

head(n = 5, most_successful_countries)
  #From looking at the top 5 teams with the most wins, Brazil appears to be the best team of all time followed by England, Germany, Argentina, and Sweden.




#Question 2
#Function to round to the floor of the decade
floor_decade <- function(value){ return(value - value %% 10) }

#Make a date column with only the year
results$date <-  format.Date(results$date, "%Y")
results$date <- as.numeric(results$date)

results$decade <- results$date %>%
  floor_decade()

results_by_decade <- results %>%
  group_by(decade, winner) %>%
  filter(winner != "Draw") %>%
  count()
  
results_by_decade <- arrange(results_by_decade, decade, desc(n))
view(results_by_decade)

  #At this point there are too many intervals, so breaking it into 30 year gaps is more suitable



#Creates the 30 year intervals
indx <- findInterval(results$date, seq(1870, 2018,by=30))
group <- seq(1870, 2020, by=30)
ind<-seq(1,length(group)-1, by=1)
labl1 <- paste(group[ind], group[ind+1], sep = "-")

results2 <- data.frame(results, era = labl1[indx], stringsAsFactors = T)
View(results2)

results_by_era <- results2 %>%
  group_by(era, winner) %>%
  filter(winner != "Draw") %>%
  count() %>%
  arrange(era, desc(n))

results_by_era$era <- as.factor(results_by_era$era)

View(results_by_era)

top_5_by_era <- results_by_era %>%
  group_by(era) %>%
  top_n(n = 5, wt = n)

view(top_5_by_era)


#Visualization of the top 5 teams per era

library(ggrepel)

ggplot(top_5_by_era, aes(winner, n)) +
  geom_point() +
  facet_wrap(~era) +
  geom_label_repel(aes(label = winner),
                   box.padding = 0.35,
                   point.padding = 0.5,
                   segment.color = 'black') +
  theme(axis.text.x=element_blank())
  


ggplot(top_5_by_era, aes(era, n)) +
  geom_point() +
  geom_label_repel(aes(label = winner),
                   box.padding = 0.35,
                   point.padding = 0.5,
                   segment.color = 'black')



#Based on wins alone England appears to be one of the dominant teams over the past 120 years in soccer


#However, does strictly looking at wins predict team success (World Cup Wins, European Championship Wins, etc.)? 
#I would argue for most countries that is not the case since they play against weaker countries and weaker squads in most friendlies.
#The USA could be considered one of the most successful teams between 1990 and 2018 based purely on wins, but it hasn't translated to huge success across all tournaments


no_friendlies <- results2 %>%
  filter(tournament != 'Friendly')


results2 %>%
  group_by(winner) %>%
  filter(winner != "Draw" & tournament != 'Friendly') %>%
  count() %>%
  arrange(desc(n))


top_5_no_friendlies <- no_friendlies %>%
  group_by(era, winner) %>%
  filter(winner != "Draw")  %>%
  count()
  
top_5_no_friendlies <- top_5_no_friendlies %>%
  group_by(era) %>%
  top_n(n = 5, wt = n)


ggplot(top_5_no_friendlies, aes(era, n)) +
  geom_point() +
  geom_label_repel(aes(label = winner),
                   box.padding = 0.35,
                   point.padding = 0.5,
                   segment.color = 'black')

#As a whole it's difficult to look at each tournament and say they are all the same. 
#Each tournament has different levels of difficulty due to the participants in the tournament and their respective qualifying group

#Question 3, changes throughout the era on home advantage, total goals scored, distribution of teams' strength etc


results2$home_winner <- ifelse(results2$winner == results2$home_team, "Win", "Loss")
results2$home_winner <- ifelse(results2$home_score == results$away_score, "Draw", results2$home_winner)

results2$home_winner <- as.factor(results2$home_winner)

names(results2)[13] <- "home_result"


home_team_stats <- results2 %>%
  count(home_result) %>%
  mutate(pct_time = ((n / sum(n)) * 100)) %>%
  arrange(desc(n))

view(home_team_stats)


#Across all competitions for all years the home team wins a little less than 50 percent of the time
#They either get a positive result (win) or ok result (draw) approximately 71% of the time
#If it is the home team odds are they will not outright lose

score_avg_era <- aggregate(results2[,4:5], list(results2$era), mean)
score_med_era <- aggregate(results2[,4:5], list(results2$era), median)

names(score_avg_era)[1] <- "era"
names(score_med_era)[1] <- "era"

view(score_avg_era)
view(score_med_era)

#Across all eras though scoring decreased and defenses appeared to get better
#The only time there was an increase in scoring from both sides was between 1930-1960


home_results_era <- results2 %>%
  group_by(era) %>%
  count(home_result) %>%
  mutate(pct_time = ((n / sum(n)) * 100)) %>%
  arrange(era, desc(n))

ggplot(home_results_era, aes(era, pct_time, color = home_result, shape = home_result)) +
  geom_point() +
  labs(title = "Home result through eras", x = "Era", y = "Percent of time") +
  theme(legend.title = element_blank())

#Across all eras the home team won consistently anywhere from approximately 48%-50% of the time
#The home team lost outright less through the eras and more draws occured 

#Question 4
#Can we say anything about geopolitics from football fixtures - how has the number of countries changed, which teams like to play each other


#Creates a table that shows how often the teams played
most_played <- table(apply(results2[2:3], 1, function(x) paste(sort(x), collapse = '-'))) 

most_played <- as.data.frame(most_played)

most_played <- most_played %>%
  arrange(desc(Freq))

column_names <- c("Countries", "Times_Played")

names(most_played) <- column_names

view(most_played)


results_by_era %>%
  group_by(era) %>%
  count()

#The two teams that have played the most throughout the last 120 years has been Argentina and Uruguay
#The top 10 teams that have played the most makes sense since they are all located next to each other 
#Additionally more and more teams are competing in competitions and every era sees an increase in teams
#The largest jump in winners came between the era of 1960-1990 and 1990-2018

#Question 5
#Which countries host the most matches where they themselves are not participating in?




neutral_games <- results2 %>%
  group_by(country, neutral) %>%
  count() %>%
  arrange(desc(neutral), desc(n))


#Based on every game played (regardless of tournament)  the USA hosts the most games
#and Malaysia hosts the 2nd most amount of games they don't participate in



neutral_games_tournament <- no_friendlies %>%
  group_by(country, neutral) %>%
  count() %>%
  arrange(desc(neutral), desc(n))

#When taking tournaments into account then the US hosts the 2nd most amount of games
#they're not participating in, and Malaysia then hosts the most matches


#Question 6
#Which teams are the most active in playing friendlies and friendly tournaments 
#and does it help or hurt them?

only_friendlies <- results2 %>%
  filter(tournament == 'Friendly')

only_friendlies %>%
  group_by(country) %>%
  count() %>%
  arrange(desc(n))

#The top 5 countries that host the most friendlies are the USA, France, 
#Germany, Austria, and Switzerland

friendlies_hosted <- results2[(results2$home_team == results2$country) & 
                                (results2$tournament == "Friendly"), ]


friendlies_by_era <- friendlies_hosted %>%
  group_by(era, country) %>%
  count() %>%
  arrange(era, desc(n))

most_friendlies_by_era <- friendlies_by_era %>%
  group_by(era) %>%
  top_n(n = 5, wt = n)



ggplot(most_friendlies_by_era, aes(era, n)) +
  geom_point() +
  geom_label_repel(aes(label = country),
                   box.padding = 0.35,
                   point.padding = 0.5,
                   segment.color = 'blue')

#Not surprisingly the US has hosted the majority of their friendlies over
#the past 30 years. Unfortunately, it's only lead to success in the 
#CONCACAF Gold Cup but they have been unable to win any major international
#tournaments (World Cup or COPA America).



#Contrast this with France over the last 30 years and they have achieved
#much more international success (2 World Cups and 2 European Championships). 
#The other 3 teams over the last 30 years haven't achieved much success either 
#so it doesn't appear that playing more friendlies really has any bearing on how 
#well a team performs in a tournament. 



