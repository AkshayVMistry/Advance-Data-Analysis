#######################

rm(list=ls())
library(magrittr)
library(dplyr)
library(tidyverse)
library(tidyr)


Q1 <- read.csv("IPL Ball-by-Ball 2008-2020.csv")
Q2 <- read.csv("IPL Matches 2008-2020.csv")

head(Q1, 10)
head(Q2, 10)

summary(Q1)
summary(Q2)

missing_Q1 <- colSums(is.na(Q1))
missing_Q2 <- colSums(is.na(Q2))

missing_Q1
missing_Q2


#Players who have played or bowled 300 balls should be considered.
#make sure that your squad has strong openers, middle-order hitters, all-rounders, a
#reliable wicket-keeper, and a collection of quick and spin bowlers.

##Match winning Performances by Batters and Bowlers
bowler_run <- Q1 %>%
  select(id, bowler, batsman_runs, ball, is_wicket) %>%
  group_by(id, bowler) %>%
  summarise(runs = sum(batsman_runs), balls = n(), wickets = sum(is_wicket)) %>%
  mutate(economy = round(runs/(balls/6), 2))  %>%
  group_by(bowler) %>%
  summarise(runs = sum(runs), balls = sum(balls), total_wickets = sum(wickets), 
            min_economy = round(min(economy),2), avg_economy = round(mean(economy),2)) %>%
  arrange(desc(total_wickets))
  

summary(bowler_run)

#An average matchwinning performance is to take 2-3 wickets with an economy of 5 
#runs per over it's a match winning performance

options(repr.plot.width = 15, repr.plot.height = 8)
ggplot(data = bowler_run, aes(x = total_wickets, avg_economy)) +
  geom_point(aes(color = "pink")) +
  labs(title = "Wickets vs Economy",x = "Wickets", y = "Economy") +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        panel.background = element_rect(fill = "darkorchid4"),
        panel.grid = element_blank(),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 15)
  ) +
  scale_color_identity(guide = "none")

batter_run <- Q1 %>%
  select(id,batsman, ball, batsman_runs) %>%
  group_by(id,batsman) %>%
  summarise(runs = sum(batsman_runs), balls = n() ) %>%
  mutate(strike_rate = round((runs/balls)*100, 2), match_played = length(unique(id))) %>%
  group_by(batsman) %>%
  summarise(runs = sum(runs), balls = sum(balls), total_matches = sum(match_played), 
            max_strike_rate = round(max(strike_rate),2), 
            avg_strike_rate = round(mean(strike_rate),2)) %>%
  arrange(desc(runs))  %>%
  top_n(50,runs)

summary(batter_run)

#An average matchwinning batting performance is to make 69 runs with the strike rate of 170

ggplot(data = batter_run, aes(x = runs, y = avg_strike_rate)) +
  geom_point(aes(color = "orange")) +
  labs(title = "Matchwinning Batting Performances", x = "Runs", y = "Strike Rate") +
  theme(plot.title = element_text(hjust=0.5, size = 20,),
        panel.grid = element_blank(),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 15),
        panel.background =  element_rect(fill = "darkorchid4")) +
  scale_color_identity(guide = "none")


#Most Boundaries Hit by Players
boundary_hitters <- Q1 %>%
  filter(Q1$batsman_runs == 4 | Q1$batsman_runs == 6) %>%
  group_by(batsman) %>%
  summarise(Four = sum(batsman_runs == 4), Six = sum(batsman_runs == 6)) %>%
  arrange(desc(Four + Six))

top_10_hitters <- boundary_hitters %>%
  top_n(10,Four + Six) %>%
  gather(key = "boundary", value = "count", 2:3)

ggplot(top_10_hitters, aes(x = batsman, y = count, fill = boundary)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 10 Boundary Hitters", x = "Batter", y = "Boundaries",colours(distinct = FALSE)) +
  theme(plot.title = element_text(hjust = 0.5, size = 20), panel.background = element_rect(fill = "orchid4"),
        panel.grid = element_blank(), axis.title = element_text(size = 20), axis.text = element_text(size = 15),
        legend.title = element_text(size = 15), legend.text = element_text(size = 15,)) +
  geom_text(aes(label = count), position = position_stack(vjust = 0.5), size = 5) + 
  scale_fill_manual(values=c('pink', 'skyblue'))


#Most Players hit way more fours than sixes


#Best Batmans by run
runscorers <- Q1 %>%
  group_by(batsman) %>%
  summarise(runs = sum(batsman_runs), balls = n(), innings = sum(is_wicket == 1),
            fours = sum(batsman_runs == 4), sixes = sum(batsman_runs == 6))%>%
  mutate(boundary_pct = round(((4*fours + 6*sixes)/runs)*100,2),
         strike_rate = round((runs/balls)*100,2), avg = round(runs/innings, 2)) %>%
  subset(select = -c(balls, fours, sixes, innings)) %>%
  arrange(desc(runs)) 

summary(runscorers)

#A player with an average of 30 and strike rate of 127 can be considered good IPL batsman

top_10_runscorers <- runscorers%>%
  top_n(10, runs) %>%
  subset(select = -c(runs)) %>%
  gather(key = "stats", value = "val", 2:4) %>%
  ggplot(aes(x = batsman, y = val, fill = stats)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 10 Run Scorers", x = "Name", y = "Value") +
  theme(plot.title = element_text(hjust = 0.5, size = 20), panel.background = element_rect(fill = "orchid4"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title = element_text(size = 20), axis.text = element_text(size = 15),
        legend.title = element_text(size = 15), legend.text = element_text(size = 15),
        strip.text = element_text(size = 20)) +
  facet_wrap(~stats, ncol = 1, 
             labeller = as_labeller(c("avg" = "Average", "boundary_pct" = "Boundary Percent", "strike_rate" = "Strike Rate"))) +
  geom_text(aes(label = val), vjust = 2, size = 5) +
  scale_fill_hue(h = c(180, 300), labels = c("Average", "Boundary Percent", "Strike Rate"))+ 
  scale_fill_manual(values=c('pink', 'skyblue','orange'))

top_10_runscorers

#Best batters by Strike Rate

top_10_strikerates <- runscorers %>%
  arrange(desc(strike_rate)) %>%
  subset(select = -c(runs)) %>%
  top_n(10, strike_rate)

summary(top_10_strikerates)

g1 <- gather(top_10_strikerates, key = "stats", value = "val", 2:3) %>%
  ggplot(aes(x = batsman, y = val, fill = stats)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 10 Strike Rates", x = "Batsman", y = "Value", fill = "Stats") +
  theme(plot.title = element_text(hjust = 0.5, size = 20), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), panel.background = element_rect(fill = "orchid4"),
        axis.title = element_text(size = 20), axis.text = element_text(size = 15),
        legend.title = element_text(size = 15), legend.text = element_text(size = 15),
        strip.text = element_text(size = 20)) +
  scale_fill_manual(values = c("orange", "mediumaquamarine"), labels = c("Boundary Percent", "Strike Rate")) +
  geom_text(aes(label = val), vjust = 2, size = 5) +
  facet_wrap(~stats, ncol = 1, labeller = as_labeller(c("boundary_pct" = "Boundary Percent", "strike_rate" = "Strike Rate")))

g1

#Most explosives batsman have strike rate around 150

#Best bowler by Wickets
top_10_wickettakers <- Q1 %>%
  group_by(bowler) %>%
  summarise(wickets = sum(is_wicket), runs = sum(batsman_runs), 
            boundaries = sum(batsman_runs == 4|batsman_runs == 6), balls = n()) %>%
  mutate(economy_rate = round(runs/(balls/6),2), boundary_pct = round((boundaries/balls)*100,2)) %>%
  subset(select = -c(runs, balls, boundaries)) %>%
  filter(wickets > 50) %>%
  arrange(desc(wickets)) %>%
  top_n(10, wickets) 

summary(top_10_wickettakers)

g2 <- gather(top_10_wickettakers, key = "features", value = "val", 2:4) %>%
  ggplot(aes(x = bowler, y = val, fill = features)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 10 Wicket Takers", x = "Bowler", y = "Values", fill = "Stats") +
  theme(plot.title = element_text(hjust = 0.5, size = 20), panel.grid = element_blank(),
        panel.background = element_rect(fill = "orchid4"), axis.title = element_text(size = 15),
        axis.text = element_text(size = 13), legend.title = element_text(size = 15),
        legend.text = element_text(size = 15), strip.text = element_text(size = 20)) +
  scale_fill_manual(values = c("olivedrab1", "orange2", "powderblue"), labels = c("Boundary Percent", "Economy", "Wickets")) +
  geom_text(aes(label = val), vjust = -0.15, size = 5) +
  facet_wrap(~features, ncol = 1, labeller = as_labeller(c("boundary_pct" = "Boundary Percent","economy_rate" = "Economy", "wickets" = "Wickets")))

g2

#Player of the Match
player_of_match_count <- Q2 %>% 
  group_by(player_of_match) %>%
  summarise(awards = n()) %>%
  filter(awards >= 10) %>%
  arrange(desc(awards))

player_of_match_count %>%
  ggplot(aes(x = reorder(player_of_match, +awards), y=(awards), fill=player_of_match)) + geom_col(position="dodge") +
  labs(x="Player_of_match", y = "Awards" , title = "Top 10 Player Man of the Match") + coord_flip() 


#Dismissal Player
dismissal_player <- Q1 %>%
  group_by(player_dismissed, dismissal_kind) %>%
  summarise(count_dismissal = n()) %>%
  arrange(desc(count_dismissal)) %>%
  filter(dismissal_kind != 'NA')

#Dismissal Fielder
dismissal_fielder <- Q1 %>%
  group_by(fielder, dismissal_kind) %>%
  summarise(count_dismissal = n()) %>%
  arrange(desc(count_dismissal)) %>%
  filter(dismissal_kind != 'NA', fielder != 'NA', dismissal_kind=='stumped')

#Extras Type
extras_type_bowler <- Q1 %>% 
  select(is_wicket, bowler, extras_type) %>%
  group_by(bowler, extras_type) %>%
  summarise(count_extras_type = n(),count_wicket = sum(is_wicket))  %>%
  arrange(desc(count_extras_type)) %>%
  filter(extras_type != 'NA')

#Maiden Overs
maiden_over <- Q1 %>%
  select(id, bowler,over,total_runs) %>%
  group_by(id, bowler,over) %>%
  summarise(over_run = sum(total_runs)) %>%
  filter(over_run == 0) %>%
  group_by(bowler) %>%
  summarise(maiden_over = n()) %>%
  arrange(desc(maiden_over))

#Matches played
match_played <- Q1 %>%
  select(id, batsman) %>%
  group_by(batsman) %>%
  mutate(match_cnt = n()) %>%
  arrange(desc(match_cnt)) %>%
  subset(select = -c(id)) %>%
  unique()

