##############################################################################
#
#
# Description: Analyse counterstrike data
# Project: R training
#
#
##############################################################################

### set paths
datapath <- "C:/Amanda.Teo/Desktop/Coding for beginners/Data"

library(ggplot2)


### import data
df <- read.csv(file = file.path(datapath, "mm_master_demos.csv"), header = TRUE)
str(df)


#############################################################################
#
# Rudimentary stats
#
#############################################################################

# How to view subsets of data
View(df[, c("file", "round", "att_team", "winner_team")]) 

# How to view counts of of a certain variable
table(df$winner_team) # winner_team is winning team 

# file is unique to each match - but very cumbersome - create new id
head(df$file)
df$file_id <- as.numeric((df$file)) #can use interaction for more than 1 group

#number of matches
max(df$file_id) #1297

#number of teams
NROW(unique(c(df$att_team, df$vic_team))) #27

#number of players
NROW(unique(c(df$att_id, df$vic_id))) #11,130 unique players

#average number of rounds within a match
num_rounds <- aggregate(round ~ file_id, df, FUN = max)
nrow(num_rounds)
mean(num_rounds$round) #26 rounds per match

#distribution of rounds per match
summary(num_rounds$round)

#total number of games(rounds) played
sum(num_rounds$round) #32752

#############################################################################
#
# Data cleaning
#
#############################################################################

#any missing values
num_miss <- lapply(df,FUN = function(x) {
  length(which(is.na(x)))
})

#translate in game coordinates into positives
df[,grep("_pos_[xy]", colnames(df))] <- sapply(df[,grep("_pos_[xy]", colnames(df))], FUN = function(x) {
  x + 10000  
})

#same winning team within a round of a match
winners_temp <- aggregate(winner_team ~ file_id + round, df,FUN = function (x) {
  length(unique(x))
})
winners_temp[winners_temp$winner_team > 1, ] #check how many rounds per match have more than 1 winner
rm(winners_temp)

#############################################################################
#
# More intricate summary statistics
#
#############################################################################

#within each weapon type, percentage of hits on different parts of the body

df$counter <- 1

weapons <- df[, c("hitbox", "wp_type", "counter")]
weapons <- aggregate(counter ~ hitbox + wp_type, weapons, FUN = sum)
weapons <- within(weapons, {total_use_wptype = ave(counter, wp_type, FUN = sum)})
weapons <- with(weapons, weapons[order(wp_type, hitbox),])
weapons$perc_freq <- with(weapons, counter/total_use_wptype) * 100

weapons_table <- reshape(weapons, idvar = c("wp_type"), drop = c("counter", "total_use_wptype"), timevar = c("hitbox"), direction = "wide")
View(weapons_table)#seems that likelihood of body part hit not really dependent on the weapon (which makes sense)
df$counter <- NULL

#top 15 players by average damge per attack, the number of attacks, favourite weapon

indiv_damage <- data.frame(
  aggregate(hp_dmg ~ att_id, df,FUN = sum),
  aggregate(wp ~ att_id, df, FUN = function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }),
  aggregate(X ~ att_id, df,FUN = length)
)
indiv_damage[, grep("att_id.[0-9]", colnames(indiv_damage))] <- list(NULL)
indiv_damage <- indiv_damage[!indiv_damage$att_id == 0,]

indiv_damage$avg_dmg <- with(indiv_damage, hp_dmg/X)
indiv_damage <- indiv_damage[order(-indiv_damage$avg_dmg),]
top15_damage <- indiv_damage[1:15,]

#distribution of average damage
ggplot(data = indiv_damage, aes(x = avg_dmg)) + geom_histogram(binwidth = 5)

indiv_damage$top_15 <- indiv_damage$att_id %in% top15_damage$att_id
ggplot(data = indiv_damage, aes(x = avg_dmg)) + geom_histogram(binwidth = 5, aes(fill = top_15))

#############################################################################
#
#  Map analysis
#
#############################################################################

table(df$map)

# count number of rounds
df <- within(df, {game_id = ave(df$X, list(df$file_id, df$round), FUN = function(x) {
  num_rows = NROW(x)
  return(1:num_rows)
})})


# most popular map
ggplot(data = df[df$game_id == 1, ]) + geom_bar(stat="count", aes(map)) + coord_flip()

# which maps benefit one side more than the other?
game_winners <- df[df$game_id == 1, c("winner_side", "map")]

ggplot(data = game_winners, aes(map)) + 
  geom_bar(stat = "count", aes(fill = winner_side), position = "Dodge") +
  coord_flip()