##############################################################################
#
#
# Description: Analyse counterstrike data
# Project: Introduction to R
#
#
##############################################################################

### set paths
datapath <- "C:/Users/Amanda.Teo/Desktop/Coding for beginners/Data/"

library(ggplot2)


### import data
df <- read.csv(file = file.path(datapath, "mm_master_demos.csv"), header = TRUE)
str(df)


#############################################################################
#
# Rudimentary analysis
#
#############################################################################

# How to view subsets of data
View(df[, c("att_team", "winner_team")]) 

# How to view counts of of a certain variable
table(df[, c("winner_team")]) # winner_team is winning team 

# file is unique to each match - but very cumbersome - create new id
head(df$file)
df$file_id <- as.numeric((df$file)) #can use interaction for more than 1 group

#number of matches
max(df$file_id) #1297

#average number of rounds within a match
num_rounds <- aggregate(round ~ file_id, df, FUN = max)
nrow(num_rounds)
mean(num_rounds$round) #26 rounds per match

#total number of games(rounds) played
sum(num_rounds$round) #32752

#number of teams
NROW(unique(c(df$att_team, df$vic_team))) #27


#number of players
NROW(unique(c(df$att_id, df$vic_id))) #11,130 unique players

#same winning team within a round of a match
winners_temp <- aggregate(winner_team ~ file_id + round, df,FUN = function (x) {
                                                                length(unique(x))
                                                        })
winners_temp[winners_temp$winner_team > 1, ] #check how many rounds per match have more than 1 winner
rm(winners_temp)

#most popular weapons
df$counter <- 1

weapons <- df[, c("hitbox", "wp_type", "counter")]
weapons <- aggregate(counter ~ hitbox + wp_type, weapons, FUN = sum)
weapons <- within(weapons, {total_use_wptype = ave(counter, wp_type, FUN = sum)})
weapons <- with(weapons, weapons[order(wp_type, hitbox),])
weapons$perc_freq <- with(weapons, counter/total_use_wptype) * 100

weapons_table <- reshape(weapons, idvar = c("wp_type"), drop = c("counter", "total_use_wptype"), timevar = c("hitbox"), direction = "wide")
#seems that likelihood of body part hit not really dependent on the weapon (which makes sense)

#top 15 players, the number of attacks, favourite weapon

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

ggplot(data = game_winners, aes(map,..count..)) + 
      geom_bar(aes(fill = winner_side), position = "Dodge") +
      coord_flip()
  
#############################################################################
#
#  Team analysis
#
#############################################################################

# fraction of wins by team

#how to identify who lost?


