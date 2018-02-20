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

View(df[, c("att_team", "winner_team")]) # att_team is team that dealt the attack
table(df[, c("winner_team")]) # winner_team is winning team 

# file is unique to each match - but very cumbersome - create new id
head(df$file)
df$file_id <- as.numeric((df$file)) #can use interaction for more than 1 group

#number of matches
max(df$file_id) #1297

#number of players
NROW(unique(c(df$att_id, df$vic_id))) #11,130 unique players

#same winning team within a match
winners_temp <- tapply(df$file, INDEX = list(df$file), FUN = function (x) {
                                                                x <- as.character(x)
                                                                as.numeric(as.factor(x))
                                                        })
df$winner_numero <- unlist(winners_temp, use.names = FALSE)
rm(winners_temp)
table(df$winner_numero) #yes,always same winner
df$winner_numero <- NULL

#############################################################################
#
#  Map analysis
#
#############################################################################

table(df$map)

# count number of plays within each match
match_id <- tapply(df$file_id, list(df$file_id), FUN = function (x) {  
  num_rows = NROW(x)
  return(1:num_rows)
})
df$match_id <- unlist(match_id, use.names = FALSE)

# most popular map
ggplot(data = df[df$match_id == 1, ]) + geom_bar(stat="count", aes(map)) + coord_flip() 

# which maps benefit one side more than the other?
match_outcome <- df[df$match_id == 1, c("winner_side", "map", "winner_team", "file_id")]

ggplot(data = match_outcome, aes(map,..count..)) + 
      geom_bar(aes(fill = winner_side), position = "Dodge") +
      coord_flip()
  
#############################################################################
#
#  Team analysis
#
#############################################################################

# fraction of wins by team

#need losses too

match_outcome$wins <- 1 
match_wins <- aggregate(match_outcome$wins, by = list(match_outcome$winner_team), FUN = sum)

