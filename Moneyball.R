########
### Moneyball Project ##########
############


###############################
## The 2002 Oakland A's ##

## The Oakland Athletics' 2002 season was the team's 35th in Oakland, California. 
## It was also the 102nd season in franchise history. The Athletics finished first in the American League West with a record of 103-59.
## The Athletics' 2002 campaign ranks among the most famous in franchise history. Following the 2001 season, Oakland saw the departure of three key players (the lost boys). Billy Beane, the team's general manager, responded with a series of under-the-radar free agent signings. The new-look Athletics, despite a comparative lack of star power, surprised the baseball world by besting the 2001 team's regular season record. 
## The team is most famous, however, for winning 20 consecutive games between August 13 and September 4, 2002.[1] The Athletics' season was the subject of Michael Lewis' 2003 book Moneyball: The Art of Winning an Unfair Game (as Lewis was given the opportunity to follow the team around throughout that season)

## This project is based off the book written by Michael Lewis (later turned into a movie).


#####################################



batting <- read.csv('Batting.csv')

head(batting)

str(batting)

head(batting$AB)
head(batting$X2B)



############################################################################
## Feature Engineering ##

## We need to add three more statistics that were used in Moneyball! These are: ##
  
##  Batting Average
##  On Base Percentage
##  Slugging Percentage

#############################################################################

batting$BA <- batting$H / batting$AB
tail(batting$BA,5)


# On Base Percentage
batting$OBP <- (batting$H + batting$BB + batting$HBP)/(batting$AB + batting$BB + batting$HBP + batting$SF)

# Creating X1B (Singles)
batting$X1B <- batting$H - batting$X2B - batting$X3B - batting$HR

# Creating Slugging Average (SLG)
batting$SLG <- ((1 * batting$X1B) + (2 * batting$X2B) + (3 * batting$X3B) + (4 * batting$HR) ) / batting$AB


str(batting)



#####################################################################################
## Merging Salary Data with Batting Data ##

## We know we don't just want the best players, we want the most undervalued players, meaning we will also need to know current salary information!##

######################################################################################


sal <- read.csv('Salaries.csv')

summary(batting)

batting <- subset(batting,yearID >= 1985)

summary(batting)

####################################################################################
## Now it is time to merge the batting data with the salary data! Since we have players playing multiple years, we'll have repetitions of playerIDs for multiple years, meaning we want to merge on both players and years.

#####################################################################################


combo <- merge(batting,sal,by=c('playerID','yearID'))

summary(combo)

######################################################################################
## Analyzing the Lost Players

## As previously mentioned, the Oakland A's lost 3 key players during the off-season. 
## We'll want to get their stats to see what we have to replace. 
## The players lost were: first baseman 2000 AL MVP Jason Giambi (giambja01) to the New York Yankees, outfielder Johnny Damon (damonjo01) to the Boston Red Sox and infielder Rainer Gustavo "Ray" Olmedo ('saenzol01').

########################################################################################


lost_players <- subset(combo,playerID %in% c('giambja01','damonjo01','saenzol01') )

lost_players

lost_players <- subset(lost_players,yearID == 2001)

lost_players <- lost_players[,c('playerID','H','X2B','X3B','HR','OBP','SLG','BA','AB')]
head(lost_players)

########################################################################################
## Replacement Players

## Now we have all the information we need to Find Replacement Players for the key three players we lost
## Using  
##  The total combined salary of the three players can not exceed 15 million dollars.
##  Their combined number of At Bats (AB) needs to be equal to or greater than the lost players.
##  Their mean OBP had to equal to or greater than the mean OBP of the lost players

########################################################################################

library(dplyr)
avail.players <- filter(combo,yearID==2001)

library(ggplot2)
ggplot(avail.players,aes(x=OBP,y=salary)) + geom_point()

avail.players <- filter(avail.players,salary<8000000,OBP>0)
avail.players <- filter(avail.players,AB >= 500)

possible <- head(arrange(avail.players,desc(OBP)),10)

possible <- possible[,c('playerID','OBP','AB','salary')]
possible

possible[2:4,]


########################################################################################
## Great, looks like I just saved the 2001 Oakland A's a lot of money! If only I had a time machine and R, I could have made a lot of money in 2001 picking players! ##
#########################################################################################
