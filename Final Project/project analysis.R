setwd("/Users/alexandraholland/ssc442_team6/project")
library(usmap)
library(ggplot2)

##RELIGION DEMOGRAPHICS

religion <- read.csv(file = 'religion.csv')
#https://www.census.gov/library/publications/2011/compendia/statab/131ed/population.html
head(religion)
names(religion)[1] <- "state"
names(religion)[2] <- "christian_adherence"
names(religion)[3] <- "pop_percent_ch"
names(religion)[4] <- "jewish_adherence"
names(religion)[5] <- "pop_percent_j"

state <- religion['state']
christian <- religion['christian_adherence']
pop_percent_ch <- religion['pop_percent_ch']

plot_usmap(data = religion, values = 'pop_percent_ch') +
  labs(title = "Percent of State Population that Identifies as Christian")+
  scale_fill_continuous(low = 'white', high = 'dark green',name = 'Percentage') +
  theme(legend.position = 'right')
  

##RACE DEMOGRAPHICS

#https://www.kff.org/other/state-indicator/distribution-by-raceethnicity/?currentTimeframe=0&sortModel=%7B%22colId%22:%22Location%22,%22sort%22:%22asc%22%7D
race <- read.csv(file = 'races.csv')

names(race)[1] <- "state"
names(race)[2] <- "white"
names(race)[3] <- "black"
names(race)[4] <- "hispanic"
names(race)[5] <- "native"
names(race)[6] <- "asian"
names(race)[7] <- "islander"
names(race)[8] <-"two_plus"

plot_usmap(data = race, values = 'white') +
  labs(title = "White Proportion of State Population")+
  scale_fill_continuous(low = 'white', high = 'black',name = 'Percentage') +
  theme(legend.position = 'right')

plot_usmap(data = race, values = 'black') +
  labs(title = "Black Proporrtion of State Population")+
  scale_fill_continuous(low = 'white', high = 'black',name = 'Percentage') +
  theme(legend.position = 'right')

plot_usmap(data = race, values = 'hispanic') +
  labs(title = "Hispanic Proportion of State Population")+
  scale_fill_continuous(low = 'white', high = 'black',name = 'Percentage') +
  theme(legend.position = 'right')
