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


## Age and Education Demographics
install.packages("usmap")
library(usmap)
library(ggplot2)
library(tidyverse)

age <- read_csv("state_age.csv")
educ <- read_csv("state_education_data.csv")

names(age) <- c('state', 'weighted_age')
names(educ) <- c('state', 'high_school_rate', 'bachelors_rate', 'advanced_rate')
educ['high_school_rate']=educ['high_school_rate']*100
educ['bachelors_rate']=educ['bachelors_rate']*100
educ['advanced_rate']=educ['advanced_rate']*100

plot_usmap(data = age, values = 'weighted_age') +
  labs(title = "Weighted Average Age by State")+
  scale_fill_continuous(low = 'white', high = 'turquoise4',name = 'Average Age') +
  theme(legend.position = 'right')

plot_usmap(data = educ, values = 'high_school_rate') +
  labs(title = "Percent of Residents who Obtained High School Degree")+
  scale_fill_continuous(low = 'white', high = 'orange',name = 'Percent') +
  theme(legend.position = 'right')

plot_usmap(data = educ, values = 'bachelors_rate') +
  labs(title = "Percent of Residents who Obtained Bachelors Degree")+
  scale_fill_continuous(low = 'white', high = 'purple3',name = 'Percent') +
  theme(legend.position = 'right')

plot_usmap(data = educ, values = 'advanced_rate') +
  labs(title = "Percent of Residents who Obtained Advanced Degree")+
  scale_fill_continuous(low = 'lightblue', high = 'black',name = 'Percent') +
  theme(legend.position = 'right')

