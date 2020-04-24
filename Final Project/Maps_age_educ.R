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
